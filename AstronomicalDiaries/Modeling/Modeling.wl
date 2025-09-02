BeginPackage["AstronomicalDiaries`Modeling`"];

fitModel

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Modeling`Utilities`"]
Needs["AstronomicalDiaries`Modeling`ConditionalSampling`"]
Needs["AstronomicalDiaries`Modeling`TrueModel`"]
Needs["AstronomicalDiaries`Astronomy`"]


modelObservationQ[observations_] :=
	MemberQ[observations, _?(
			! MissingQ[#Object] &&
			! MissingQ[#Reference] &&
			! MissingQ[#EarliestDate] &&
			! MissingQ[#LatestDate] &&
			! MissingQ[#Time] &&
			! MissingQ[#Relation] &&
			! (MissingQ[#Cubits] && MissingQ[#Fingers])&
		)
	]


(* Update rules *)

(* l *)
lPrior[] := NormalDistribution[1/2, 1/2]
lInit[] := RandomVariate@lPrior[]
lUpdate[c_, deltaStar_, sigma2_, inliers_] :=
	normalNormalRegressionSample[lPrior[], Sqrt[sigma2], deltaStar[[inliers]], 0, c[[inliers]]];


(* sigma2 *)
sigma2Prior[] := InverseGammaDistribution[1/2, 1/2]
sigma2Init[] := RandomVariate@sigma2Prior[]
sigma2Update[c_, deltaStar_, l_, inliers_] :=
	normalNormalVarianceSample[sigma2Prior[], deltaStar[[inliers]]*l, c[[inliers]]];


(* muOutlier *)
muOutlierPrior[] := NormalDistribution[]
muOutlierInit[] := RandomVariate@muOutlierPrior[]
muOutlierUpdate[c_, sigma2Outlier_, outliers_] :=
	normalNormalMeanSample[muOutlierPrior[], Sqrt[sigma2Outlier], c[[outliers]]]


(* sigma2Outlier *)
(* TODO: Make higher variance? *)
sigma2OutlierPrior[] := InverseGammaDistribution[1.5, 2]
sigma2OutlierInit[] := RandomVariate@sigma2OutlierPrior[]
sigma2OutlierUpdate[c_, muOutlier_, outliers_] :=
	normalNormalVarianceSample[sigma2OutlierPrior[], muOutlier, c[[outliers]]]


(* p *)
pPrior[] := BetaDistribution[1/2, 1]
pInit[] := RandomVariate@pPrior[]
pUpdate[m_] := betaBernoulliSample[pPrior[], m]


(* m *)
mPrior[p_] := BernoulliDistribution[p]
mInit[p_, observations_] := RandomVariate[BernoulliDistribution[p], Length@observations]
mUpdate[p_, deltaStar_, l_, sigma2_, muOutlier_, sigma2Outlier_, c_] :=
	binaryNormalMixtureSample[
		mPrior[p],
		NormalDistribution[muOutlier, Sqrt[sigma2Outlier]],
		NormalDistribution[deltaStar*l, Sqrt[sigma2]],
		c
	]

getInliersOutliers[m_] := Lookup[PositionIndex[m], {0, 1}, {}]


(* muTimes and sigma2Times *)
muTimesPrior[possibleTimeCats_] := AssociationMap[NormalDistribution[0, 6] &, possibleTimeCats]
muTimesInit[possibleTimeCats_] := RandomVariate /@ muTimesPrior[possibleTimeCats]

sigma2TimesPrior[possibleTimeCats_] := AssociationMap[InverseGammaDistribution[1/2, 1/2] &, possibleTimeCats]
sigma2TimesInit[possibleTimeCats_] := RandomVariate /@ sigma2TimesPrior[possibleTimeCats]

muTimesSigma2TimesUpdate[sigma2Times0_, t_, timeCats_, possibleTimeCats_] :=
	Module[{muPrior, sigma2Prior, muTimes, sigma2Times},
		muPrior = muTimesPrior[possibleTimeCats];
		sigma2Prior = sigma2TimesPrior[possibleTimeCats];
		muTimes = sigma2Times = <||>;
		Scan[
			With[{obsT = Pick[t, timeCats, #]},
				muTimes[#] = normalNormalMeanSample[muPrior[#], Sqrt[sigma2Times0[#]], obsT];
				sigma2Times[#] = normalNormalVarianceSample[sigma2Prior[#], muTimes[#], obsT]
			]&,
			possibleTimeCats
		];
		{muTimes, sigma2Times}
	]


(* t *)
tPrior[muTimes_, sigma2Times_, timeCats_] := NormalDistribution[Lookup[muTimes, timeCats], Sqrt@Lookup[sigma2Times, timeCats]]
tInit[muTimes_, sigma2Times_, timeCats_] := normalArraySample@tPrior[muTimes, sigma2Times, timeCats]
tUpdate[muTimes_, sigma2Times_, timeCats_, l_, sigma2_, c_, deltaParams_, inliers_, outliers_] :=
	Module[{t},
		t = ConstantArray[0., Length[c]];
		t[[inliers]] =
			normalNormalRegressionSample[
				NormalDistribution[Lookup[muTimes, timeCats[[inliers]]], Sqrt@Lookup[sigma2Times, timeCats[[inliers]]]],
				Sqrt[sigma2],
				(l (deltaParams[[inliers, 2]] - deltaParams[[inliers, 1]]))/(timeRange[[2]] - timeRange[[1]]),
				l (deltaParams[[inliers, 1]] - (timeRange[[1]] (deltaParams[[inliers, 2]] - deltaParams[[inliers, 1]]))/(timeRange[[2]] - timeRange[[1]])),
				c[[inliers]]
			];
		t[[outliers]] = normalArraySample@NormalDistribution[Lookup[muTimes, timeCats[[outliers]]], Sqrt@Lookup[sigma2Times, timeCats[[outliers]]]];
		t
	]


(* timeCats *)
timeCatsInfo[observations_] := {
		observations[[All, "Time"]],
		DeleteMissing@Union@observations[[All, "Time"]],
		Position[observations[[All, "Time"]], _Missing, {1}, Heads -> False][[All, 1]]
	}

timeCatsDistPrior[possibleTimeCats_] := DirichletDistribution[ConstantArray[1/2, Length@possibleTimeCats]]
timeCatsDistInit[possibleTimeCats_] := dirichletSample@timeCatsDistPrior[possibleTimeCats]
timeCatsDistUpdate[possibleTimeCats_, timeCats_] :=
	dirichletCategoricalSample[timeCatsDistPrior[possibleTimeCats], Lookup[Counts[timeCats], possibleTimeCats, 0]]

timeCatsPrior[possibleTimeCats_, timeCatsDist_] := CategoricalDistribution[possibleTimeCats, timeCatsDist]
timeCatsInit[timeCatsRaw_, possibleTimeCats_, missingTimeCats_, timeCatsDist_] :=
	Module[{timeCats = timeCatsRaw},
		timeCats[[missingTimeCats]] = RandomVariate[timeCatsPrior[possibleTimeCats, timeCatsDist], Length[missingTimeCats]];
		timeCats
	]
timeCatsUpdate[timeCats0_, possibleTimeCats_, missingTimeCats_, timeCatsDist_, muTimes_, sigma2Times_, t_, inliers_, outliers_] :=
	Module[{timeCats = timeCats0, missingTimeCatInliers, missingTimeCatOutliers, inlierCatLogProbs},
		missingTimeCatInliers = Intersection[missingTimeCats, inliers];
		missingTimeCatOutliers = Intersection[missingTimeCats, outliers];

		inlierCatLogProbs =
			Transpose[
				logNormalPDF[NormalDistribution[muTimes[#], Sqrt@sigma2Times[#]], t[[missingTimeCatInliers]]] & /@
					possibleTimeCats
			];

		timeCats[[missingTimeCatInliers]] =
			logPMFSample[#, possibleTimeCats] & /@ inlierCatLogProbs;

		timeCats[[missingTimeCatOutliers]] =
			RandomVariate[CategoricalDistribution[possibleTimeCats, timeCatsDist], Length[missingTimeCatOutliers]];

		timeCats
	]


(* d *)
dateInfo[observations_] :=
	Module[{missingDates},
		missingDates = Position[observations[[All, "Date"]], _Missing, {1}, Heads -> False][[All, 1]];
		dateRanges = DateRange[#EarliestDate, #LatestDate, CalendarType->"Julian"] &/@ observations;
		{
			dateRanges,
			missingDates
		}
	]

dPrior[dateRanges_] :=
	DiscreteUniformDistribution[{0, Length[#]-1}] &/@ dateRanges

dInit[dateRanges_, missingDates_] :=
	MapThread[
		#1[[RandomVariate[#2]+1]]&,
		{dateRanges, dPrior[dateRanges]}
	]

dUpdate[observations_, c_, l_, sigma2_, t_, dateRanges_, missingDates_, inliers_, outliers_] :=
	Module[{d, missingDateInliers, missingDateOutliers},

		If[missingDates === {}, Return[dateRanges[[All,1]]]];

		d = dateRanges[[All, 1]];
		missingDateInliers = Intersection[missingDates, inliers];
		missingDateOutliers = Intersection[missingDates, outliers];

		d[[missingDateInliers]] =
				MapThread[
					Function[{obs, ts, cs, dateRange, prior}, Module[{dmax, distances, dayLogProbs, priorLogProbs, logProbs},
						dmax = Length[dateRange];
						distances =
							objectDistanceApprox[
								ConstantArray[obs["Object"], dmax],
								ConstantArray[obs["Reference"], dmax],
								ConstantArray[obs["Relation"], dmax],
								dateRange,
								ts
							];

					dayLogProbs = logNormalPDF[NormalDistribution[distances*l, Sqrt[sigma2]], cs];
					priorLogProbs = N[LogLikelihood[prior, {#}] &/@ Range[0, Length[dateRange]-1]];
					logProbs = priorLogProbs + dayLogProbs;
					(*TODO: Is it right to normalize here or before combining the prior with the dayLogProbs?*)
					(* logProbs = logProbs - logSumExp[logProbs]; *)
					logPMFSample[logProbs, dateRange]
				]],
				{
					observations[[missingDateInliers]],
					t[[missingDateInliers]],
					c[[missingDateInliers]],
					dateRanges[[missingDateInliers]],
					dPrior[dateRanges[[missingDateInliers]]]
				}
			];

		d[[missingDateOutliers]] =
			MapThread[
				#1[[RandomVariate[#2]+1]]&,
				{
					dateRanges[[missingDateOutliers]],
					dPrior[dateRanges[[missingDateOutliers]]]
				}
			];

		d
	]


(* Full model *)
fitModel[observations_, steps_, vars_ : {}] :=
	Module[{
			res,
			deltaParams, deltaStar,
			c, l, sigma2,
			muOutlier, sigma2Outlier,
			muTimes, sigma2Times,
			timeCatsRaw, timeCats, possibleTimeCats, missingTimeCats, timeCatsDist, t,
			d, missingDates, dateRanges,
			m, p, inliers, outliers
		},

		(*Observed data*)
		c = N[observationCubitsSigned /@ observations];

		(* Initialization *)

		(* Time categories *)
		{timeCatsRaw, possibleTimeCats, missingTimeCats} = timeCatsInfo[observations];
		timeCatsDist = timeCatsDistInit[possibleTimeCats];
		timeCats = timeCatsInit[timeCatsRaw, possibleTimeCats, missingTimeCats, timeCatsDist];

		(* Times *)
		muTimes = muTimesInit[possibleTimeCats];
		sigma2Times = sigma2TimesInit[possibleTimeCats];
		t = tInit[muTimes, sigma2Times, timeCats];

		(* Outlier detection *)
		p = pInit[];
		m = mInit[p, observations];
		{inliers, outliers} = getInliersOutliers[m];

		(* Inlier model *)
		l = lInit[];
		sigma2 = sigma2Init[];

		(* Outlier model *)
		muOutlier = muOutlierInit[];
		sigma2Outlier = sigma2OutlierInit[];

		(* Dates *)
		{dateRanges, missingDates} = dateInfo[observations];
		d = dInit[dateRanges, missingDates];

		(*Compute true distances*)
		deltaParams = objectDistanceApproxParams[
				observations[[All, "Object"]],
				observations[[All, "Reference"]],
				observations[[All, "Relation"]],
				d
			];
		deltaStar = objectDistanceApprox[deltaParams, t];

		(*Updates*)
		res = Reap@GeneralUtilities`MonitoredScan[
			Function[

				d = dUpdate[observations, c, l, sigma2, t, dateRanges, missingDates, inliers, outliers];

				(* Update true params because they depend on d *)
				deltaParams = objectDistanceApproxParams[
						observations[[All, "Object"]],
						observations[[All, "Reference"]],
						observations[[All, "Relation"]],
						d
					];

				t = tUpdate[muTimes, sigma2Times, timeCats, l, sigma2, c, deltaParams, inliers, outliers];

				(* Update true distance because they depend on t *)
				deltaStar = objectDistanceApprox[deltaParams, t];

				(* Inlier model *)
				l = lUpdate[c, deltaStar, sigma2, inliers];
				sigma2 = sigma2Update[c, deltaStar, l, inliers];

				(* Outlier model *)
				muOutlier = muOutlierUpdate[c, sigma2Outlier, outliers];
				sigma2Outlier = sigma2OutlierUpdate[c, muOutlier, outliers];

				(* Outlier detection *)
				m = mUpdate[p, deltaStar, l, sigma2, muOutlier, sigma2Outlier, c];
				p = pUpdate[m];
				{inliers, outliers} = getInliersOutliers[m];

				(* Times *)
				{muTimes, sigma2Times} = muTimesSigma2TimesUpdate[sigma2Times, t, timeCats, possibleTimeCats];

				(* Time categories *)
				timeCatsDist = timeCatsDistUpdate[possibleTimeCats, timeCats];
				timeCats = timeCatsUpdate[timeCats, possibleTimeCats, missingTimeCats, timeCatsDist, muTimes, sigma2Times, t, inliers, outliers];

				Sow@KeyTake[vars]@<|
						"p" -> p,
						"m" -> m,
						"muTimes" -> muTimes,
						"sigma2Times" -> sigma2Times,
						"t" -> t,
						"l" -> l,
						"sigma2" -> sigma2,
						"muOutlier" -> muOutlier,
						"sigma2Outlier" -> sigma2Outlier,
						"d" -> d,
						"timeCats" -> timeCats,
						"timeCatsDist" -> timeCatsDist
					|>
			],
			Range[steps]
		];

		res[[2, 1]]
	]


End[];
EndPackage[];