BeginPackage["AstronomicalDiaries`Modeling`"];

fitModel

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Modeling`Utilities`"]
Needs["AstronomicalDiaries`Modeling`ConditionalSampling`"]
Needs["AstronomicalDiaries`Modeling`TrueModel`"]
Needs["AstronomicalDiaries`Modeling`GriddyGibbs`"]
Needs["AstronomicalDiaries`Chronology`"]
Needs["AstronomicalDiaries`Astronomy`"]


modelObservationQ[obs_?AssociationQ] :=
	! MissingQ[#Object] &&
	! MissingQ[#Reference] &&
	! MissingQ[#EarliestDay] &&
	! MissingQ[#LatestDay] &&
	! MissingQ[#Relation] &&
	! (MissingQ[#Cubits] && MissingQ[#Fingers])&@obs

modelObservationQ[observations_List] :=
	AllTrue[observations, modelObservationQ]


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
tUpdate[muTimes_, sigma2Times_, timeCats_, l_, sigma2_, c_, deltaFunctions_, inliers_, outliers_] :=
	Module[{t, pts, ptsReplicated, distances, logPriors, logLikelihoods, logPDFs},
		t = ConstantArray[0., Length[c]];

		pts = Range[-11,11,0.1] + RandomReal[{-0.5,0.5}];
		ptsReplicated = ConstantArray[pts, Length[inliers]];
		distances = objectDistanceApprox[deltaFunctions[[inliers]], ptsReplicated];

		logPriors = logNormalPDF[tPrior[muTimes, sigma2Times, timeCats[[inliers]]], ptsReplicated];
		logLikelihoods = logNormalPDF[NormalDistribution[distances*l, Sqrt[sigma2]], c[[inliers]]];
		logPDFs = logPriors + logLikelihoods;

		(* TODO: Look at this Clip *)
		(* t[[inliers]] = griddyGibbsSample[Transpose[{pts, Exp[Clip[#, {-10000, Infinity}]] /. -Infinity|Indeterminate -> $MachineEpsilon}]] &/@ logPDFs; *)
		t[[inliers]] = griddyGibbsSampleLog[Transpose[{pts, #}]] &/@ logPDFs;

		(* t[[inliers]] =
			MapThread[
				Function[{deltaFunction, timeCat, cs},
					Module[{prior, likelihood, pdf},
						prior = PDF[NormalDistribution[muTimes[timeCat], Sqrt@sigma2Times[timeCat]], pts];
						likelihood = PDF[NormalDistribution[#*l, Sqrt[sigma2]], cs]&/@deltaFunction[pts];
						(* likelihood = Exp[-(cs - deltaFunction[pts]*l)^2 / (2*sigma2)] / (Sqrt[2 * Pi] * Sqrt[sigma2]); *)
						pdf = Transpose[{pts, prior * likelihood}];
						griddyGibbsSample[pdf]
					]
				],
				{deltaFunctions[[inliers]], timeCats[[inliers]], c[[inliers]]}
			]; *)

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


(* deltaD *)
getMissingDays[observations_] :=
	Position[observations[[All, "Date"]], _Missing, {1}, Heads -> False][[All, 1]]

deltaDPrior[observations_] :=
	DiscreteUniformDistribution[{0, #LatestDay - #EarliestDay}] &/@ observations

deltaDInit[observations_] :=
	RandomVariate /@ deltaDPrior[observations]

deltaDUpdate[observations_, c_, l_, sigma2_, t_, missingDays_, months_, years_, inliers_, outliers_] :=
	Module[{deltaD, missingDateInliers, missingDateOutliers},

		deltaD = ConstantArray[0, Length[observations]];

		If[missingDays === {}, Return@deltaD];

		missingDateInliers = Intersection[missingDays, inliers];
		missingDateOutliers = Intersection[missingDays, outliers];

		deltaD[[missingDateInliers]] =
				MapThread[
					Function[{obs, ts, cs, year, month, prior}, Module[{maxDeltaD, distances, dayLogProbs, priorLogProbs, logProbs},
						maxDeltaD = obs["LatestDay"] - obs["EarliestDay"];
						distances =
							objectDistanceApprox[
								ConstantArray[obs["Object"], maxDeltaD + 1],
								ConstantArray[obs["Reference"], maxDeltaD + 1],
								ConstantArray[obs["Relation"], maxDeltaD + 1],
								ADFromBabylonianDate[{year, month, #}] &/@ Range[obs["EarliestDay"], obs["LatestDay"]],
								ts
							];

						dayLogProbs = logNormalPDF[NormalDistribution[distances*l, Sqrt[sigma2]], cs];
						priorLogProbs = N[LogLikelihood[prior, {#}] &/@ Range[0, maxDeltaD]];
						logProbs = priorLogProbs + dayLogProbs;
						(*TODO: Is it right to normalize here or before combining the prior with the dayLogProbs?*)
						(* logProbs = logProbs - logSumExp[logProbs]; *)
						logPMFSample[logProbs]
				]],
				{
					observations[[missingDateInliers]],
					t[[missingDateInliers]],
					c[[missingDateInliers]],
					years[[missingDateInliers]],
					months[[missingDateInliers]],
					deltaDPrior[observations[[missingDateInliers]]]
				}
			];

		deltaD[[missingDateOutliers]] =
			RandomVariate /@ deltaDPrior[observations[[missingDateOutliers]]];

		deltaD
	]


(* months *)
getMonthGroup[Missing["Unknown", i_]] := i
getMonthGroup[a_] := Missing[]

getMissingMonthGroups[observations_] :=
	KeyDrop[PositionIndex[getMonthGroup /@ observations[[All, "Month"]]], Missing[]]

(* monthsPrior[observations_] := CategoricalDistribution[getYearMonths[#SEYear]] &/@ observations *)
monthsInit[observations_, years_, missingMonthGroups_] :=
	Module[{months, missingMonths},
		months = observations[[All, "Month"]];
		missingMonths = Catenate@missingMonthGroups;
		months[[missingMonths]] = RandomChoice[getYearMonths[#]] &/@ years[[missingMonths]];
		months
	]

monthsUpdate[observations_, c_, l_, sigma2_, t_, missingMonthGroups_, deltaD_, years_, inliers_, outliers_] :=
	Module[{months, missingMonths, missingMonthInliers, missingMonthOutliers, monthPosteriors, groupSamples},

		months = observations[[All, "Month"]];

		If[missingMonthGroups === {}, Return@months];

		missingMonths = Catenate@missingMonthGroups;

		missingMonthInliers = Intersection[missingMonths, inliers];
		missingMonthOutliers = Intersection[missingMonths, outliers];
		
		monthPosteriors = ConstantArray[0, Length[months]];

		monthPosteriors[[missingMonthInliers]] =
				MapThread[
					Function[{obs, ts, cs, year, delta}, Module[{possibleMonths, distances, monthLogProbs, priorLogProbs, logProbs},
						possibleMonths = getYearMonths[year];
						distances =
							objectDistanceApprox[
								ConstantArray[obs["Object"], Length[possibleMonths]],
								ConstantArray[obs["Reference"], Length[possibleMonths]],
								ConstantArray[obs["Relation"], Length[possibleMonths]],
								ADFromBabylonianDate[{year, #, obs["EarliestDay"] + delta}] &/@ possibleMonths,
								ts
							];

						monthLogProbs = logNormalPDF[NormalDistribution[distances*l, Sqrt[sigma2]], cs];
						(* Currently the prior is uniform over possible months *)
						priorLogProbs = 0;
						logProbs = priorLogProbs + monthLogProbs;
						logProbs
				]],
				{
					observations[[missingMonthInliers]],
					t[[missingMonthInliers]],
					c[[missingMonthInliers]],
					years[[missingMonthInliers]],
					deltaD[[missingMonthInliers]]
				}
			];

		monthPosteriors[[missingMonthOutliers]] =
			ConstantArray[0, Length@getYearMonths@#] &/@ years[[missingMonthOutliers]];

		groupSamples = logPMFSample[Total[monthPosteriors[[#]]], getYearMonths[years[[#[[1]]]]]] &/@ missingMonthGroups;

		MapThread[(months[[#1]] = #2)&, {missingMonthGroups, groupSamples}];

		months
	]


(* years *)
getYearGroup[Missing["Unknown", i_]] := i
getYearGroup[a_] := Missing[]

getMissingYearGroups[observations_] :=
	KeyDrop[PositionIndex[getYearGroup /@ observations[[All, "SEYear"]]], Missing[]]


priorYearRange := priorYearRange = MinMax@Keys[ADChronology[]][[All, 1]];

yearsInit[observations_, missingYearGroups_] :=
	Module[{years, missingYears},
		years = observations[[All, "SEYear"]];
		missingYears = Catenate@missingYearGroups;
		years[[missingYears]] =
			If[MissingQ[#Month],
				RandomInteger[priorYearRange],
				RandomChoice[getMonthYears[#Month]]
			] &/@ observations[[missingYears]];
		years
	]

yearsUpdate[observations_, c_, l_, sigma2_, t_, missingYearGroups_, deltaD_, months_, inliers_, outliers_] :=
	Module[{years, missingYears, missingYearInliers, missingYearOutliers, yearPosteriors, groupSamples},

		years = observations[[All, "SEYear"]];

		If[missingYearGroups === {}, Return@years];

		missingYears = Catenate@missingYearGroups;

		missingYearInliers = Intersection[missingYears, inliers];
		missingYearOutliers = Intersection[missingYears, outliers];
		
		yearPosteriors = ConstantArray[0, Length[years]];

		yearPosteriors[[missingYearInliers]] =
				MapThread[
					Function[{obs, ts, cs, month, delta}, Module[{possibleYears, distances, yearLogProbs, priorLogProbs, logProbs},
						possibleYears = getMonthYears[month];
						distances =
							objectDistanceApprox[
								ConstantArray[obs["Object"], Length[possibleYears]],
								ConstantArray[obs["Reference"], Length[possibleYears]],
								ConstantArray[obs["Relation"], Length[possibleYears]],
								ADFromBabylonianDate[{#, month, obs["EarliestDay"] + delta}] &/@ possibleYears,
								ts
							];

						yearLogProbs = logNormalPDF[NormalDistribution[distances*l, Sqrt[sigma2]], cs];
						(* Currently the prior is uniform over possible years *)
						priorLogProbs = 0;
						logProbs = priorLogProbs + yearLogProbs;
						logProbs
				]],
				{
					observations[[missingYearInliers]],
					t[[missingYearInliers]],
					c[[missingYearInliers]],
					months[[missingYearInliers]],
					deltaD[[missingYearInliers]]
				}
			];

		yearPosteriors[[missingYearOutliers]] =
			ConstantArray[0, Length@getMonthYears@#] &/@ months[[missingYearOutliers]];

		groupSamples = logPMFSample[Total[yearPosteriors[[#]]], getMonthYears[months[[#[[1]]]]]] &/@ missingYearGroups;

		MapThread[(years[[#1]] = #2)&, {missingYearGroups, groupSamples}];

		years
	]


(* Full model *)
fitModel[observations_, steps_, vars_ : {}] :=
	Module[{
			res,
			deltaFunctions, deltaStar,
			c, l, sigma2,
			muOutlier, sigma2Outlier,
			muTimes, sigma2Times,
			timeCatsRaw, timeCats, possibleTimeCats, missingTimeCats, timeCatsDist, t,
			years, missingYearGroups,
			months, missingMonthGroups,
			d, deltaD, earliestDays, missingDays,
			m, p, inliers, outliers
		},

		If[!modelObservationQ[observations],
			Return@Failure["InvalidObservations", <|
				"Message" -> "Some input observations cannot be used for inferencing.",
				"InvalidObservations" -> Discard[observations, modelObservationQ]
			|>]
		];

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
		missingYearGroups = getMissingYearGroups[observations];
		years = yearsInit[observations, missingYearGroups];
		missingMonthGroups = getMissingMonthGroups[observations];
		months = monthsInit[observations, years, missingMonthGroups];

		(* Day ranges *)
		missingDays = getMissingDays[observations];
		earliestDays = observations[[All, "EarliestDay"]];
		deltaD = deltaDInit[observations];

		d = MapThread[
				{y, m, ed, delta} |-> ADFromBabylonianDate[{y, m, ed + delta}],
				{years, months, earliestDays, deltaD}
			];

		(*Compute true distances*)
		deltaFunctions = objectDistanceApproxFunction[
				observations[[All, "Object"]],
				observations[[All, "Reference"]],
				observations[[All, "Relation"]],
				d
			];
		deltaStar = objectDistanceApprox[deltaFunctions, t];

		(*Updates*)
		res = Reap@GeneralUtilities`MonitoredScan[
			Function[

				deltaD = deltaDUpdate[observations, c, l, sigma2, t, missingDays, months, years, inliers, outliers];
				months = monthsUpdate[observations, c, l, sigma2, t, missingMonthGroups, deltaD, years, inliers, outliers];
				years = yearsUpdate[observations, c, l, sigma2, t, missingYearGroups, deltaD, months, inliers, outliers];
				d = MapThread[
						{y, m, ed, delta} |-> ADFromBabylonianDate[{y, m, ed + delta}],
						{years, months, earliestDays, deltaD}
					];

				(* Update true params because they depend on d *)
				deltaFunctions = objectDistanceApproxFunction[
						observations[[All, "Object"]],
						observations[[All, "Reference"]],
						observations[[All, "Relation"]],
						d
					];

				t = tUpdate[muTimes, sigma2Times, timeCats, l, sigma2, c, deltaFunctions, inliers, outliers];

				(* Update true distance because they depend on t *)
				deltaStar = objectDistanceApprox[deltaFunctions, t];

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
						"deltaD" -> deltaD,
						"years" -> years,
						"months" -> months,
						"timeCats" -> timeCats,
						"timeCatsDist" -> timeCatsDist
					|>
			],
			Range[steps]
		];

		res[[2, 1]]
	]


(* Options[ADFitModel] = {
	MaxIterations -> 2000,
	"BurnIn" -> 500,
	"Chains" -> 1,
	"Parallel" -> False
};
ADFitModel[observations_, opts:OptionsPattern[]] :=
	Module[{samples, chains, vars, numChains, parallelQ, burnIn, numSamples, tableFun},
		numChains = OptionValue["Chains"];
		parallelQ = numChains === 1 || OptionValue["Parallel"];
		numSamples = OptionValue[MaxIterations];
		burnIn = OptionValue["BurnIn"];

		vars = {
			"p",
			"m",
			"muTimes",
			"sigma2Times",
			"t",
			"l",
			"sigma2",
			"muOutlier",
			"sigma2Outlier",
			"deltaD",
			"years",
			"months",
			"timeCats",
			"timeCatsDist"
		};

		tableFun = If[parallelQ, ParallelTable, Table];
		chains = tableFun[fitModel[observations, numSamples], {numChains}];
		samples = Catenate@chains[[All, 500;;]];

		<|
			"CubitLength" -> EmpiricalDistribution[1/samples[[All,"l"]]],
			"Variance" -> EmpiricalDistribution[1/samples[[All,"sigma2"]]],
			"OutlierProbability" -> EmpiricalDistribution[samples[[All,"p"]]],
			"IdealObservationTimes" -> EmpiricalDistribution /@ Merge[samples[[All, "muTimes"]], Identity],
			"IdealObservationTimesVariance" -> EmpiricalDistribution /@ Merge[samples[[All, "sigma2Times"]], Identity],
			"Observations" ->
				MapThread[
					Function[{obs, t, m, months, years, deltaD, timeCats}, Module[{dateCounts},
						dateCounts =
							KeyMap[
								ADFromBabylonianDate,
								Counts@Transpose@{years, months, obs["EarliestDay"] + deltaD}
							];
						<|
							"Time" -> EmpiricalDistribution[t],
							"TimeCategory" -> EmpiricalDistribution[timeCats],
							"Outlier" -> N@Mean[m],
							"Month" -> EmpiricalDistribution[months],
							"Year" -> EmpiricalDistribution[years],
							"DeltaD" -> EmpiricalDistribution[deltaD],
							"DateProbabilities" -> N@dateCounts / Total[dateCounts],
							"Date" -> EmpiricalDistribution[Values[dateCounts] -> Keys[dateCounts]]
						|>
					]],
					Prepend[Transpose[{
						samples[[All,"t"]],
						samples[[All,"m"]],
						samples[[All,"months"]],
						samples[[All,"years"]],
						samples[[All,"deltaD"]],
						samples[[All,"timeCats"]]
					}, 2<->3], observations]
				]
		|>
	] *)


End[];
EndPackage[];