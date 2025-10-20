BeginPackage["AstronomicalDiaries`Modeling`"];

fitModel

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Modeling`Utilities`"]
Needs["AstronomicalDiaries`Modeling`ConditionalSampling`"]
Needs["AstronomicalDiaries`Modeling`TrueModel`"]
Needs["AstronomicalDiaries`Modeling`GriddyGibbs`"]
Needs["AstronomicalDiaries`Modeling`Interpolation`"]
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
lInit[] := <|"l" -> RandomVariate@lPrior[]|>
lUpdate[s_] :=
	<|"l" -> normalNormalRegressionSample[lPrior[], Sqrt[s["sigma2"]], s[["deltaStar", s["inliers"]]], 0, s[["c", s["inliers"]]]]|>


(* sigma2 *)
sigma2Prior[] := InverseGammaDistribution[1/2, 1/2]
sigma2Init[] := <|"sigma2" -> RandomVariate@sigma2Prior[]|>
sigma2Update[s_] :=
	<|"sigma2" -> normalNormalVarianceSample[sigma2Prior[], s[["deltaStar", s["inliers"]]]*s["l"], s[["c", s["inliers"]]]]|>


(* muOutlier *)
muOutlierPrior[] := NormalDistribution[]
muOutlierInit[] := <|"muOutlier" -> RandomVariate@muOutlierPrior[]|>
muOutlierUpdate[s_] :=
	<|"muOutlier" -> normalNormalMeanSample[muOutlierPrior[], Sqrt[s["sigma2Outlier"]], s[["c", s["outliers"]]]]|>


(* sigma2Outlier *)
(* TODO: Make higher variance? *)
sigma2OutlierPrior[] := InverseGammaDistribution[1.5, 2]
sigma2OutlierInit[] := <|"sigma2Outlier" -> RandomVariate@sigma2OutlierPrior[]|>
sigma2OutlierUpdate[s_] :=
	<|"sigma2Outlier" -> normalNormalVarianceSample[sigma2OutlierPrior[], s["muOutlier"], s[["c", s["outliers"]]]]|>


(* p *)
pPrior[] := BetaDistribution[1/2, 1]
pInit[] := <|"p" -> RandomVariate@pPrior[]|>
pUpdate[s_] := <|"p" -> betaBernoulliSample[pPrior[], s["m"]]|>


(* m *)
getInliersOutliers[m_] :=
	Module[{inliers, outliers},
		{inliers, outliers} = Lookup[PositionIndex[m], {0, 1}, {}];
		<|"inliers" -> inliers, "outliers" -> outliers|>
	]

mPrior[p_] := BernoulliDistribution[p]
mInit[s_] :=
	With[{m = RandomVariate[mPrior[s["p"]], Length@s["observations"]]},
		<|"m" -> m, getInliersOutliers[m]|>
	]
mUpdate[s_] :=
	Module[{m},
		m =
			binaryNormalMixtureSample[
				mPrior[s["p"]],
				NormalDistribution[s["muOutlier"], Sqrt[s["sigma2Outlier"]]],
				NormalDistribution[s["deltaStar"]*s["l"], Sqrt[s["sigma2"]]],
				s["c"]
			];
		<|"m" -> m, getInliersOutliers[m]|>
	]


(* muTimes and sigma2Times *)
muTimesPrior[possibleTimeCats_] := AssociationMap[NormalDistribution[0, 6]&, possibleTimeCats]
muTimesInit[s_] := <|"muTimes" -> RandomVariate /@ muTimesPrior[s["possibleTimeCats"]]|>

sigma2TimesPrior[possibleTimeCats_] := AssociationMap[InverseGammaDistribution[1/2, 1/2]&, possibleTimeCats]
sigma2TimesInit[s_] := <|"sigma2Times" -> RandomVariate /@ sigma2TimesPrior[s["possibleTimeCats"]]|>

muTimesSigma2TimesUpdate[s_] :=
	Module[{sigma2Times0, muPrior, sigma2Prior, muTimes, sigma2Times},
		sigma2Times0 = s["sigma2Times"];
		muPrior = muTimesPrior[s["possibleTimeCats"]];
		sigma2Prior = sigma2TimesPrior[s["possibleTimeCats"]];
		muTimes = sigma2Times = <||>;
		Scan[
			With[{obsT = Pick[s["t"], s["timeCats"], #]},
				muTimes[#] = normalNormalMeanSample[muPrior[#], Sqrt[sigma2Times0[#]], obsT];
				sigma2Times[#] = normalNormalVarianceSample[sigma2Prior[#], muTimes[#], obsT]
			]&,
			s["possibleTimeCats"]
		];

		<|"muTimes" -> muTimes, "sigma2Times" -> sigma2Times|>
	]


(* delta *)
setDeltaParams[s_] :=
	<|"deltaParams" -> objectDistanceApproxParams[
		s[["observations", All, "Object"]],
		s[["observations", All, "Reference"]],
		s[["observations", All, "Relation"]],
		s["d"]
	]|>

setDeltaStar[s_] :=
	<|"deltaStar" -> objectDistanceApprox[s["deltaParams"], s["t"]]|>


(* t *)
computeTimeLogPDFs[muTimes_, sigma2Times_, timeCats_, l_, sigma2_, c_, distances_, samplePoints_] :=
	Module[{logPriors, logLikelihoods, logPDFs},

		logPriors = logNormalPDF[tPrior[timeCats, muTimes, sigma2Times], samplePoints];
		logLikelihoods = logNormalPDF[NormalDistribution[distances*l, Sqrt[sigma2]], c];
		logPDFs = logPriors + logLikelihoods;

		logPDFs
	]

tFixedSamplePointCount = 200;
tAdaptiveSamplePointCount = 50;
tSamplePointsUpdateInterval = 50;

jitteredGrid[min_, max_, len_] :=
	Subdivide[min, max, len - 1] + RandomReal[{-1, 1}*(max - min)/(len - 1)/2]

tSamplePointsDistancesInit[s_] :=
	Module[{samplePoints, distances},
		samplePoints = ConstantArray[jitteredGrid[-12,12,tFixedSamplePointCount + tAdaptiveSamplePointCount], Length[s["deltaParams"]]];
		distances = objectDistanceApprox[s["deltaParams"], samplePoints];

		<|"tSamplePoints" -> samplePoints, "tSampleDistances" -> distances|>
	]

tSamplePointsDistancesUpdate[s_, idxs_:All] :=
	Module[{s0, metaSamplePoints, metaDistances, logPDFs, adaptiveGrid, adaptiveDistances, samplePoints, distances, len},
		len = If[idxs === All, Length[s["c"]], Length[idxs]];
		metaSamplePoints = ConstantArray[jitteredGrid[-12,12,tFixedSamplePointCount], len];
		metaDistances = objectDistanceApprox[s[["deltaParams", idxs]], metaSamplePoints];

		logPDFs = computeTimeLogPDFs[s["muTimes"], s["sigma2Times"], s[["timeCats", idxs]], s["l"], s["sigma2"], s[["c", idxs]], metaDistances, metaSamplePoints];
		
		adaptiveGrid = griddyGibbsMakeGrid[metaSamplePoints, logPDFs, tAdaptiveSamplePointCount];
		adaptiveDistances = objectDistanceApprox[s[["deltaParams", idxs]], adaptiveGrid];

		{samplePoints, distances} =
			Transpose[
				SortBy[First] /@ Transpose[
					{
						Join[metaSamplePoints, adaptiveGrid, 2],
						Join[metaDistances, adaptiveDistances, 2]
					},
					{3, 1, 2}
				],
				{2, 3, 1}
			];

		s0 = s;
		s0[["tSamplePoints", idxs]] = samplePoints;
		s0[["tSampleDistances", idxs]] = distances;
		<|"tSamplePoints" -> s0["tSamplePoints"], "tSampleDistances" -> s0["tSampleDistances"]|>
	]

tPrior[timeCats_, muTimes_, sigma2Times_] := NormalDistribution[Lookup[muTimes, timeCats], Sqrt@Lookup[sigma2Times, timeCats]]
tInit[s_] := <|"t" -> normalArraySample@tPrior[s["timeCats"], s["muTimes"], s["sigma2Times"]], "oldDeltaParams" -> Missing[]|>
tUpdate[s0_] :=
	Module[{s = s0, t, changedIndices, cacheUpdate = <||>, samplePoints, logPDFs},

		(* If deltaParams was changed since the last iteration, recompute the cached grid values *)
		If[!MissingQ[s["oldDeltaParams"]] && s["oldDeltaParams"] =!= s["deltaParams"],
			changedIndices = Position[MapThread[SameQ, {s["oldDeltaParams"], s["deltaParams"]}],False,{1}][[All,1]];
			If[Length[changedIndices] > 0,
				cacheUpdate = tSamplePointsDistancesUpdate[s, changedIndices];
				s //= Append@cacheUpdate
			]
		];

		t = ConstantArray[0., Length[s["observations"]]];

		samplePoints = s[["tSamplePoints", s["inliers"]]];
		logPDFs = computeTimeLogPDFs[s["muTimes"], s["sigma2Times"], s[["timeCats", s["inliers"]]], s["l"], s["sigma2"], s[["c", s["inliers"]]], s[["tSampleDistances", s["inliers"]]], s[["tSamplePoints", s["inliers"]]]];

		t[[s["inliers"]]] = griddyGibbsSample[samplePoints, logPDFs];

		t[[s["outliers"]]] = normalArraySample@NormalDistribution[Lookup[s["muTimes"], s[["timeCats", s["outliers"]]]], Sqrt@Lookup[s["sigma2Times"], s[["timeCats", s["outliers"]]]]];

		<|"t" -> t, "oldDeltaParams" -> s["deltaParams"], cacheUpdate|>
	]


(* timeCats *)
timeCatsInfo[s_] := <|
		"timeCatsRaw" -> s["observations"][[All, "Time"]],
		"possibleTimeCats" -> DeleteMissing@Union@s["observations"][[All, "Time"]],
		"missingTimeCats" -> Position[s["observations"][[All, "Time"]], _Missing, {1}, Heads -> False][[All, 1]]
	|>

timeCatsDistPrior[possibleTimeCats_] := DirichletDistribution[ConstantArray[1/2, Length@possibleTimeCats]]
timeCatsDistInit[s_] := <|"timeCatsDist" -> dirichletSample@timeCatsDistPrior[s["possibleTimeCats"]]|>
timeCatsDistUpdate[s_] :=
	<|"timeCatsDist" -> dirichletCategoricalSample[timeCatsDistPrior[s["possibleTimeCats"]], Lookup[Counts[s["timeCats"]], s["possibleTimeCats"], 0]]|>

timeCatsPrior[possibleTimeCats_, timeCatsDist_] := CategoricalDistribution[possibleTimeCats, timeCatsDist]
timeCatsInit[s_] :=
	Module[{timeCats = s["timeCatsRaw"]},
		timeCats[[s["missingTimeCats"]]] = RandomVariate[timeCatsPrior[s["possibleTimeCats"], s["timeCatsDist"]], Length[s["missingTimeCats"]]];
		<|"timeCats" -> timeCats|>
	]
timeCatsUpdate[s_] :=
	Module[{timeCats = s["timeCats"], missingTimeCatInliers, missingTimeCatOutliers, inlierCatLogProbs},
		missingTimeCatInliers = Intersection[s["missingTimeCats"], s["inliers"]];
		missingTimeCatOutliers = Intersection[s["missingTimeCats"], s["outliers"]];

		inlierCatLogProbs =
			Transpose[
				logNormalPDF[NormalDistribution[s[["muTimes", #]], Sqrt@s[["sigma2Times", #]]], s[["t", missingTimeCatInliers]]] & /@
					s["possibleTimeCats"]
			];

		timeCats[[missingTimeCatInliers]] =
			logPMFSample[#, s["possibleTimeCats"]] & /@ inlierCatLogProbs;

		timeCats[[missingTimeCatOutliers]] =
			RandomVariate[CategoricalDistribution[s["possibleTimeCats"], s["timeCatsDist"]], Length[missingTimeCatOutliers]];

		<|"timeCats" -> timeCats|>
	]


(* d *)
(* d represents the dates of all observations. It needs to be updated once after all of the day, month, and year sampling is done. *)
setD[s_] :=
	<|
		"d" -> MapThread[
				{y, m, ed, delta} |-> ADFromBabylonianDate[{y, m, ed + delta}],
				{s["years"], s["months"], s["earliestDays"], s["deltaD"]}
			]
	|>

(* deltaD *)
deltaDPrior[observations_] :=
	DiscreteUniformDistribution[{0, #LatestDay - #EarliestDay}] &/@ observations

deltaDInit[s_] :=
	<|
		"deltaD" -> RandomVariate /@ deltaDPrior[s["observations"]],
		"missingDays" -> Position[s[["observations", All, "Date"]], _Missing, {1}, Heads -> False][[All, 1]],
		"earliestDays" -> s[["observations", All, "EarliestDay"]]
	|>

deltaDUpdate[s_] :=
	Module[{deltaD, missingDateInliers, missingDateOutliers, priors},

		If[s["missingDays"] === {}, Return@<||>];

		deltaD = ConstantArray[0, Length[s["observations"]]];

		missingDateInliers = Intersection[s["missingDays"], s["inliers"]];
		missingDateOutliers = Intersection[s["missingDays"], s["outliers"]];

		priors = deltaDPrior[s["observations"]];

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
								ConstantArray[ts, maxDeltaD + 1]
							];

						dayLogProbs = logNormalPDF[NormalDistribution[distances*s["l"], Sqrt[s["sigma2"]]], cs];
						priorLogProbs = N[LogLikelihood[prior, {#}] &/@ Range[0, maxDeltaD]];
						logProbs = priorLogProbs + dayLogProbs;
						logPMFSample[logProbs]
				]],
				{
					s[["observations", missingDateInliers]],
					s[["t", missingDateInliers]],
					s[["c", missingDateInliers]],
					s[["years", missingDateInliers]],
					s[["months", missingDateInliers]],
					priors[[missingDateInliers]]
				}
			];

		deltaD[[missingDateOutliers]] =
			RandomVariate /@ priors[[missingDateOutliers]];

		<|"deltaD" -> deltaD|>
	]


(* months *)
getMonthGroup[Missing["Unknown", i_]] := i
getMonthGroup[a_] := Missing[]

(* monthsPrior[observations_] := CategoricalDistribution[getYearMonths[#SEYear]] &/@ observations *)
monthsInit[s_] :=
	Module[{months, missingMonthGroups, missingMonths},
		months = s[["observations", All, "Month"]];
		missingMonthGroups = KeyDrop[PositionIndex[getMonthGroup /@ months], Missing[]];
		missingMonths = Catenate@missingMonthGroups;
		months[[missingMonths]] = RandomChoice[getYearMonths[#]] &/@ s[["years", missingMonths]];
		<|"months" -> months, "missingMonthGroups" -> missingMonthGroups|>
	]

monthsUpdate[s_] :=
	Module[{months, missingMonths, missingMonthInliers, missingMonthOutliers, monthPosteriors, groupSamples},

		If[s["missingMonthGroups"] === <||>, Return@<||>];

		months = s[["observations", All, "Month"]];

		missingMonths = Catenate@s["missingMonthGroups"];

		missingMonthInliers = Intersection[missingMonths, s["inliers"]];
		missingMonthOutliers = Intersection[missingMonths, s["outliers"]];
		
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
								ConstantArray[ts, Length[possibleMonths]]
							];

						monthLogProbs = logNormalPDF[NormalDistribution[distances*s["l"], Sqrt[s["sigma2"]]], cs];
						(* Currently the prior is uniform over possible months *)
						priorLogProbs = 0;
						logProbs = priorLogProbs + monthLogProbs;
						logProbs
				]],
				{
					s[["observations", missingMonthInliers]],
					s[["t", missingMonthInliers]],
					s[["c", missingMonthInliers]],
					s[["years", missingMonthInliers]],
					s[["deltaD", missingMonthInliers]]
				}
			];

		monthPosteriors[[missingMonthOutliers]] =
			ConstantArray[0, Length@getYearMonths@#] &/@ s[["years", missingMonthOutliers]];

		groupSamples = logPMFSample[Total[monthPosteriors[[#]]], getYearMonths[s[["years", #[[1]]]]]] &/@ s["missingMonthGroups"];

		MapThread[(months[[#1]] = #2)&, {s["missingMonthGroups"], groupSamples}];

		<|"months" -> months|>
	]


(* years *)
getYearGroup[Missing["Unknown", i_]] := i
getYearGroup[a_] := Missing[]

priorYearRange := priorYearRange = MinMax@Keys[ADChronology[]][[All, 1]];

yearsInit[s_] :=
	Module[{years, missingYearGroups, missingYears},
		years = s[["observations", All, "SEYear"]];
		missingYearGroups = KeyDrop[PositionIndex[getYearGroup /@ years], Missing[]];
		missingYears = Catenate@missingYearGroups;
		years[[missingYears]] =
			If[MissingQ[#Month],
				RandomInteger[priorYearRange],
				RandomChoice[getMonthYears[#Month]]
			] &/@ s[["observations", missingYears]];

		<|"years" -> years, "missingYearGroups" -> missingYearGroups|>
	]

yearsUpdate[s_] :=
	Module[{years, missingYears, missingYearInliers, missingYearOutliers, yearPosteriors, groupSamples},

		If[s["missingYearGroups"] === <||>, Return@<||>];

		years = s[["observations", All, "SEYear"]];

		missingYears = Catenate@s["missingYearGroups"];

		missingYearInliers = Intersection[missingYears, s["inliers"]];
		missingYearOutliers = Intersection[missingYears, s["outliers"]];
		
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
								ConstantArray[ts, Length[possibleYears]]
							];

						yearLogProbs = logNormalPDF[NormalDistribution[distances*s["l"], Sqrt[s["sigma2"]]], cs];
						(* Currently the prior is uniform over possible years *)
						priorLogProbs = 0;
						logProbs = priorLogProbs + yearLogProbs;
						logProbs
				]],
				{
					s[["observations", missingYearInliers]],
					s[["t", missingYearInliers]],
					s[["c", missingYearInliers]],
					s[["months", missingYearInliers]],
					s[["deltaD", missingYearInliers]]
				}
			];

		yearPosteriors[[missingYearOutliers]] =
			ConstantArray[0, Length@getMonthYears@#] &/@ s[["months", missingYearOutliers]];

		groupSamples = logPMFSample[Total[yearPosteriors[[#]]], getMonthYears[s[["months", #[[1]]]]]] &/@ s["missingYearGroups"];

		MapThread[(years[[#1]] = #2)&, {s["missingYearGroups"], groupSamples}];

		<|"years" -> years|>
	]


initializeModel[observations_] :=
	Module[{s},

		If[!modelObservationQ[observations],
			Return@Failure["InvalidObservations", <|
				"Message" -> "Some input observations cannot be used for inferencing.",
				"InvalidObservations" -> Discard[observations, modelObservationQ]
			|>]
		];

		s = <||>;

		(*Observed data*)
		s["observations"] = observations;
		s["c"] = N[observationCubitsSigned /@ observations];

		(* Initialization *)

		(* Time categories *)
		s //= Append@timeCatsInfo[s];
		s //= Append@timeCatsDistInit[s];
		s //= Append@timeCatsInit[s];

		(* Times *)
		s //= Append@muTimesInit[s];
		s //= Append@sigma2TimesInit[s];
		s //= Append@tInit[s];

		(* Outlier detection *)
		s //= Append@pInit[];
		s //= Append@mInit[s];

		(* Inlier model *)
		s //= Append@lInit[];
		s //= Append@sigma2Init[];

		(* Outlier model *)
		s //= Append@muOutlierInit[];
		s //= Append@sigma2OutlierInit[];

		(* Dates *)
		s //= Append@yearsInit[s];
		s //= Append@monthsInit[s];

		(* Day ranges *)
		s //= Append@deltaDInit[s];

		s //= Append@setD[s];

		(*Compute true distances*)
		s //= Append@setDeltaParams[s];
		s //= Append@setDeltaStar[s];

		(* Get the sample points and distances for time updates *)
		(* TODO: Move this higher? *)
		s //= Append@tSamplePointsDistancesInit[s];

		s
	]


updateModel[s0_] :=
	Module[{s = s0},

		s //= Append@deltaDUpdate[s];
		s //= Append@monthsUpdate[s];
		s //= Append@yearsUpdate[s];
		s //= Append@setD[s];

		(* Update true params because they depend on d *)
		s //= Append@setDeltaParams[s];

		(* Update observation times *)
		s //= Append@tUpdate[s];

		(* Update true distance because they depend on t *)
		s //= Append@setDeltaStar[s];

		(* Inlier model *)
		s //= Append@lUpdate[s];
		s //= Append@sigma2Update[s];

		(* Outlier model *)
		s //= Append@muOutlierUpdate[s];
		s //= Append@sigma2OutlierUpdate[s];

		(* Outlier detection *)
		s //= Append@mUpdate[s];
		s //= Append@pUpdate[s];

		(* Times *)
		s //= Append@muTimesSigma2TimesUpdate[s];

		(* Time categories *)
		s //= Append@timeCatsDistUpdate[s];
		s //= Append@timeCatsUpdate[s];

		s
	]


(* Full model *)
fitModel[observations_, steps_, vars_ : {}] :=
	Module[{s, oldDeltaParams = Missing[], changedIndices},

		s = initializeModel[observations];

		(*Updates*)
		GeneralUtilities`MonitoredMap[
			Function[

				s = updateModel[s];

				(* Every tSamplePointsUpdateInterval steps, recompute the grid used for time estimates of all observations *)
				If[Divisible[#, tSamplePointsUpdateInterval],
					s //= Append@tSamplePointsDistancesUpdate[s]
				];

				KeyTake[s, Replace[vars, All -> Keys[s]]]
			],
			Range[steps]
		]
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
