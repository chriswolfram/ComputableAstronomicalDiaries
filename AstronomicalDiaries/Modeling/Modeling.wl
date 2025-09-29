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
	<|"l" -> normalNormalRegressionSample[lPrior[], Sqrt[s["sigma2"]], s[["deltaStar", s["inliers"]]], 0, s[["c", s["inliers"]]]]|>;


(* sigma2 *)
sigma2Prior[] := InverseGammaDistribution[1/2, 1/2]
sigma2Init[] := <|"sigma2" -> RandomVariate@sigma2Prior[]|>
sigma2Update[s_] :=
	normalNormalVarianceSample[sigma2Prior[], s[["deltaStar", s["inliers"]]]*s["l"], s[["c", s["inliers"]]]];


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
	<|"sigma2Outlier" -> normalNormalVarianceSample[sigma2OutlierPrior[], s["muOutlier", s[["c", s["outliers"]]]]]|>


(* p *)
pPrior[] := BetaDistribution[1/2, 1]
pInit[] := <|"p" -> RandomVariate@pPrior[]|>
pUpdate[s_] := <|"p" -> betaBernoulliSample[pPrior[], s["m"]]|>


(* m *)
mPrior[s_] := BernoulliDistribution[s["p"]]
mInit[s_] := RandomVariate[BernoulliDistribution[s["p"]], Length@s["observations"]]
mUpdate[s_] :=
	<|"m" -> binaryNormalMixtureSample[
		mPrior[s],
		NormalDistribution[s["muOutlier"], Sqrt[s["sigma2Outlier"]]],
		NormalDistribution[s["deltaStar"]*s["l"], Sqrt[s["sigma2"]]],
		c
	]|>

getInliersOutliers[s_] :=
	Module[{inliers, outliers},
		{inliers, outliers} = Lookup[PositionIndex[m], {0, 1}, {}];
		<|"inliers" -> inliers, "outliers" -> outliers|>
	]


(* muTimes and sigma2Times *)
muTimesPrior[s_] := AssociationMap[NormalDistribution[0, 6]&, s["possibleTimeCats"]]
muTimesInit[s_] := RandomVariate /@ muTimesPrior[s]

sigma2TimesPrior[s_] := AssociationMap[InverseGammaDistribution[1/2, 1/2]&, s["possibleTimeCats"]]
sigma2TimesInit[s_] := RandomVariate /@ sigma2TimesPrior[s]

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
		<|"muTimes" -> muTimes, sigma2Times -> "sigma2Times"|>
	]


(* t *)
computeTimeLogPDFs[muTimes_, sigma2Times_, timeCats_, l_, sigma2_, c_, distances_, samplePoints_] :=
	Module[{logPriors, logLikelihoods, logPDFs},

		logPriors = logNormalPDF[tPrior[muTimes, sigma2Times, timeCats], samplePoints];
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
		samplePoints = ConstantArray[jitteredGrid[-12,12,tFixedSamplePointCount + tAdaptiveSamplePointCount], Length[deltaParams]];
		distances = objectDistanceApprox[deltaParams, samplePoints];
		{samplePoints, distances}
	]

tSamplePointsDistancesUpdate[s_] :=
	Module[{metaSamplePoints, metaDistances, logPDFs, adaptiveGrid, adaptiveDistances, samplePoints, distances},
		metaSamplePoints = ConstantArray[jitteredGrid[-12,12,tFixedSamplePointCount], Length[s["c"]]];
		metaDistances = objectDistanceApprox[s["deltaParams"], metaSamplePoints];

		logPDFs = computeTimeLogPDFs[s["muTimes"], s["sigma2Times"], s["timeCats"], s["l"], s["sigma2"], s["c"], metaDistances, s["tSamplePoints"]];
		
		adaptiveGrid = griddyGibbsMakeGrid[metaSamplePoints, logPDFs, tAdaptiveSamplePointCount];
		adaptiveDistances = objectDistanceApprox[s["deltaParams"], adaptiveGrid];

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

		<|"tSamplePoints" -> samplePoints, "distances" -> distances|>
	]

tPrior[s_] := NormalDistribution[Lookup[s["muTimes"], s["timeCats"]], Sqrt@Lookup[s["sigma2Times"], s["timeCats"]]]
tInit[s_] := normalArraySample@tPrior[s]
tUpdate[s_] :=
	Module[{t, samplePoints, logPDFs},
		t = ConstantArray[0., Length[s["observations"]]];

		samplePoints = s[["tSamplePoints", s["inliers"]]];
		logPDFs = computeTimeLogPDFs[s["muTimes"], s["sigma2Times"], s[["timeCats", s["inliers"]]], s["l"], s["sigma2"], s[["c", s["inliers"]]], s[["distances", s["inliers"]]], s["tSamplePoints"]];

		t[[s["inliers"]]] = griddyGibbsSample[samplePoints, logPDFs];

		t[[s["outliers"]]] = normalArraySample@NormalDistribution[Lookup[s["muTimes"], s[["timeCats", s["outliers"]]]], Sqrt@Lookup[s["sigma2Times"], s[["timeCats", s["outliers"]]]]];
		
		<|"t" -> t|>
	]


(* timeCats *)
timeCatsInfo[s_] := <|
		"timeCatsRaw" -> s["observations"][[All, "Time"]],
		"possibleTimeCats" -> DeleteMissing@Union@s["observations"][[All, "Time"]],
		"missingTimeCats" -> Position[s["observations"][[All, "Time"]], _Missing, {1}, Heads -> False][[All, 1]]
	|>

timeCatsDistPrior[s_] := DirichletDistribution[ConstantArray[1/2, Length@s["possibleTimeCats"]]]
timeCatsDistInit[s_] := <|"timeCatsDist" -> dirichletSample@timeCatsDistPrior[s["possibleTimeCats"]]|>
timeCatsDistUpdate[s_] :=
	<|"timeCatsDist" -> dirichletCategoricalSample[timeCatsDistPrior[s["possibleTimeCats"]], Lookup[Counts[s["timeCats"]], s["possibleTimeCats"], 0]]|>

timeCatsPrior[s_] := CategoricalDistribution[s["possibleTimeCats"], s["timeCatsDist"]]
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


(* deltaD *)
getMissingDays[s_] :=
	Position[s[["observations", All, "Date"]], _Missing, {1}, Heads -> False][[All, 1]]

deltaDPrior[s_] :=
	DiscreteUniformDistribution[{0, #LatestDay - #EarliestDay}] &/@ s["observations"]

deltaDInit[s_] :=
	<|"deltaD" -> RandomVariate /@ deltaDPrior[s["observations"]]|>

deltaDUpdate[s_] :=
	Module[{deltaD, missingDateInliers, missingDateOutliers, priors},

		deltaD = ConstantArray[0, Length[s["observations"]]];

		If[s["missingDays"] === {}, Return@deltaD];

		missingDateInliers = Intersection[s["missingDays"], s["inliers"]];
		missingDateOutliers = Intersection[s["missingDays"], s["inliers"]];

		priors = deltaDPrior[s];

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

						dayLogProbs = logNormalPDF[NormalDistribution[s["distances"]*s["l"], Sqrt[s["sigma2"]]], cs];
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
								ConstantArray[ts, Length[possibleMonths]]
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
								ConstantArray[ts, Length[possibleYears]]
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
			s,

			res,
			deltaParams, deltaStar,
			c, l, sigma2,
			muOutlier, sigma2Outlier,
			muTimes, sigma2Times,
			timeCatsRaw, timeCats, tSamplePoints, tDistances, tDeltaParams, t,
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
		s["missingDays"] = getMissingDays[s];
		s["earliestDays"] = s[["observations", All, "EarliestDay"]];
		deltaD = deltaDInit[s];

		d = MapThread[
				{y, m, ed, delta} |-> ADFromBabylonianDate[{y, m, ed + delta}],
				{years, months, earliestDays, deltaD}
			];

		(*Compute true distances*)
		deltaParams = objectDistanceApproxParams[
				observations[[All, "Object"]],
				observations[[All, "Reference"]],
				observations[[All, "Relation"]],
				d
			];
		deltaStar = objectDistanceApprox[deltaParams, t];

		(* Get the sample points and distances for time updates *)
		{tSamplePoints, tDistances} = tSamplePointsDistancesInit[tDeltaParams = deltaParams];

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
				deltaParams = objectDistanceApproxParams[
						observations[[All, "Object"]],
						observations[[All, "Reference"]],
						observations[[All, "Relation"]],
						d
					];

				(* Every tSamplePointsUpdateInterval steps, recompute the grid used for time estimates for all observations *)
				If[Divisible[#, tSamplePointsUpdateInterval],
					{tSamplePoints, tDistances} = tSamplePointsDistancesUpdate[muTimes, sigma2Times, timeCats, l, sigma2, c, deltaParams];
				];
				(* If deltaParams was change since the last iteration, also recompute the grid *)
				With[{changedIndices = Position[MapThread[SameQ, {deltaParams, tDeltaParams}],False,{1}][[All,1]]},
					If[Length[changedIndices] > 0,
						tDeltaParams = deltaParams;
						{tSamplePoints[[changedIndices]], tDistances[[changedIndices]]} =
							tSamplePointsDistancesUpdate[muTimes, sigma2Times, timeCats[[changedIndices]], l, sigma2, c[[changedIndices]], deltaParams[[changedIndices]]];
					]
				];
				(* Update observation times *)
				t = tUpdate[muTimes, sigma2Times, timeCats, l, sigma2, c, tDistances, inliers, outliers, tSamplePoints];

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
