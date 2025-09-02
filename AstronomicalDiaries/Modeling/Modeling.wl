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


pPrior[] := BetaDistribution[1/2, 1]
pInit[] := RandomVariate@pPrior[]
pUpdate[m_] := betaBernoulliSample[pPrior[], m]


muPrior[p_] := BernoulliDistribution[p]
mInit[p_, observations_] := RandomVariate[BernoulliDistribution[p], Length@observations]
mUpdate[p_, deltaStar_, l_, sigma2_, muOutlier_, sigma2Outlier_, c_] :=
	binaryNormalMixtureSample[
		muPrior[p],
		NormalDistribution[muOutlier, Sqrt[sigma2Outlier]],
		NormalDistribution[deltaStar*l, Sqrt[sigma2]],
		c
	]

getInliersOutliers[m_] := Lookup[PositionIndex[m], {0, 1}, {}]


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


fitModel[observations_, steps_, vars_ : {}] :=
	Module[{res, c, timeCats, possibleTimeCats, p,
	m, muTimes,
	sigma2Times, t, sigma2Prior, sigma2, lPrior,
	l, muOutlierPrior, muOutlier, sigma2OutlierPrior,
	sigma2Outlier, d, missingDates, missingTimes, timeCatDist,
	timeCatDistPrior, deltaParams, deltaStar, outliers, inliers},

		(*Observed data*)
		c = N[observationCubitsSigned /@ observations];
		timeCats = observations[[All, "Time"]];
		possibleTimeCats = DeleteMissing@Union@timeCats;

		missingTimes = Position[timeCats, _Missing, {1}, Heads -> False][[All, 1]];
		timeCatDistPrior = DirichletDistribution[ConstantArray[1/2, Length@possibleTimeCats]];
		timeCatDist = dirichletSample@timeCatDistPrior;
		timeCats[[missingTimes]] = RandomVariate[CategoricalDistribution[possibleTimeCats, timeCatDist], Length[missingTimes]];

		(*Priors and initialization*)
		p = pInit[];
		m = mInit[p, observations];
		{inliers, outliers} = getInliersOutliers[m];

		muTimes = muTimesInit[possibleTimeCats];
		sigma2Times = sigma2TimesInit[possibleTimeCats];
		t = tInit[muTimes, sigma2Times, timeCats];

		sigma2Prior = InverseGammaDistribution[1/2, 1/2]; sigma2 = RandomVariate@sigma2Prior;
		lPrior = NormalDistribution[1/2, 1/2]; l = RandomVariate@lPrior;

		muOutlierPrior = NormalDistribution[]; muOutlier = RandomVariate@muOutlierPrior;
		(*TODO: Make higher variance*)
		sigma2OutlierPrior = InverseGammaDistribution[1.5, 2]; sigma2Outlier = RandomVariate@sigma2OutlierPrior;

		missingDates = Position[MissingQ /@ observations[[All, "Date"]], True, {1}, Heads -> False][[All, 1]];
		d = observations[[All, "Date"]];
		d[[missingDates]] =
			#EarliestDate +
			Quantity[RandomVariate@DiscreteUniformDistribution[{0, #LatestDay - #EarliestDay}], "Days"]& /@ observations[[missingDates]];

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
				Module[{missingDateInliers, missingDateOutliers},
					missingDateInliers = Intersection[missingDates, inliers];
					missingDateOutliers = Intersection[missingDates, outliers];

					d[[missingDateInliers]] =
						MapThread[
							Function[Module[{dmax, distances, dayLogProbs, priorLogProbs, logProbs},
								dmax = #LatestDay - #EarliestDay;
								distances =
									objectDistanceApprox[
										ConstantArray[#Object, dmax + 1],
										ConstantArray[#Reference, dmax + 1],
										ConstantArray[#Relation, dmax + 1],
										(*TODO: precompute the date ranges once*)
										#EarliestDate + Quantity[Range[0, dmax], "Days"],
										#2
									];

							dayLogProbs = logNormalPDF[NormalDistribution[distances*l, Sqrt[sigma2]], #3];
							(*priorLogProbs=LogLikelihood[BetaBinomialDistribution[alpha,beta,dmax],{#}]&/@Range[0,dmax];*)
							priorLogProbs = 0;
							logProbs = priorLogProbs + dayLogProbs;
							(*TODO: Is it right to normalize here or before combining the prior with the dayLogProbs?*)
							logProbs = logProbs - logSumExp[logProbs];
							#EarliestDate + Quantity[logPMFSample[logProbs], "Days"]
						]],
						{observations[[missingDateInliers]], t[[missingDateInliers]], c[[missingDateInliers]]}
					];
				(*d[[missingDateOutliers]] =
					#EarliestDate+
					Quantity[RandomVariate@BetaBinomialDistribution[alpha,beta,#LatestDay-#EarliestDay],"Days"]&/@observations[[missingDateOutliers]];*)
				d[[missingDateOutliers]] =
					#EarliestDate +
					Quantity[RandomVariate@DiscreteUniformDistribution[{0, #LatestDay - #EarliestDay}], "Days"] & /@ observations[[missingDateOutliers]];
				];

				(*Compute true distances*)
				deltaParams = objectDistanceApproxParams[
						observations[[All, "Object"]],
						observations[[All, "Reference"]],
						observations[[All, "Relation"]],
						d
					];
				t = tUpdate[muTimes, sigma2Times, timeCats, l, sigma2, c, deltaParams, inliers, outliers];
				deltaStar = objectDistanceApprox[deltaParams, t];

				l = normalNormalRegressionSample[lPrior, Sqrt[sigma2], deltaStar[[inliers]], 0, c[[inliers]]];
				sigma2 = normalNormalVarianceSample[sigma2Prior, deltaStar[[inliers]]*l, c[[inliers]]];

				muOutlier = normalNormalMeanSample[muOutlierPrior, Sqrt[sigma2Outlier], c[[outliers]]];
				sigma2Outlier = normalNormalVarianceSample[sigma2OutlierPrior, muOutlier, c[[outliers]]];

				m = mUpdate[p, deltaStar, l, sigma2, muOutlier, sigma2Outlier, c];
				{inliers, outliers} = getInliersOutliers[m];

				p = pUpdate[m];

				{muTimes, sigma2Times} = muTimesSigma2TimesUpdate[sigma2Times, t, timeCats, possibleTimeCats];

				Module[{missingTimeCatInliers, missingTimeCatOutliers, inlierCatLogProbs},
					missingTimeCatInliers = Intersection[missingTimes, inliers];
					missingTimeCatOutliers = Intersection[missingTimes, outliers];

					timeCatDist = dirichletCategoricalSample[timeCatDistPrior, Lookup[Counts[timeCats], possibleTimeCats, 0]];

					inlierCatLogProbs =
						Transpose[
							logNormalPDF[NormalDistribution[muTimes[#], Sqrt@sigma2Times[#]], t[[missingTimeCatInliers]]] & /@
								possibleTimeCats
						];
					timeCats[[missingTimeCatInliers]] = logPMFSample[#, possibleTimeCats] & /@ inlierCatLogProbs;

					timeCats[[missingTimeCatOutliers]] = RandomVariate[CategoricalDistribution[possibleTimeCats, timeCatDist], Length[missingTimeCatOutliers]];
				];

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
						"timeCatDist" -> timeCatDist
					|>
			],
			Range[steps]
		];

		res[[2, 1]]
	]


End[];
EndPackage[];