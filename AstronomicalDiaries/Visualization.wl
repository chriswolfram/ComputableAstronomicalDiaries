BeginPackage["AstronomicalDiaries`Visualization`"];

eclipticLatitudeLine
eclipticLongitudeLine

eclipticLatitudeBand
eclipticLongitudeBand

observationHeroPlot

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Astronomy`"]

eclipticLatitudeLine[long_, date_] :=
	Line@AstroPosition[Table[{long, elat}, {elat, -90, 90}], {"Ecliptic", "Date" -> date, "Location" -> $observationLocation}]

eclipticLongitudeLine[lat_, date_] :=
	Line@AstroPosition[Table[{elon, lat}, {elon, -180, 180}], {"Ecliptic", "Date" -> date, "Location" -> $observationLocation}]

eclipticLatitudeBand[{minLong_, maxLong_}, date_] :=
	Polygon@AstroPosition[
		Join[Table[{minLong, elat}, {elat, -90, 90}], Reverse@Table[{maxLong, elat}, {elat, -90, 90}]],
		{"Ecliptic", "Date" -> date, "Location" -> $observationLocation}
	]

eclipticLongitudeBand[{minLat_, maxLat_}, date_] :=
	Polygon@AstroPosition[
		Join[Table[{elon, minLat}, {elon, -180, 180}], Reverse@Table[{elon, maxLat}, {elon, -180, 180}]],
		{"Ecliptic", "Date" -> date, "Location" -> $observationLocation}
	]


samplesCubitLength[samples_] := Quantity[Mean[1/samples[[All, "l"]]], "AngularDegrees"]
samplesObservationStandardDeviation[samples_] := Quantity[Mean@Sqrt@samples[[All, "sigma2"]], "AngularDegrees"]
samplesCredibleInterval[samples_, q_:{0.025, 0.975}] :=
	Quantity[Quantile[
		NormalDistribution[0, QuantityMagnitude[samplesObservationStandardDeviation[samples], "AngularDegrees"]],
		q
	], "AngularDegrees"]


(* Line marking where an object is reported to be *)
observationCenterLine[{samples_, modelObservations_}, obsIdx_, date_] :=
	Module[{obs, cubitLength, observedDist, referencePos},
		obs = modelObservations[[obsIdx]];

		cubitLength = samplesCubitLength[samples];

		observedDist = QuantityMagnitude[observationCubitsSigned[obs]*cubitLength, "AngularDegrees"];
		referencePos = AstroPosition[obs["Reference"], {"Ecliptic", date, $observationLocation}];
		
		If[relationAxes[obs["Relation"]] === 2,
			eclipticLatitudeLine[referencePos["Longitude"] + observedDist, date],
			eclipticLongitudeLine[referencePos["Latitude"] + observedDist, date]
		]
	]


observationBand[{samples_, modelObservations_}, obsIdx_, date_, q_:{0.025, 0.975}] :=
	Module[{obs, objectPos, lowerBound, upperBound},
		obs = modelObservations[[obsIdx]];

		objectPos = AstroPosition[obs["Object"], {"Ecliptic", date, $observationLocation}];
		{lowerBound, upperBound} = samplesCredibleInterval[samples, q];

		If[relationAxes[obs["Relation"]] === 2,
			eclipticLatitudeBand[objectPos["Longitude"] + QuantityMagnitude[{lowerBound, upperBound}, "AngularDegrees"], date],
			eclipticLongitudeBand[objectPos["Latitude"] + QuantityMagnitude[{lowerBound, upperBound}, "AngularDegrees"], date]
		]
	]


observationDistanceLine[{samples_, modelObservations_}, obsIdx_, date_] :=
	Module[{obs, cubitLength, observedDist, referencePos, lengthStr, lengthLabel},
		obs = modelObservations[[obsIdx]];
		cubitLength = samplesCubitLength[samples];

		observedDist = QuantityMagnitude[observationCubitsSigned[obs]*cubitLength, "AngularDegrees"];
		referencePos = AstroPosition[obs["Reference"], {"Ecliptic", date, $observationLocation}];

		lengthStr = If[IntegerQ[observationCubits[obs]],
			ToString[IntegerPart@observationCubits[obs],InputForm] <> " cubits",
			ToString[IntegerPart@observationCubits[obs],InputForm] <> " " <> ToString[FractionalPart@observationCubits[obs],InputForm] <> " cubits"
		];
		lengthLabel = Style[lengthStr, FontSize -> 16];

		If[relationAxes[obs["Relation"]] === 2,
			{
				GeoPath[{referencePos, AstroPosition[referencePos["Data"]+{observedDist,0,0}, referencePos["Frame"]]}],
				Inset[lengthLabel,
					AstroPosition[referencePos["Data"]+{observedDist/2,0,0}, referencePos["Frame"]],
					{0,1.5}
				]
			},
			{
				GeoPath[{referencePos, AstroPosition[referencePos["Data"]+{0,observedDist,0}, referencePos["Frame"]]}],
				Inset[Rotate[lengthLabel, Pi/2],
					AstroPosition[referencePos["Data"]+{0,observedDist/2,0}, referencePos["Frame"]],
					{1.5,0}
				]
			}
		]
	]


objectPosition[obs_, date_?DateObjectQ] :=
	AstroPosition[obs["Object"], {"Ecliptic", date, $observationLocation}]
objectPosition[obs_, t_?NumberQ] :=
	objectPosition[obs, DateObject[obs["Date"], "Instant", TimeZone -> $timeZone] + Quantity[t, "Hours"]]

objectMovementCurve[obs_, {tmin_, tmax_}] := 
	Line@Table[objectPosition[obs, t], {t, tmin, tmax}]
objectMovementCurve[obs_] := objectMovementCurve[obs, {-100, 100}]


timeString[t_?TimeObjectQ] :=
	IntegerString[DateValue[t, "Hour24"], 10, 2] <> ":" <> IntegerString[DateValue[t, "Minute"], 10, 2]

objectGhosts[{samples_, modelObservations_}, obsIdx_, times_] :=
	Table[
		Module[{obs, pos},
			obs = modelObservations[[obsIdx]];
			pos = objectPosition[obs, t];
			{
				GeoDisk[pos, Quantity[0.25, "AngularDegrees"]],
				Inset[
					Style[timeString[TimeObject@t], Opacity[0.3], FontFamily -> "Courier New", FontSize -> 16],
					pos,
					{0, -3}
				]
			}
			],
		{t, times}
	]


Options[observationHeroPlot] =
	Join[
		Options[AstroGraphics],
		{}
	];
observationHeroPlot[sampleInfo:{samples_, modelObservations_}, obsIdx_, date_?DateObjectQ, opts:OptionsPattern[]] :=
	Module[{obs, objPos, refPos},
		obs = modelObservations[[obsIdx]];

		objPos = AstroPosition[obs["Object"], {"Ecliptic", date, $observationLocation}];
		refPos = AstroPosition[obs["Reference"], {"Ecliptic", date, $observationLocation}];

		AstroGraphics[
			{
				{White, Opacity[0.5], objectMovementCurve[modelObservations[[obsIdx]]]},
				{StandardGreen, Opacity[0.5], objectMovementCurve[modelObservations[[obsIdx]], Quantile[samples[[All,"t",obsIdx]], {0.025, 0.975}]]},
				{White, observationBand[sampleInfo, obsIdx, date]},
				{StandardRed, Dashed, Opacity[0.4], observationCenterLine[sampleInfo, obsIdx, date]},
				{StandardRed, observationDistanceLine[sampleInfo, obsIdx, date]},
				{White, objectGhosts[sampleInfo, obsIdx, Range[-24, 24, 4]]}
			},
			FilterRules[{opts}, Options[AstroGraphics]],
			AstroReferenceFrame -> {"Ecliptic", date, $observationLocation},
			(*AstroGridLines -> {"Ecliptic" -> Quantity[Mean[1/samples[[All,"l"]]], "AngularDegrees"]},*)
			AstroGridLines -> None,
			AstroBackground -> 
			AstroStyling[{
					"BlackSky",
					"ShowConstellations" -> False, 
					"ShowMainPlanes" -> False,
					"StarLabelStyle" -> Transparent, 
					"GalaxyLabelStyle" -> Transparent, 
					"OpenClusterLabelStyle" -> Transparent, 
					"GlobularClusterLabelStyle" -> Transparent, 
					"DiffuseNebulaLabelStyle" -> Transparent, 
					"PlanetaryNebulaLabelStyle" -> Transparent
				}],
			AstroCenter -> AstroPosition[Mean[{objPos["Data"], refPos["Data"]}], {"Ecliptic", date, $observationLocation}],
			AstroRange -> Quantity[2, "AngularDegrees"]
		]
	]
observationHeroPlot[sampleInfo:{samples_, modelObservations_}, obsIdx_, opts:OptionsPattern[]] :=
	Module[{date, time},
		date = DateObject[modelObservations[[obsIdx,"Date"]], "Instant", TimeZone->$timeZone];
		time = Quantity[Mean@samples[[All,"t",obsIdx]], "Hours"];
		observationHeroPlot[sampleInfo, obsIdx, date + time, opts]
	]


(* Model-free visualizations *)

(* TODO: Don't hardcode these like this *)

cubitLength = Quantity[2.42573, "AngularDegrees"];
observationVariance = Quantity[0.512, "AngularDegrees"];


Options[ADObservationPlot] =
	Join[
		Options[AstroGraphics],
		{
			"HideEarth" -> False
		}
	];
ADObservationPlot[obs_, opts:OptionsPattern[]] :=
	Module[{date, ref, observedDist, referencePos},
		date = DateObject[obs["Date"], "Instant", TimeZone->$timeZone];
		ref = {"Ecliptic", "Date" -> date, "Location" -> $observationLocation};
		
		observedDist = QuantityMagnitude[observationCubitsSigned[obs]*cubitLength, "AngularDegrees"];
		referencePos = AstroPosition[obs["Reference"], ref];

		Column[{
			ADObservationText[obs["ID"]],
			Dataset[{obs[[{"Object", "Cubits", "Fingers", "Relation", "Reference", "Time"}]]}],
			AstroGraphics[{
				{StandardRed, Arrowheads[Small], Arrow[{#Reference, #Object}]},
				StandardBlue,
				If[relationAxes[#Relation] === 2,
					With[{refPos = referencePos["Longitude"]},
						{
							eclipticLatitudeLine[refPos + observedDist, date],
							Dashed,
							eclipticLatitudeLine[refPos + observedDist - QuantityMagnitude[observationVariance, "AngularDegrees"], date],
							eclipticLatitudeLine[refPos + observedDist + QuantityMagnitude[observationVariance, "AngularDegrees"], date]
						}
					]
					,
					With[{refPos = referencePos["Latitude"]},
						{
							eclipticLongitudeLine[refPos + observedDist, date],
							Dashed,
							eclipticLongitudeLine[refPos + observedDist - QuantityMagnitude[observationVariance, "AngularDegrees"], date],
							eclipticLongitudeLine[refPos + observedDist + QuantityMagnitude[observationVariance, "AngularDegrees"], date]
						}
					]
				]
				},
				FilterRules[{opts}, Options[AstroGraphics]],
				AstroBackground -> AstroStyling[{"BlackSky", If[OptionValue["HideEarth"], "HideObjects" -> {"Earth"}, Nothing]}],
				AstroReferenceFrame -> {"Horizon", "Date" -> date, "Location" -> $observationLocation},
				AstroGridLines -> {"Ecliptic" -> cubitLength},
				AstroRange -> Quantity[15, "AngularDegrees"],
				AstroCenter -> #Object,
				ImageSize -> Large
			] &@obs
		}]
	]

End[];
EndPackage[];