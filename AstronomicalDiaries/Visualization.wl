BeginPackage["AstronomicalDiaries`Visualization`"];

eclipticLatitudeLine
eclipticLongitudeLine

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Astronomy`"]

eclipticLatitudeLine[long_, date_] :=
	Line[AstroPosition[Table[{long, elat}, {elat, -90, 90}], {"Ecliptic", "Date" -> date, "Location" -> $observationLocation}]]

eclipticLongitudeLine[lat_, date_] :=
	Line[AstroPosition[Table[{elon, lat}, {elon, -180, 180}], {"Ecliptic", "Date" -> date, "Location" -> $observationLocation}]]


(* TODO: Don't hardcode these like this *)

cubitLength = Quantity[2.4, "AngularDegrees"];
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