BeginPackage["AstronomicalDiaries`Astronomy`"];

$timeZone
$observationLocation
relationAxes
relationSigns
fingersPerCubit
observationCubits
observationCubitsSigned
objectDisplacement
observationDistance

Begin["`Private`"];

Needs["AstronomicalDiaries`"]

$timeZone = 3;

(* The coordinates of Babylon (specifically Esagila temple) *)
$observationLocation = GeoPosition[{32.533806, 44.421494}];

(* For interpreting observations *)

(* Whether each relation corresponds to the first or second axis of the 2D displacement *)
relationAxes =
	<|
		"Above" -> 1,
		"Below" -> 1,
		"InFrontOf" -> 2,
		"Behind" -> 2,
		"North" -> 1,
		"South" -> 1,
		"East" -> 2,
		"West" -> 2
	|>;

(* Whether each relation corresponds to an increase or decrease in its axis *)
relationSigns =
	<|
		"Above" -> 1,
		"Below" -> -1,
		"InFrontOf" -> -1,
		"Behind" -> 1,
		"North" -> 1,
		"South" -> -1,
		(*TODO: Confirm this is the direction for east-west*)
		"East" -> -1,
		"West" -> 1
	|>;


fingersPerCubit = 24;

observationCubits[obs_] :=
	If[MissingQ[obs["Cubits"]] && MissingQ[obs["Fingers"]],
		Missing[],
		Replace[obs["Cubits"], _Missing -> 0] + Replace[obs["Fingers"], _Missing -> 0] / fingersPerCubit
	]

observationCubitsSigned[obs_] :=
	If[MissingQ[obs["Relation"]] || MissingQ[observationCubits[obs]],
		Missing[],
		relationSigns[obs["Relation"]] * observationCubits[obs]
	]


(* For computing true positions *)

objectDisplacement[obj1_, obj2_, date_, opts___] :=
	If[MissingQ[obj1] || MissingQ[obj2] || MissingQ[date], Missing[],
		Mod[
			AstroPosition[obj1, {"Ecliptic", opts, "Date" -> date, "Location" -> $observationLocation}][{"Latitude", "Longitude"}] - 
			AstroPosition[obj2, {"Ecliptic", opts, "Date" -> date, "Location" -> $observationLocation}][{"Latitude", "Longitude"}],
			{360, 180},
			{-180, -90}
		]
	]

observationDistance[obs_, t_:0, opts___] :=
	If[MissingQ[#Relation] || MissingQ[#Date], Missing[],
		With[{disp = objectDisplacement[#Object, #Reference, DateObject[#Date, "Instant", TimeZone -> $timeZone] + t, opts]},
			If[MissingQ[disp], disp, disp[[relationAxes[#Relation]]]]
		]
	]& @ obs


End[];
EndPackage[];