BeginPackage["AstronomicalDiaries`Dataset`Times`"];

findTextTimePositions

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Dataset`"]


timeSpecs =
	<|
		"beginning of the night" -> "BeginningOfTheNight",
		"beginning part of the night" -> "BeginningOfTheNight",
		"first part of the night" -> "FirstPartOfTheNight",
		"middle part of the night" -> "MiddlePartOfTheNight",
		"middle of the night" -> "MiddlePartOfTheNight",
		"last part of the night" -> "LastPartOfTheNight"
	|>;

timePattern := timePattern = Alternatives @@ (optionalBracketPattern /@ Keys[timeSpecs]);
timePatternRule := timePatternRule = o : timePattern :> timeSpecs[ToLowerCase@StringDelete[o, "[" | "]"]];


findTextTimePositions[t_] :=
	Module[{positions, times},
		positions = StringPosition[t, timePattern, IgnoreCase -> True, Overlaps -> False];
		times = StringCases[t, timePatternRule, IgnoreCase -> True, Overlaps -> False];

		Thread[positions -> times]
	]

End[];
EndPackage[];