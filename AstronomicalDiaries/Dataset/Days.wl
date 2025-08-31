BeginPackage["AstronomicalDiaries`Dataset`Days`"];

findTextDayPositions
nextEarlierDay

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Dataset`"]


dayPattern := dayPattern = 
	(
		optionalBracketPattern["Night of the "] | 
		optionalBracketPattern["the "]
	) ~~
	Longest[(DigitCharacter | "[" | "]") ..];

dayPatternRule := dayPatternRule =
	(
		(n : optionalBracketPattern["Night of the "]) |
		(t : optionalBracketPattern["the "])
	) ~~ 
	d : Longest[(DigitCharacter | "[" | "]") ..] :>
		{If[StringLength[n] > 0, "Night", "Day"], Interpreter["Integer"][StringDelete[d, "[" | "]"]]};

monthIntroPattern := monthIntroPattern =
	(
		(
			optionalBracketPattern@"(the 1st of which was identical with) the " ~~
			Longest[(DigitCharacter | "[" | "]") ..] ~~
			("st" | "nd" | "rd" | "th") ~~
			optionalBracketPattern@" (of the preceding month)"
		) |
		(
			optionalBracketPattern@"(of which followed the " ~~
			Longest[(DigitCharacter | "[" | "]") ..] ~~
			("st" | "nd" | "rd" | "th") ~~
			" of the preceding month)"
		)
	);


findTextDayPositions[t_] :=
	Module[{positions, timeNumbers, monthIntroPositions, dayPositions},
		positions = StringPosition[t, dayPattern, IgnoreCase -> True, Overlaps -> False];
		timeNumbers = StringCases[t, dayPatternRule, IgnoreCase -> True, Overlaps -> False];
		dayPositions = Thread[positions -> timeNumbers];
		dayPositions = DeleteCases[dayPositions, _ -> {_, Except[_Integer]}];

		monthIntroPositions = StringPosition[t, monthIntroPattern, IgnoreCase -> True, Overlaps -> False];
		If[Length[monthIntroPositions] > 0,
		dayPositions //= Discard[Between[#[[1, 1]], monthIntroPositions] || Between[#[[1, 2]], monthIntroPositions]&]
		];

		dayPositions
	]


nextEarlierDay[m_?MissingQ] := m
nextEarlierDay[{"Day", n_Integer}] := {"Night", n}
nextEarlierDay[{"Night", n_Integer}] := {"Day", n - 1}
nextEarlierDay[r_ -> d_] := r -> nextEarlierDay[d]


End[];
EndPackage[];