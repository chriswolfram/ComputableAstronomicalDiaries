BeginPackage["AstronomicalDiaries`Dataset`Days`"];

findObservationDayRanges

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
nextEarlierDay[r_ -> {"Night", n_Integer}] := r -> {"Day", n - 1}
nextEarlierDay[r_ -> {"Day", n_Integer}] := r -> {"Night", n}


findObservationDayRanges[text_, observationRanges_] :=
	Module[{dayPositions, chunkSplits},
		dayPositions = findTextDayPositions[text];
		chunkSplits = findChunkSplits[text];
		Function[obsRange,
			Module[{sameChunkAnnotations, earlierDay, laterDay},
				sameChunkAnnotations = getSameChunkAnnotations[dayPositions, chunkSplits, obsRange];
				earlierDay = findEarlierAnnotation[sameChunkAnnotations, obsRange];

				If[MissingQ[earlierDay] || earlierAnnotationDamageQ[text, obsRange, earlierDay],
					laterDay = nextEarlierDay@findLaterAnnotation[sameChunkAnnotations, obsRange],
					laterDay = earlierDay
				];

				earlierDay //= Replace[(r_ -> {t_, d_}) :> (r -> d)];
				laterDay //= Replace[(r_ -> {t_, d_}) :> (r -> d)];
				{
					Replace[earlierDay, _Missing -> (Missing[] -> 1)],
					Replace[laterDay, _Missing -> (Missing[] -> 30)]
				}
			]
		] /@ observationRanges
	]


End[];
EndPackage[];