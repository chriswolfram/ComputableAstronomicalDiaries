BeginPackage["AstronomicalDiaries`Dataset`"];

findChunkSplits
getSameChunkAnnotations
findEarlierAnnotation
findLaterAnnotation
earlierAnnotationDamageQ
observationDamagedQ
optionalBracketPattern

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Dataset`Days`"]
Needs["AstronomicalDiaries`Dataset`Times`"]
Needs["AstronomicalDiaries`Dataset`Names`"]
Needs["AstronomicalDiaries`Chronology`"]
Needs["AstronomicalDiaries`Texts`"]

observationsDataPath := observationsDataPath = FileNameJoin[{$ADBase, "Observations", "observationsData.mx"}];

(* Parsing utilities *)

findChunkSplits[text_] := Join[{1}, StringPosition[text, "\n\n"][[All, 1]], {StringLength[text]}]

getSameChunkAnnotations[annotations_, chunkSplits_, {start_Integer, end_Integer}] :=
	Module[{chunk, range},
		chunk = Last@Select[chunkSplits, LessEqualThan[start] -> "Index"];
		range = chunkSplits[[{chunk, chunk + 1}]];
		Select[annotations, Between[Mean[#[[1]]], range] &]
	]


findEarlierAnnotation[annotations_, {start_Integer, end_Integer}] :=
	Catch@annotations[[First@Last[Position[annotations[[All, 1, 2]], _?(LessEqualThan[end]), {1}], Throw[Missing[]]]]]

findEarlierAnnotation[annotations_, ranges_List] := findEarlierAnnotation[annotations, #] & /@ ranges


findLaterAnnotation[annotations_, {start_Integer, end_Integer}] :=
	Catch[annotations[[First@FirstPosition[annotations[[All, 1, 2]], _?(GreaterThan[end]),Throw[Missing[]]]]]]

findLaterAnnotation[annotations_, ranges_List] := findLaterAnnotation[annotations, #] & /@ ranges


earlierAnnotationDamageQ[text_, {obsStart_, obsEnd_}, {annotationStart_, annotationEnd_} -> annotation_] :=
	If[annotationEnd >= obsStart,
		False,
		StringContainsQ[StringTake[text, {annotationEnd, obsStart}], "..."]
	]

observationDamagedQ[{text_, {obsStart_, obsEnd_}}] := 
	StringContainsQ[StringTake[ADText@text, {obsStart, obsEnd}], "..."]

optionalBracketPattern[str_] := StringExpression @@ Riffle[Characters[str], ("[" | "]") ...]


(* Line numbers *)

getPositionLineInteger[td_, p_] := StringCount[StringTake[removeLineNumbers@td[[1]], p], "\n"] + 1

getPositionLineNumber[td_, p_] :=
	ReplaceRepeated[
		StringCases[
			StringSplit[td[[1]], "\n"],
			StartOfString ~~ Longest[line___] ~~ "\t" :> line
		],
		{a___, b_, {}, d___} :> {a, b, b, d}
	][[getPositionLineInteger[td, p], 1]]


(* Numbers *)

parseMixedFraction[str_?StringQ] := parseMixedFraction[str] =
	First[StringCases[StringDelete[str, "?"], {

		StartOfString ~~ i : DigitCharacter .. ~~ EndOfString :>
			ToExpression[i],

		StartOfString ~~ n : DigitCharacter .. ~~ ("/" | "\\/") ~~ d : DigitCharacter .. ~~ EndOfString :>
			ToExpression[n]/ToExpression[d],

		StartOfString ~~ i : DigitCharacter .. ~~ Whitespace ~~ n : DigitCharacter .. ~~ ("/" | "\\/") ~~ d : DigitCharacter .. ~~ EndOfString :>
			ToExpression[i] + ToExpression[n]/ToExpression[d]

	}], Missing[]]

parseMixedFraction[_] := Missing[]


Options[ADObservations] = {
	"Chronology" -> Automatic
};

ADObservations[observationParses_, OptionsPattern[]] :=
	Module[{hash, observationsData, observationDayRanges, observationTimes, lineMonths, observations},
		
		hash = Hash[observationParses];

		If[FileExistsQ[observationsDataPath],
			observationsData = Import@observationsDataPath;
			If[observationsData["Hash"] =!= hash,
				Return@Failure["ChecksumFailure", <|
						"MessageTemplate" -> "The inputted observation parses do not match those used to generate the cached observations.\
						Delete the cached files under `1` and regenerate observation parses to continue.",
						"MessageParameters" -> {observationsDataPath}
					|>]
			];
			Return@observationsData["Observations"]
		];
		
		observationDayRanges =
			Join @@ KeyValueMap[
				Function[{textID, obsIDs},
					AssociationThread[obsIDs, findObservationDayRanges[ADText[textID], obsIDs[[All, 2]]]]
				],
				GroupBy[Keys[observationParses], First]
			];

		observationTimes =
			Join @@ KeyValueMap[
				Function[{textID, obsIDs},
					AssociationThread[obsIDs, findObservationTimes[ADText[textID], obsIDs[[All, 2]]]]
				],
				GroupBy[Keys[observationParses], First]
			];

		lineMonths = ADLineMonths[];

		observations = KeyValueMap[
			Module[{obsID, line, earliestDay, latestDay, day, time, timeRange, rawMonthYear, month, rawRegnalYear, regnalYear, seYear, julianDate},
				obsID = #1;
				Function[
					line = getPositionLineNumber[ADTextData[obsID[[1]]], obsID[[2, 1]]];

					earliestDay = observationDayRanges[obsID][[1, 2]];
					latestDay = observationDayRanges[obsID][[2, 2]];
					day = If[earliestDay === latestDay, earliestDay, Missing[]];

					{time, timeRange} =
						Replace[observationTimes[obsID], {
								_Missing -> {Missing[], Missing[]},
								(range_ -> val_) :> {val, range}
							}];

					rawMonthYear = Lookup[Lookup[lineMonths, obsID[[1]], <||>], line,
							<|"Year" -> Missing[], "Month" -> Missing[]|>
						];
					month = rawMonthYear["Month"];
					rawRegnalYear = rawMonthYear["Year"];
					regnalYear = parseRegnalYear[rawRegnalYear];
					seYear = regnalToSE[regnalYear];
					julianDate = ADFromBabylonianDate[{seYear, month, day}];

					<|
						"ID" -> obsID,
						"Object" -> Lookup[objectNames, #object, Missing[]],
						"Relation" -> Lookup[relationNames, #relation, Missing[]],
						"Reference" -> Lookup[referenceNames, #reference, Missing[]],
						"Cubits" -> parseMixedFraction[#cubits],
						"Fingers" -> parseMixedFraction[#fingers],
						"Time" -> time,
						"Day" -> day,
						"EarliestDay" -> earliestDay,
						"LatestDay" -> latestDay,
						"Month" -> month,
						"RegnalYear" -> regnalYear,
						"SEYear" -> seYear,
						"Date" -> julianDate,
						"EarliestDate" -> ADFromBabylonianDate[{seYear, month, earliestDay}],
						"LatestDate" -> ADFromBabylonianDate[{seYear, month, latestDay}],
						"Damaged" -> observationDamagedQ[obsID],
						(*"String"->getObservationText[obsID],*)
						"Line" -> line,
						"TextID" -> obsID[[1]],
						"Range" -> obsID[[2]],
						"TimeRange" -> timeRange,
						"EarliestDayRange" -> observationDayRanges[obsID][[1, 1]],
						"LatestDayRange" -> observationDayRanges[obsID][[2, 1]]
					|>
				]@#2
			]&,
			observationParses
		];

		Export[observationsDataPath, <|"Hash" -> hash, "Observations" -> observations|>];

		observations
	]

ADObservations[] := ADObservations[ADObservationParses[]]


End[];
EndPackage[];