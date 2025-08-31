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

findChunkSplits[text_] := Join[{0}, StringPosition[text, "\n\n"][[All, 1]], {StringLength[text]}]

findTextDamagePositions[text_] := StringPosition[text, "..."][[All,2]]

observationDamagedQ[{text_, {obsStart_, obsEnd_}}] := 
	StringContainsQ[StringTake[ADText@text, {obsStart, obsEnd}], "..."]

optionalBracketPattern[str_] := StringExpression @@ Riffle[Characters[str], ("[" | "]") ...]


(* Days and times *)

findObservationDayTimes[text_, obsList_] :=
	Module[{chunkSplits, damagePositions, dayPositions, timePositions, lastDamage, lastChunk, nextChunk, lastBreak, time, day, earlierDay, laterDay},
		chunkSplits = findChunkSplits[text];
		damagePositions = findTextDamagePositions[text];
		dayPositions = findTextDayPositions[text];
		timePositions = findTextTimePositions[text];
		
		Function[{obsStart, obsEnd},
			lastDamage = Max[0,Select[damagePositions, # <= obsStart&]];
			(* TODO: Should this use month breaks instead of chunk breaks? *)
			lastChunk = Max[0,Select[chunkSplits, # <= obsStart&]];
			lastBreak = Max[lastDamage, lastChunk];

			nextChunk = Min[StringLength[text],Select[chunkSplits, obsEnd <= #&]];
			
			time = Last[Select[timePositions,
				MatchQ[({start_, end_} -> time_) /; lastBreak <= end && start <= obsEnd]
			], Missing[]->Missing[]];
			
			day = Select[dayPositions,
				MatchQ[({start_, end_} -> day_) /; lastBreak <= end && start <= obsEnd]
			];
			
			If[day =!= {},
				(* If there is a day marker that is not seperated by damage from the observation, that is the earliest and latest day *)
				earlierDay = laterDay = Last[day]
				,
				(* Otherwise, the earliest day is the last one before the observation (within the same chunk) *)
				earlierDay = Last[
						Select[dayPositions, MatchQ[({start_, end_} -> day_) /; start <= obsEnd && lastChunk <= start <= nextChunk]],
						Missing[] -> {"Night", 1}
					];
				(* and the latest day is the first one after the observation int he same chunk, so long as it is not earlier than earlierDay *)
				laterDay = nextEarlierDay@First[
						Select[dayPositions, MatchQ[({start_, end_} -> day_) /; obsStart <= end && lastChunk <= start <= nextChunk && earlierDay[[2,2]] <= nextEarlierDay[day][[2]]]],
						Missing[] -> {"Day", 30}
					];
			];
			
			<|"Time" -> time, "DayRange" -> {earlierDay, laterDay}|>
		] @@@ obsList
	]


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
	Module[{hash, observationsData, observationDayTimes, lineMonths, observations},
		
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
		
		observationDayTimes =
			Join @@ KeyValueMap[
				Function[{textID, obsIDs},
					AssociationThread[obsIDs, findObservationDayTimes[ADText[textID], obsIDs[[All, 2]]]]
				],
				GroupBy[Keys[observationParses], First]
			];

		lineMonths = ADLineMonths[];

		observations = KeyValueMap[
			Module[{obsID, line, dayTimeData, earliestDay, latestDay, day, time, timeRange, rawMonthYear, month, rawRegnalYear, regnalYear, seYear, julianDate},
				obsID = #1;
				Function[
					line = getPositionLineNumber[ADTextData[obsID[[1]]], obsID[[2, 1]]];

					dayTimeData = observationDayTimes[obsID];
					{earliestDay, latestDay} = dayTimeData["DayRange"][[All, 2, 2]];
					day = If[earliestDay === latestDay, earliestDay, Missing[]];

					{time, timeRange} = Replace[dayTimeData["Time"], (range_ -> val_) :> {val, range}];

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
						"EarliestDayRange" -> dayTimeData[["DayRange", 1, 1]],
						"LatestDayRange" -> dayTimeData[["DayRange", 2, 1]]
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