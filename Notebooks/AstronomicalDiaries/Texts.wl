BeginPackage["AstronomicalDiaries`Texts`"];

oraccTextURL
removeLineNumbers
getTextLineNumbers
chunkOffsets
allChunks

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Utilities`"]

PacletInstall["ChristopherWolfram/CuneiformTools"];
Needs["ChristopherWolfram`CuneiformTools`"]

textsPath := FileNameJoin[{$ADBase, "InputTexts"}]
rawTabletTextDataPath := FileNameJoin[{textsPath, "rawTabletTextData.mx"}]
rawOraccPath := FileNameJoin[{textsPath, "rawOracc.txt"}]
revisedOraccPath := FileNameJoin[{textsPath, "revisedOracc.txt"}]
tabletTextDataPath := FileNameJoin[{textsPath, "tabletTextData.mx"}]

$OraccBase = "oracc.museum.upenn.edu";
oraccProjects = {{"adsd", "adart1"}, {"adsd", "adart2"}, {"adsd", "adart3"}};

getProjectTexts[proj_] :=
	Select[
		With[{catalog=OraccData[proj]},
			GeneralUtilities`MonitoredMap[{OraccData[proj, #"id_text", "TranslationWithLineNumbers"], #}&, catalog]
		],
		!StringMatchQ[#[[1]],WhitespaceCharacter...]&
	]


getRawTabletData[] :=
	Module[{rawTabletTextData},
		If[FileExistsQ[rawTabletTextDataPath],
			Return@Import[rawTabletTextDataPath]
		];
		rawTabletTextData = Merge[getProjectTexts /@ oraccProjects, Last];
		Export[rawTabletTextDataPath, rawTabletTextData];
		rawTabletTextData
	]

exportRawOracc[] :=
	Module[{rawTabletTextData},
		If[!FileExistsQ[rawOraccPath],
			rawTabletTextData = getRawTabletData[];
			Export[
				rawOraccPath,
				StringRiffle[
					"## START NEW TEXT\n" <> "START TEXT ID\n" <> #2["id_text"] <> 
					"\nEND TEXT ID\n\nSTART TEXT METADATA\n" <> 
					FromCharacterCode[ToCharacterCode[ExportString[#2, "JSON"]], 
					"UTF-8"] <> "\nEND TEXT METADATA\n\nSTART TEXT\n" <> #1 <> 
					"\nEND TEXT" & @@@ Values[rawTabletTextData],
					"\n\n\n"
				]
			]
		]
	]

ADRevisedOraccExport[] :=
	(
		If[!FileExistsQ[revisedOraccPath],
			exportRawOracc[];
			CopyFile[rawOraccPath, revisedOraccPath]
		];
		revisedOraccPath
	)


ADRevisedOraccImport[] :=
	Module[{tabletTextDataAssoc, rawStr, rawTexts, tabletTextData},

		If[!FileExistsQ[revisedOraccPath],
			Return@Failure["NoFile", <|"Message" -> "Revised Oracc file does not exist.", "Path" -> revisedOraccPath|>]
		];

		If[FileExistsQ[tabletTextDataPath],
			tabletTextDataAssoc = Import[tabletTextDataPath];
			If[FileHash[revisedOraccPath] === tabletTextDataAssoc["Hash"],
				Return@tabletTextDataAssoc["Data"]
			]
		];

		rawStr = Import[revisedOraccPath];
		rawTexts = StringSplit[rawStr, "## START NEW TEXT\n"];
		tabletTextData = Association@Map[
			t |-> Module[{metadataStr, metadata, text},
					id = StringCases[t, "START TEXT ID\n" ~~ id___ ~~ "\nEND TEXT ID" :> id][[1]];
					metadataStr = StringCases[t, "START TEXT METADATA\n" ~~ md___ ~~ "\nEND TEXT METADATA" :> md][[1]];
					metadata = ImportString[FromCharacterCode@ToCharacterCode[metadataStr, "UTF8"], "RawJSON"];
					text = StringCases[t, "START TEXT\n" ~~ md___ ~~ "\nEND TEXT" :> md][[1]];
					(*The text contain non-breaking spaces in their line numbers that are not always tokenized
					correctly by the LLM. This just replaces them with regular spaces.*)
					text //= StringReplace[" " -> " "];
					(*Texts like X301332 contain extra line numbers that create empty lines. This removes those.*)
					text //= StringDelete[StartOfLine ~~ Except["\n"] ... ~~ "\t" ~~ "\n"];id -> {text, metadata}
				],
			rawTexts
		];

		Export[tabletTextDataPath, <|"Data" -> tabletTextData, "Hash" -> FileHash[revisedOraccPath]|>];

		tabletTextData
	]


(* Chunking *)

chunkDelimiter = "\n\n";

chunkTabletData[t_, delim_] := {#, t[[2]]} & /@ StringSplit[t[[1]], "\n\n"]

chunkTabletData[texts_?AssociationQ, delim_] :=
	Association@Catenate@KeyValueMap[
		{id, t} |-> MapIndexed[{id, #2[[1]]} -> #1 &, chunkTabletData[t, delim]],
		texts
	]

chunkTabletData[t_] := chunkTabletData[t, chunkDelimiter]

allChunks := allChunks = Keys@chunkTabletData@ADTextData[];

chunkOffsets := chunkOffsets =
	Module[{textChunkLengths, textChunkOffsets},
		textChunkLengths = StringLength[GroupBy[allChunks, First -> getText]] + StringLength[chunkDelimiter];
		textChunkOffsets = Prepend[0]@*Accumulate@*Most /@ textChunkLengths;
		Association@Catenate@MapIndexed[{#2[[1, 1]], #2[[2]]} -> #1 &, textChunkOffsets, {2}]
	];


(* Utilities *)

oraccTextURL[text_] := URLBuild[{$OraccBase, "adsd", text}]

removeLineNumbers[text_] := StringDelete[text, StartOfLine ~~ Shortest[Except["\n"] ...] ~~ "\t"]

getTextLineNumbers[td_] := StringCases[td[[1]], StartOfLine ~~ lineno : Shortest[Except["\n"] ...] ~~ "\t" :> lineno]

tabletTextData = Missing[];
ADTextData[] :=
	If[AssociationQ[tabletTextData],
		tabletTextData,
		tabletTextData = ADRevisedOraccImport[]
	]

ADTextData[tabletID_?StringQ] := ADTextData[][tabletID]
ADTextData[{tabletID_, i_}] := chunkTabletData[ADTextData[tabletID]][[i]]

ADReloadTextData[] := (tabletTextData = Missing[];)

ADText[textSpec_] := removeLineNumbers[ADTextData[textSpec][[1]]]

ADObservationText[{textSpec_, range : {min_Integer, max_Integer}}] := StringTake[ADText[textSpec], range]
ADObservationText[l_List] := ADObservationText /@ l


End[];
EndPackage[];