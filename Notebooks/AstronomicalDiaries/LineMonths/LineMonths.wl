BeginPackage["AstronomicalDiaries`LineMonths`"];

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`OpenAIUtilities`"]
Needs["AstronomicalDiaries`Utilities`"]
Needs["AstronomicalDiaries`Texts`"]
Needs["AstronomicalDiaries`LineMonths`Prompt`"]

lineMonthsPath := lineMonthsPath = FileNameJoin[{$ADBase, "LineMonths"}];
batchPath := batchPath = FileNameJoin[{lineMonthsPath, "batch.mx"}];
batchResponsePath := batchResponsePath = FileNameJoin[{lineMonthsPath, "batchResponse.mx"}];
rawLineMonthsDataPath := rawLineMonthsDataPath = FileNameJoin[{lineMonthsPath, "rawLineMonthsData.mx"}];
hardcodedPath := hardcodedPath = FileNameJoin[{lineMonthsPath, "Hardcoded"}];

(* Forming requests *)

schema[td_] :=
	<|
		"name" -> "line_month_extraction",
		"schema" -> <|
				"type" -> "object",
				"properties" -> <|
						"explanation" -> <|"type" -> "string"|>,
						"results" -> <|
								"type" -> "array",
								"items" -> <|
										"type" -> "object",
										"properties" -> <|
												"line" -> <|"type" -> "string", 
												"enum" -> getTextLineNumbers[td]|>,
												"month" -> <|
														"type" -> {"string", "null"},
														"enum" -> {
																"I", "II", "III", "IV", "V", "VI", "VI2", "VII", 
																"VIII", "IX", "X", "XI", "XII", "XII2", Null
															}
													|>,
												"year" -> <|"type" -> {"string", "null"}|>
											|>,
										"required" -> {"line", "month", "year"},
										"additionalProperties" -> False
									|>
							|>
					|>,
				"required" -> {"explanation", "results"},
				"additionalProperties" -> False
			|>,
		"strict" -> True
	|>


formatInput[td_] := 
	"Month Guide: " <> td[[2, "months_recorded"]] <> "
	Year Guide: " <> td[[2, "ancient_year"]] <> "
	Text:\n" <> td[[1]]


request[td_] :=
	<|
		"model" -> "gpt-5",
		"instructions" -> prompt,
		"text" -> <|
				"format" -> Append[schema[td], "type" -> "json_schema"],
				"verbosity" -> "high"
			|>,
		"reasoning" -> <|"effort" -> "minimal", "summary" -> "detailed"|>,
		"input" -> formatInput[td]
	|>


(* Hardcoding *)

toHardcodedString[lineMonths_] :=
	StringRiffle[
		KeyValueMap[StringRiffle[{#1, #2["Year"], #2["Month"]} /. _Missing->"-", "\t"]&, lineMonths],
		"\n"
	]

fromHardcodedString[str_] :=
	GroupBy[
		StringSplit[StringSplit[str, "\n"], "\t"] /. "-" -> Missing[],
		First -> Function[<|"Month" -> #[[3]], "Year" -> #[[2]]|>],
		First
	]

importHardcodedLineMonths[] :=
	With[{paths = FileNames["*.txt", hardcodedPath]},
		AssociationThread[
			FileBaseName /@ paths,
			fromHardcodedString@*ReadString /@ paths
		]
	]

ADLineMonthHardcode[textid_, lineMonths_] :=
	Export[
		FileNameJoin[{hardcodedPath, textid<>".txt"}],
		toHardcodedString[lineMonths],
		OverwriteTarget -> False
	]

ADLineMonthHardcode[textid_] := ADLineMonthHardcode[textid, ADLineMonths["LineMonths"][textid]]


(* Submitting batches *)

batchCreate[] := OpenAIBatchCreate@AssociationMap[request@*ADTextData, Keys@ADTextData[]]

batchResponseLoad[batchResponse_] :=
	Module[{rawResponses, explanations, lineMonths},
		rawResponses = importUTF8JSON /@ batchResponse["Outputs"];
		explanations = rawResponses[[All, "explanation"]];
		lineMonths =
			GroupBy[
				#results,
				Lookup["line"] -> Function[<|"Month" -> #month, "Year" -> #year|> /. Null -> Missing[]],
				First
			] & /@ rawResponses;
		
		<|"Explanations" -> explanations, "RawLineMonths" -> lineMonths|>
	]


lineMonths = Missing[];

ADLineMonths[All] :=
	Module[{batch, batchResponse, rawLineMonthsData, hardcodedLineMonths},

		 (* Load if already cached *)
		 If[!MissingQ[lineMonths], Return@lineMonths];

		(* Create batch *)
		If[!FileExistsQ[batchPath],
			batch = batchCreate[];
			Export[batchPath, batch]
			,
			batch := batch = Import@batchPath
		];

		(* Download response *)
		If[!FileExistsQ[batchResponsePath],
			OpenAIBatchWait[batch];
			batchResponse = OpenAIBatchRetrieve@OpenAIBatchStatusRetrieve[batch];
			Export[batchResponsePath, batchResponse]
			,
			batchResponse := batchResponse = Import@batchResponsePath
		];

		(* Process response *)
		If[!FileExistsQ[rawLineMonthsDataPath],
			rawLineMonthsData = batchResponseLoad@batchResponse;
			Export[rawLineMonthsDataPath, rawLineMonthsData];
			,
			rawLineMonthsData = Import@rawLineMonthsDataPath
		];

		hardcodedLineMonths = importHardcodedLineMonths[];

		lineMonths = <|
			rawLineMonthsData,
			"HardcodedLineMonths" -> hardcodedLineMonths,
			"LineMonths" -> Join[rawLineMonthsData["RawLineMonths"], hardcodedLineMonths]
		|>
	]

ADLineMonths[key_] := ADLineMonths[All][key]
ADLineMonths[] := ADLineMonths["LineMonths"]
ADLineMonths["Reload"] := (lineMonths = Missing[]; ADLineMonths[])


End[];
EndPackage[];