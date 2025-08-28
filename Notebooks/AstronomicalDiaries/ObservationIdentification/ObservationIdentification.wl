BeginPackage["AstronomicalDiaries`ObservationIdentification`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`ObservationIdentification`Prompt`"]
Needs["AstronomicalDiaries`Texts`"]
Needs["AstronomicalDiaries`OpenAIUtilities`"]
Needs["AstronomicalDiaries`FuzzyAlignment`"]
Needs["AstronomicalDiaries`Utilities`"]

Begin["`Private`"];

obsIDPath := obsIDPath = FileNameJoin[{$ADBase, "ObservationIdentification"}];
batchPath := batchPath = FileNameJoin[{obsIDPath, "batch.mx"}];
batchResponsePath := batchResponsePath = FileNameJoin[{obsIDPath, "batchResponse.mx"}];
observationIDsPath := observationIDsPath = FileNameJoin[{obsIDPath, "observationIDs.mx"}]

(* Formatting requests *)

schema = <|
		"name" -> "observation_extraction",
		"schema" -> <|
				"type" -> "object",
				"properties" -> <|"observations" -> <|"type" -> "array", "items" -> <|"type" -> "string"|>|>|>,
				"required" -> {"observations"},
				"additionalProperties" -> False
			|>,
		"strict" -> True
	|>;


request[text_] :=
	<|
		"model" -> "gpt-5",
		"instructions" -> prompt,
		"text" -> <|
				"format" -> Append[schema, "type" -> "json_schema"],
				"verbosity" -> "high"
			|>,
		"reasoning" -> <|"effort" -> "minimal", "summary" -> "detailed"|>,
		"input" -> text
	|>


(* Submitting batches *)

batchCreate[] := OpenAIBatchCreate@Discard[AssociationMap[request@*ADText, allChunks], #input === "" &]

batchResponseLoad[batchResponse_] :=
	Module[{chunkObservationStrings, chunkObservationPositions, textObservationPositions},

		chunkObservationStrings = importUTF8JSON[#]["observations"] & /@ batchResponse["Outputs"];

		chunkObservationPositions =
			MapIndexed[
				FuzzyAlignments[ADText[#2[[1, 1]]], #1]&,
				chunkObservationStrings
			];

		textObservationPositions = Join[
			AssociationMap[{} &, Keys@ADTextData[]],
			Merge[KeyValueMap[First[#1] -> chunkOffsets[#1] + #2 &, chunkObservationPositions], Catenate]
		];

		Catenate@KeyValueMap[Function[{text, spans}, {text, #} & /@ spans], textObservationPositions]
	]


observationIDs = Missing[];

ADObservationIDs[] :=
	Module[{batch, batchResponse},

		 (* Load if already cached *)
		 If[!MissingQ[observationIDs], Return@observationIDs];

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
		If[!FileExistsQ[observationIDsPath],
			observationIDs = batchResponseLoad@batchResponse;
			Export[observationIDsPath, observationIDs];
			,
			observationIDs = Import@observationIDsPath
		];

		observationIDs
	]


End[];

EndPackage[];