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
textObservationPositionsPath := textObservationPositionsPath = FileNameJoin[{obsIDPath, "textObservationPositions.mx"}]

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
	Module[{chunkObservationStrings, chunkObservationPositions},

		chunkObservationStrings = importUTF8JSON[#]["observations"] & /@ batchResponse["Outputs"];

		chunkObservationPositions =
			MapIndexed[
				FuzzyAlignments[getText[#2[[1, 1]]], #1]&,
				chunkObservationStrings
			];

		Join[
			AssociationMap[{} &, allTexts],
			Merge[KeyValueMap[First[#1] -> chunkOffsets[#1] + #2 &, chunkObservationPositions], Catenate]
		]
	]


textObservationPositions = Missing[];

ADTextObservationPositions[] :=
	Module[{batch, batchResponse},

		 (* Load if already cached *)
		 If[!MissingQ[textObservationPositions], Return@textObservationPositions];

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
		If[!FileExistsQ[textObservationPositionsPath],
			textObservationPositions = batchResponseLoad@batchResponse;
			Export[textObservationPositionsPath, textObservationPositions];
			,
			textObservationPositions = Import@textObservationPositionsPath
		];

		textObservationPositions
	]


End[];

EndPackage[];