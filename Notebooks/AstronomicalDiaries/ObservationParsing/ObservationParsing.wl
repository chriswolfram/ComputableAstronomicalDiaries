BeginPackage["AstronomicalDiaries`ObservationParsing`"];

formatObservation

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`ObservationParsing`Prompt`"]
Needs["AstronomicalDiaries`Texts`"]
Needs["AstronomicalDiaries`OpenAIUtilities`"]
Needs["AstronomicalDiaries`FuzzyAlignment`"]
Needs["AstronomicalDiaries`Utilities`"]

obsParsePath := obsParsePath = FileNameJoin[{$ADBase, "ObservationParsing"}];
batchDataPath := batchDataPath = FileNameJoin[{obsParsePath, "batchData.mx"}];
batchResponsePath := batchResponsePath = FileNameJoin[{obsParsePath, "batchResponse.mx"}];
observationParsesPath := observationParsesPath = FileNameJoin[{obsParsePath, "observationParses.mx"}]

(* Formatting requests *)

schema := schema =
	<|
		"name" -> "observation_parsing",
		"schema" -> <|
				"type" -> "object",
				"properties" -> <|
						"object" -> <|"type" -> {"string", "null"}|>,
						"cubits" -> <|"type" -> {"string", "null"}|>,
						"fingers" -> <|"type" -> {"string", "null"}|>,
						"relation" -> <|
								"type" -> {"string", "null"},
								"enum" -> {"in front of", "behind", "above", "below", "north", "south", "east", "west", Null}
							|>,
						"reference" -> <|"type" -> {"string", "null"}|>
					|>,
				"required" -> {"object", "cubits", "fingers", "relation", "reference"},
				"additionalProperties" -> False
			|>,
		"strict" -> True
	|>;


formatObservationContext[t0_, obsRange_, r_, {delimLeft_, delimRight_}] :=
	Module[{t = t0},
		t = StringInsert[StringInsert[t, delimRight, obsRange[[2]] + 1], delimLeft, obsRange[[1]]];
		StringTake[t, {
				Max[obsRange[[1]] - r, 1],
				Min[obsRange[[2]] + StringLength@delimLeft + StringLength@delimRight + r, StringLength[t]]
			}]
	]

formatObservationContext[tSpec_, obsRange_, r_] := formatObservationContext[tSpec, obsRange, r, {"{{", "}}"}]
formatObservationContext[tSpec_, obsRange_] := formatObservationContext[tSpec, obsRange, 100]


formatObservation[t_, obsRange_] :=
	StringTemplate["\
### Text
`Text`

### Context
`Context`\
"][<|
	"Text" -> StringTake[t, obsRange],
	"Context" -> formatObservationContext[t, obsRange]
|>]


request[t_, obsRange_] :=
	<|
		"model" -> "gpt-5-mini",
		"instructions" -> prompt,
		"text" -> <|
		"format" -> Append[schema, "type" -> "json_schema"], "verbosity" -> "medium"|>,
		"reasoning" -> <|"effort" -> "minimal", "summary" -> "detailed"|>,
		"input" -> formatObservation[t, obsRange]
	|>

(* request[t_, obsRange_] :=
	<|
		"model" -> "gpt-5-mini",
		"messages" -> {
				<|"role" -> "developer", "content" -> prompt|>,
				<|"role" -> "user", "content" -> formatObservation[t, obsRange]|>
			},
		"response_format" -> <|"type" -> "json_schema", "json_schema" -> schema|>,
		"reasoning_effort" -> "minimal",
		"n" -> 8
	|> *)


(* Submitting batches *)

batchCreate[observationIDs_] :=
	OpenAIBatchCreate@AssociationMap[
		Apply[request[ADText[#1], #2]&],
		observationIDs
	]

batchResponseLoad[batchResponse_] := importUTF8JSON & /@ batchResponse["Outputs"]


observationParses = Missing[];

ADObservationParses[observationIDs_] :=
	Module[{hash, batchData, batch, batchResponse, observationParses},

		hash = Hash[observationIDs];

		(* Create batch *)
		If[!FileExistsQ[batchDataPath],
			batch = batchCreate[observationIDs];
			Export[batchDataPath, <|"Hash" -> hash, "Batch" -> batch|>]
			,
			batchData := batchData = Import@batchDataPath;
			If[batchData["Hash"] =!= hash,
					Return@Failure["ChecksumFailure", <|
						"MessageTemplate" -> "The inputted observation IDs do not match those used to generate the cached version of\
the observation parses. Delete the cached files under `1` and regeneration observation parses to continue.",
						"MessageParameters" -> {obsParsePath}
					|>]
				];
			batch = batchData["Batch"]
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
		If[!FileExistsQ[observationParsesPath],
			observationParses = batchResponseLoad@batchResponse;
			Export[observationParsesPath, observationParses];
			,
			observationParses = Import@observationParsesPath
		];

		observationParses
	]

ADObservationParses[] := ADObservationParses@ADObservationIDs[]


End[];

EndPackage[];