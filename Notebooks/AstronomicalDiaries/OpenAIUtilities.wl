BeginPackage["AstronomicalDiaries`OpenAIUtilities`"];

(*
OpenAIRequest
OpenAIFile
OpenAIFileUpload
OpenAIDownloadFile
*)

OpenAIBatch
OpenAIBatchCreate
OpenAIBatchStatusRetrieve
OpenAIBatchWait
OpenAIBatchRetrieve
OpenAIBatchResponse

OpenAIChatComplete
OpenAIResponse

Begin["`Private`"];

Needs["AstronomicalDiaries`"]


(* General API calls *)

openaiAPIKey := openaiAPIKey = SystemCredential["OPENAI_API_KEY"];

openaiRequestObject[params_] :=
	HTTPRequest[<|
		"Scheme" -> "https",
		"Domain" -> "api.openai.com",
		"Headers" -> {"Authorization" ->
		"Bearer " <> SystemCredential["OPENAI_API_KEY"]},
		params
	|>]

OpenAIRequest[params_]:=
	With[{resp = URLRead[openaiRequestObject[params]]},
		Which[
			FailureQ[resp], resp,

			resp["StatusCode"] =!= 200,
			Failure["HTTPError", <|
				"MessageTemplate" -> "HTTP error: `1`",
				"MessageParameters" -> {resp["StatusCode"]},
				"HTTPResponse" -> resp|>
			],

			True, ImportByteArray[resp["BodyByteArray"], "RawJSON"]
		]
	]


(* File uploading/downloading *)

OpenAIFile[assoc_][prop_] := assoc[prop]

OpenAIFileUpload[bodyBytes_?ByteArrayQ, purpose_:"batch", mimeType_:"application/jsonl"] :=
	Enclose@OpenAIFile@Confirm@OpenAIRequest[<|
		"Method" -> "POST",
		"Path" -> {"v1","files"},
		"Body" -> {
		"file" -> <|"Content" -> bodyBytes, "MIMEType" -> mimeType|>,
		"purpose" -> purpose
		}
	|>]


openaiFileDownloadRequest[fileID_?StringQ] :=
	openaiRequestObject[<|
		"Method" -> "GET",
		"Path" -> {"v1" ,"files", fileID, "content"}
	|>]

openaiFileDownloadRequest[file_OpenAIFile] :=
	openaiFileDownloadRequest[file["id"]]

OpenAIDownloadFile[fileID_?StringQ] :=
	URLRead[openaiFileDownloadRequest[fileID]]


(* Batch requests *)

OpenAIBatch[assoc_][prop_] := assoc[prop]


batchRequestByteArray[idBodies_, endpoint_] :=
	ByteArray@ToCharacterCode@StringRiffle[
		KeyValueMap[
			ExportString[<|
					"custom_id"->BaseEncode[BinarySerialize[#1]],
					"method"->"POST",
					"url"->endpoint,
					"body"->#2
				|>,
				"JSON","Compact"->True
			]&,
			idBodies
		],
		"\n"
	]

OpenAIBatchCreate[fileID_?StringQ, endpoint_, completionWindow_] :=
	Enclose@OpenAIBatch@Confirm@OpenAIRequest[<|
		"Method" -> "POST",
		"Path" -> {"v1", "batches"},
		"Body" -> ExportByteArray[<|
				"input_file_id" -> fileID,
				"endpoint" -> endpoint,
				"completion_window" -> "24h"
			|>,"JSON"],
		"ContentType" -> "application/json"
	|>]

OpenAIBatchCreate[file_OpenAIFile, endpoint_, completionWindow_] :=
	OpenAIBatchCreate[file["id"], endpoint, completionWindow]

OpenAIBatchCreate[batchMessages_, endpoint_, completionWindow_] :=
	OpenAIBatchCreate[OpenAIFileUpload@batchRequestByteArray[batchMessages, endpoint], endpoint, completionWindow]

OpenAIBatchCreate[batchSpec_] := OpenAIBatchCreate[batchSpec, "/v1/responses"]
OpenAIBatchCreate[batchSpec_, endpoint_] := OpenAIBatchCreate[batchSpec, endpoint, "24h"]


OpenAIBatchStatusRetrieve[batchID_?StringQ] :=
	Enclose@OpenAIBatch@Confirm@OpenAIRequest[<|
		"Method"->"GET",
		"Path"->{"v1", "batches", batchID}
	|>]

OpenAIBatchStatusRetrieve[batch_OpenAIBatch]:=
	OpenAIBatchStatusRetrieve[batch["id"]]


OpenAIBatchWait[batchID_?StringQ, t_] :=
	Module[{status},
		status=OpenAIBatchStatusRetrieve[batchID];
		Monitor[
			While[!MemberQ[{"completed", "failed", "expired", "cancelled"}, status["status"]],
				Pause[t];status=OpenAIBatchStatusRetrieve[batchID]
			],
			Dataset[status[[1]]]
		]
	]

OpenAIBatchWait[batch_OpenAIBatch, t_] :=
	OpenAIBatchWait[batch["id"], t]

OpenAIBatchWait[batch_] :=
	OpenAIBatchWait[batch, 5]


(* Batch responses *)

OpenAIBatchResponse[assoc_][prop_] := assoc[prop]

OpenAIBatchResponse[assoc_]["TotalUsage"] :=
	Total@assoc[[All, "response", "body", "usage"]]

OpenAIBatchResponse[assoc_]["CompletionOutputs"] :=
	OpenAIBatchResponse[assoc]["CompletionOutputChoices"][[All,1]]

OpenAIBatchResponse[assoc_]["CompletionOutputChoices"] :=
	Association[BinaryDeserialize[BaseDecode[#"custom_id"]] -> #[["response", "body", "choices", All, "message", "content"]] & /@ assoc]

OpenAIBatchResponse[assoc_]["ResponseOutputs"] :=
	Association[BinaryDeserialize[BaseDecode[#"custom_id"]] -> #[["response", "body", "output", -1, "content", 1, "text"]] & /@ assoc]

OpenAIBatchResponse[assoc_]["Outputs"] := OpenAIBatchResponse[assoc]["ResponseOutputs"]


importJSONL[i_] :=
	ImportString[#, "RawJSON"] & /@ StringSplit[Import[i, "Text"], "\n"]

OpenAIBatchRetrieve[resp_] :=
	Enclose@OpenAIBatchResponse@Confirm@importJSONL[resp]

OpenAIBatchRetrieve[file_OpenAIFile] :=
	Enclose@OpenAIBatchRetrieve@Confirm@openaiDownloadFile@file

OpenAIBatchRetrieve[batch_OpenAIBatch] :=
	Enclose@OpenAIBatchRetrieve@Confirm@OpenAIDownloadFile@Lookup[batch[[1]], "output_file_id", ConfirmAssert["No output file."]]


(* Chat completions *)

OpenAIChatComplete[req_]:=
	OpenAIRequest[<|
		"Method" -> "POST",
		"Path" -> {"v1", "chat", "completions"},
		"Body" -> ExportByteArray[req,"JSON"],
		"ContentType" -> "application/json"
	|>]


(* Responses API *)

OpenAIResponse[req_]:=
	OpenAIRequest[<|
		"Method" -> "POST",
		"Path" -> {"v1", "responses"},
		"Body" -> ExportByteArray[req,"JSON"],
		"ContentType" -> "application/json"
	|>]

End[];
EndPackage[];