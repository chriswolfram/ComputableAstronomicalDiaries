BeginPackage["OpenAIUtilities`"];

(*
OpenAIRequest
OpenAIFile
OpenAIUploadFile
OpenAIDownloadFile
*)

OpenAIBatch
OpenAICreateBatch
OpenAIRetrieveBatchStatus
OpenAIWaitBatch
OpenAIRetrieveBatch
OpenAIBatchResponse

OpenAICompleteChat

Begin["`Private`"];

OpenAICompleteChat[x_] := x+2

End[];
EndPackage[];