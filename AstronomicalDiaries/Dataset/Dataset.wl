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
Needs["AstronomicalDiaries`Texts`"]

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


End[];
EndPackage[];