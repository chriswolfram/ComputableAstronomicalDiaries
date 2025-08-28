BeginPackage["AstronomicalDiaries`TrainingExamples`"];

trainingExample

Begin["`Private`"];

Needs["AstronomicalDiaries`"]

trainingExampleBase := trainingExampleBase = FileNameJoin[{$ADBase, "TrainingExamples"}]

trainingExamplePath[{id_, i_}] :=
	FileNameJoin[{trainingExampleBase, id <> "-" <> ToString[i] <> "-chunk.csv"}]

trainingExample[spec_] :=
	Module[{csv, rawAssoc},

		csv = Import[trainingExamplePath[spec], "Numeric" -> False] /. "" -> Missing["NotAvailable"];

		If[Length[csv] === 2 && ContainsOnly[csv[[2]], {Missing["NotAvailable"]}],
			Return[{}]
		];

		rawAssoc = AssociationThread[First[csv], #] & /@ Rest[csv];
		<|
			"Line" -> #line,
			"RawString" -> #"raw_string",
			"Damaged" -> Replace[#damaged, {"TRUE" -> True, "FALSE" -> False}],
			"Explanation" -> #explanation,
			"EarliestDay" -> 
			Replace[#"earliest_day", s_?StringQ :> ToExpression[s]],
			"LatestDay" -> 
			Replace[#"latest_day", s_?StringQ :> ToExpression[s]],
			"Time" -> #time,
			"Object" -> #object,
			"Cubits" -> #cubits,
			"Fingers" -> #fingers,
			"Relation" -> #relation,
			"Reference" -> #reference
		|> & /@ rawAssoc
	]

getTrainingExampleList[] :=
	MapAt[ToExpression, StringSplit[FileBaseName[#], "-"][[;; 2]], 2] &/@ FileNames["*-chunk.csv", trainingExampleBase]


End[];

EndPackage[];