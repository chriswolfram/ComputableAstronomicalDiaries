BeginPackage["AstronomicalDiaries`ObservationIdentification`FewShotExamples`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Utilities`"]
Needs["AstronomicalDiaries`TrainingExamples`"]

examplePrompt

Begin["`Private`"];


trainingChunks = {
		(*{"X202071",3},*)
		{"X301551", 1},
		{"X202022", 4},
		{"X102840", 3},
		{"X201835", 2}
	};

formatExample[chunk : {_?StringQ, _?IntegerQ}] :=
	{
		getText[chunk],
		exportUTF8JSON@<|"observations" -> (#RawString & /@ trainingExample[chunk])|>
	}

formatExample[chunks : {{_?StringQ, _?IntegerQ} ...}] := formatExample /@ chunks


examplePrompt := examplePrompt = 
  StringRiffle[
		StringTemplate["## Input\n\n`1`\n\n## Output\n\n`2`"] @@@ (formatExample /@ trainingChunks),
		"\n\n\n"
	]


End[];

EndPackage[];