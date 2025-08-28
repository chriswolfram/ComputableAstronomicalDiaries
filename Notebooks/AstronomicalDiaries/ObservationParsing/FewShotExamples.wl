BeginPackage["AstronomicalDiaries`ObservationParsing`FewShotExamples`"];

examplePrompt

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`ObservationParsing`"]
Needs["AstronomicalDiaries`Utilities`"]


examples := examples = {
	<|
	"Text" -> "X202071",
	"Range" -> {5737, 5779},
	"Output" -> <|"object" -> "moon", "cubits" -> "1 1/2", "fingers" -> Null, "relation" -> "behind", "reference" -> "\[Alpha] Virginis"|>
	|>,
	<|
		"Text" -> "X300953",
		"Range" -> {1074, 1102},
		"Output" -> <|"object" -> "moon", "cubits" -> Null, "fingers" -> Null, "relation" -> Null, "reference" -> Null|>
	|>,
	(*<|
		"Text"->"X202011",
		"Range"->{256,301},
		"Output"-><|"object"->"moon","cubits"->"1 1/2","fingers"->Null,"relation"->Null,"reference"->"\[Delta] Cancri"|>
	|>,*)
	<|
		"Text" -> "X300953",
		"Range" -> {1105, 1152},
		"Output" -> <|"object" -> "moon", "cubits" -> "2 1/2", "fingers" -> Null, "relation" -> "behind", "reference" -> "Jupiter"|>
	|>,
	<|
		"Text" -> "X202071",
		"Range" -> {2580, 2611},
		"Output" -> <|"object" -> Null, "cubits" -> Null, "fingers" -> Null, "relation" -> Null, "reference" -> Null|>
	|>,
	<|
		"Text" -> "X103210",
		"Range" -> {5737, 5776},
		"Output" -> <|"object" -> "Mercury", "cubits" -> Null, "fingers" -> 4, "relation" -> "west", "reference" -> "\[Mu] Geminorum"|>
	|>
	(*<|
		"Text"->"X300953",
		"Range"->{1451,1495},
		"Output"-><|"object"->"moon","cubits"->"3 2/3","fingers"->Null,"relation"->"south","reference"->"\[Beta] Capricorni"|>
	|>*)
	};


formatExample[example_] := 
	StringTemplate["## Input\n\n`1`\n\n## Output\n\n`2`"][
			formatObservation[ADText@example["Text"], example["Range"]],
			exportUTF8JSON[example["Output"]]
		]

examplePrompt := examplePrompt = StringRiffle[formatExample /@ examples, "\n\n\n"];

End[];

EndPackage[];