BeginPackage["AstronomicalDiaries`Chronology`"];

regnalToSE
parseRegnalYear

getPDChronologyMonthMap

Begin["`Private`"];

Needs["AstronomicalDiaries`"]


chronologyPath := FileNameJoin[{$ADBase, "Chronology"}]

(* Regnal years *)

reignSE = <|
		"NbkII" -> -293,
		"DarI" -> -210,
		"ArtI" -> -153,
		"DarII" -> -112,
		"ArtII" -> -93,
		"ArtIII" -> -47,
		"DarIII" -> -24,
		"AlexanderIII" -> -25,
		"Philip Arrhidaeus" -> -12,
		"Philip" -> -12,(*Just another name for the line above*)
		"AlexanderIV" -> -5,
		"SE" -> 0
	|>;

regnalToSE[{king_, year_}] :=  Catch[Lookup[reignSE, king, Throw[Missing[]]] + year]
regnalToSE[m_Missing] := m


parseRegnalYear[str_?StringQ] :=
	First[
		StringCases[
			str,
			StartOfString ~~
			Shortest[king___] ... ~~
			Longest[WhitespaceCharacter ...] ~~
			num : DigitCharacter ... ~~
			EndOfString :> {king, ToExpression@num}
		],
		Missing[]
	]

parseRegnalYear[x_] := Missing[]


(* P&D *)

(*
	Import the Parker and Dubberstein chronology from here:
		http://www.staff.science.uu.nl/~gent0113/babylon/babycal.htm
		http://www.staff.science.uu.nl/~gent0113/babylon/downloads/babylonian_chronology_pd_1971.dat

	Another source is available here:
		http://baptiste.meles.free.fr/site/mesocalc.html#dates
*)


rawPDChronologyPath := FileNameJoin[{chronologyPath, "PD", "babylonian_chronology_pd_1971.dat"}]
pdChronologyMonthMapPath := FileNameJoin[{chronologyPath, "PD", "pdChronologyMonthMap.mx"}]

getRawPDChronologyDat[] :=
	(
		If[FileExistsQ[rawPDChronologyPath],
			Return@ReadString[rawPDChronologyPath]
		];
		URLDownload[
			"http://www.staff.science.uu.nl/~gent0113/babylon/downloads/babylonian_chronology_pd_1971.dat",
			rawPDChronologyPath,
			CreateIntermediateDirectories -> True
		];
		ReadString[rawPDChronologyPath]
	)

pdMonthMap = <|
		"1" -> "I",
		"2" -> "II",
		"3" -> "III",
		"4" -> "IV",
		"5" -> "V",
		"6" -> "VI",
		"6b" -> "VI2",
		"7" -> "VII",
		"8" -> "VIII",
		"9" -> "IX",
		"10" -> "X",
		"11" -> "XI",
		"12" -> "XII",
		"12b" -> "XII2"
	|>;

getPDChronologyMonthMap[] := getPDChronologyMonthMap[] =
	Module[{pdChronologyMonthMap, rawPDChronologyDat, rawPDChronology, adjustedPDChronology},
		If[FileExistsQ[pdChronologyMonthMapPath],
			Return@Import[pdChronologyMonthMapPath]
		];

		rawPDChronologyDat = getRawPDChronologyDat[];
		rawPDChronology =
			{
				ToExpression[#[[1]]],
				pdMonthMap[#[[2]]],
				ToExpression[#[[3]]],
				ToExpression[#[[4]]],
				ToExpression[#[[5]]]
			} & /@ StringSplit[StringSplit[rawPDChronologyDat, "\r\n"], Whitespace][[All, ;; 5]];
		
		adjustedPDChronology = MapAt[If[# < 1, # - 1, #] &, rawPDChronology, {All, 3}];

		pdChronologyMonthMap =
			GroupBy[
				adjustedPDChronology,
				(#[[;; 2]] &) -> (DateObject[#[[3 ;;]], "Day", CalendarType -> "Julian"] &),
				First
			];

		Export[pdChronologyMonthMapPath, pdChronologyMonthMap];
		pdChronologyMonthMap
	]


(* ADART *)

(*
	This is the chronology given by Sachs and Hunger in Astronomical Diaries and Related Texts from Babylonia.
	It is made up of adjustments to the P&D chronology imported above.
*)

adartChronologyPath := FileNameJoin[{DirectoryName@FindFile["AstronomicalDiaries`Chronology`"], "ADARTChronology", "adartChronology.csv"}]
adartChronologyMonthMapPath := FileNameJoin[{chronologyPath, "ADART", "adartChronologyMonthMap.mx"}]

getADARTChronologyMonthMap[] := getADARTChronologyMonthMap[] =
	Module[{rawAdartChronology, adartChronologyAdjustments, adartChronologyMonthMap},
		If[FileExistsQ[adartChronologyMonthMapPath],
			Return@Import[adartChronologyMonthMapPath]
		];
		
		rawAdartChronology = Import@adartChronologyPath;
		adartChronologyAdjustments = Association[{regnalToSE[{#[[1]], #[[2]]}], #[[3]]} -> #[[4]] & /@ Rest[rawAdartChronology]];
		adartChronologyMonthMap = Merge[KeyUnion[{getPDChronologyMonthMap[], adartChronologyAdjustments}, 0 &], Apply[#1 - Quantity[#2, "Days"] &]];

		Export[adartChronologyMonthMapPath, adartChronologyMonthMap];
		adartChronologyMonthMap
	]


(* General *)

ADChronology["PD"] := getPDChronologyMonthMap[]
ADChronology["ADART"] := getADARTChronologyMonthMap[]
ADChronology[Automatic] := ADChronology["ADART"]
ADChronology[] := ADChronology[Automatic]

ADFromBabylonianDate[{y_, m_, _Missing | Null}, chron_] := Missing[]

ADFromBabylonianDate[{y_, m_, d_}, chron_] := ADFromBabylonianDate[{y, m, d}, chron] =
	Catch[Lookup[chron, Key[{y, m}], Throw[Missing[]]] + Quantity[d - 1, "Days"]]
	
ADFromBabylonianDate[d_] := ADFromBabylonianDate[d, ADChronology[]]

End[];
EndPackage[];