BeginPackage["AstronomicalDiaries`Modeling`GriddyGibbs`"];

truncatePDF
griddyGibbsSample
griddyGibbsSampleLog

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Modeling`Interpolation`"]


(*
	Implements Griddy-Gibbs to sample from a pdf per:
	"Facilitating the Gibbs Sampler: The Gibbs Stopper and the Griddy-Gibbs Sampler"
	by Christian Ritter and Martin A. Tanner
*)

(* Real-space version (slow and unstable) *)

(* Converts a pdf to a cdf with the proper centering *)
pdfToCDF[pdf_] :=
	Module[{x, y},
		x = Join[{pdf[[1, 1]]}, Most[pdf[[All, 1]]] + Differences[pdf[[All, 1]]]/2, {pdf[[-1, 1]]}];
		y = Prepend[#/Last[#] &@ Accumulate[pdf[[All, 2]] * Differences[x]], 0.];
		Transpose@{x, y}
	]

(* Adds a small value to points that coincide so that Interpolation doesn't panic *)
jitterList[l_, tol_] := Catenate[# + tol*Range[0, Length[#] - 1] & /@ Split[l, Abs[#1-#2]<tol&]]
jitterPoints[pts_, tol_ : $MachineEpsilon*1000] :=
	SubsetMap[jitterList[#, tol]&, SubsetMap[jitterList[#, tol]&, pts, {All, 1}], {All, 2}]

(* Set an interval to 0 in a pdf *)
truncatePDF[pdf_, rawInterval_Interval] :=
	With[
		{interval = IntervalIntersection[rawInterval, Interval[MinMax[pdf[[All, 1]]]]]},
		{filteredPDF = Pick[pdf, IntervalMemberQ[interval, pdf[[All, 1]]], False]},
		{if = Interpolation[jitterPoints@pdf, InterpolationOrder -> 1, Method -> "Hermite"]},
		SortBy[
			Join[
				filteredPDF,
				Catenate[
					{
						{#[[1]], if[#[[1]]]},
						{#[[1]] + 10*$MachineEpsilon, 0},
						{#[[2]] - 10*$MachineEpsilon, 0},
						{#[[2]], if[#[[2]]]}
					} & /@ List @@ interval
				]
			],
			First
		]
	]


griddyGibbsSample[pdf_, n_] :=
	With[
		{cdf = pdfToCDF[pdf]},
		{if = Interpolation[jitterPoints[Reverse /@ cdf], Method -> "Hermite", InterpolationOrder -> 1]},
		if[RandomReal[{0, 1}, n]]
	]
griddyGibbsSample[pdf_] := griddyGibbsSample[pdf, 1]


(* Log-space version *)

logSumExpPlus[a_,b_] := Native`UncheckedBlock@With[{c = Max[a, b]}, c + Log[Exp[a - c] + Exp[b - c]]]

normalizeLogAccumulate[logPDF_] := With[{cdf = FoldList[logSumExpPlus, logPDF]}, cdf - Last[cdf]]

pdfToCDFPositions[pts_] := Join[{pts[[1]]}, Most[pts] + Differences[pts]/2, {pts[[-1]]}]

logPDFToLogCDF[x_, logPDF_] :=
	Prepend[normalizeLogAccumulate[logPDF + Log[Differences[x]]], -$MaxMachineNumber]

eGriddyGibbsSampleLog[pts_, logPDFs_] :=
	Module[{x},
		x = pdfToCDFPositions[pts];
		eLinearInterpolateMonotonic[Exp@logPDFToLogCDF[x, #], x, RandomReal[]] &/@ logPDFs
	]

griddyGibbsSampleLog := griddyGibbsSampleLog =
	FunctionCompile[
		{
			FunctionDeclaration[Differences,
				Typed[{"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				Function[l,
					Rest[l]-Most[l]
				]
			],
			FunctionDeclaration[logSumExpPlus,
				Typed[{"MachineReal","MachineReal"}->"MachineReal"]@
				DownValuesFunction[logSumExpPlus]
			],
			FunctionDeclaration[normalizeLogAccumulate,
				Typed[{"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[normalizeLogAccumulate]
			],
			FunctionDeclaration[pdfToCDFPositions,
				Typed[{"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[pdfToCDFPositions]
			],
			FunctionDeclaration[logPDFToLogCDF,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[logPDFToLogCDF]
			],
			FunctionDeclaration[eLinearInterpolateMonotonic,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1],"MachineReal"}->"MachineReal"]@
				DownValuesFunction[eLinearInterpolateMonotonic]
			],
			FunctionDeclaration[eGriddyGibbsSampleLog,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 2]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[eGriddyGibbsSampleLog]
			]
		},
		eGriddyGibbsSampleLog,
		CompilerOptions -> {"AbortHandling" -> False}
	]


End[];
EndPackage[];
