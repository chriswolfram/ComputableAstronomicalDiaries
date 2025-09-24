BeginPackage["AstronomicalDiaries`Modeling`GriddyGibbs`"];

truncatePDF
griddyGibbsSample
griddyGibbsSampleLog

Begin["`Private`"];

Needs["AstronomicalDiaries`"]

(* Converts a pdf to a cdf with the proper centering *)
pdfToCDF[pdf_] :=
	Module[{x, y},
		x = Join[{pdf[[1, 1]]}, Most[pdf[[All, 1]]] + Differences[pdf[[All, 1]]]/2, {pdf[[-1, 1]]}];
		y = Prepend[#/Last[#] &@ Accumulate[pdf[[All, 2]] * Differences[x]], 0.];
		Transpose@{x, y}
	]

cLogCDF := cLogCDF =
	FunctionCompile[
		FunctionDeclaration[logSumExpPlus,
			Function[{Typed[a, "MachineReal"], Typed[b, "MachineReal"]},
				With[{c = Max[a, b]}, c + Log[Exp[a - c] + Exp[b - c]]]
			]
		],
		Function[Typed[logPDF, "PackedArray"::["MachineReal", 1]],
			With[{cdf = FoldList[logSumExpPlus, logPDF]}, cdf - Last[cdf]]
		]
	];

logPDFToLogCDF[logPDF_] :=
	Module[{x, y},
		x = Join[{logPDF[[1, 1]]}, Most[logPDF[[All, 1]]] + Differences[logPDF[[All, 1]]]/2, {logPDF[[-1, 1]]}];
		y = Prepend[cLogCDF[logPDF[[All, 2]] + Log[Differences[x]]], -$MaxMachineNumber];
		Transpose@{x, y}
	]

(* Adds a small value to points that coincide so that Interpolation doesn't panic *)
jitterList[l_, tol_] := Catenate[# + tol*Range[0, Length[#] - 1] & /@ Split[l, Abs[#1-#2]<tol&]]
jitterPoints[pts_, tol_ : $MachineEpsilon*1000] :=
	SubsetMap[jitterList[#, tol] &, SubsetMap[jitterList[#, tol] &, pts, {All, 1}], {All, 2}]

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

(*
	Use Griddy-Gibbs to sample from a pdf per:
	"Facilitating the Gibbs Sampler: The Gibbs Stopper and the Griddy-Gibbs Sampler"
	by Christian Ritter and Martin A. Tanner
*)
griddyGibbsSample[pdf_, n_] :=
	With[
		{cdf = pdfToCDF[pdf]},
		{if = Interpolation[jitterPoints[Reverse /@ cdf], Method -> "Hermite", InterpolationOrder -> 1]},
		if[RandomReal[{0, 1}, n]]
	]
griddyGibbsSample[pdf_] := griddyGibbsSample[pdf, 1]


griddyGibbsSampleLog[logPDF_, n_] :=
	With[
		{logCDF = logPDFToLogCDF[logPDF]},
		{if = Interpolation[jitterPoints[Reverse /@ logCDF], Method -> "Hermite", InterpolationOrder -> 1]},
		if[Log@RandomReal[{0, 1}, n]]
	]
griddyGibbsSampleLog[pdf_] := griddyGibbsSampleLog[pdf, 1]


End[];
EndPackage[];
