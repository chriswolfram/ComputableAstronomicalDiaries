BeginPackage["AstronomicalDiaries`Modeling`GriddyGibbs`"];

griddyGibbsSample
griddyGibbsMakeGrid

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Modeling`Interpolation`"]


(*
	Implements Griddy-Gibbs to sample from a pdf per:
	"Facilitating the Gibbs Sampler: The Gibbs Stopper and the Griddy-Gibbs Sampler"
	by Christian Ritter and Martin A. Tanner
*)

logSumExpPlus[a_,b_] := Native`UncheckedBlock@With[{c = Max[a, b]}, c + Log[Exp[a - c] + Exp[b - c]]]

normalizeLogAccumulate[logPDF_] := Native`UncheckedBlock@With[{cdf = FoldList[logSumExpPlus, logPDF]}, cdf - Last[cdf]]

pdfToCDFPositions[pts_] := Native`UncheckedBlock@Join[{pts[[1]]}, Most[pts] + Differences[pts]/2, {Last[pts]}]

logPDFToLogCDF[x_, logPDF_] :=
	Native`UncheckedBlock@Prepend[normalizeLogAccumulate[logPDF + Log[Differences[x]]], -$MaxMachineNumber]


eGriddyGibbsSample[xPDF_, logPDF_] :=
	Native`UncheckedBlock@Module[{xCDF, cdf},
		xCDF = pdfToCDFPositions[xPDF];
		cdf = Exp@logPDFToLogCDF[xCDF, logPDF];
		eLinearInterpolate[cdf, xCDF, RandomReal[]]
	]

eGriddyGibbsSampleThreaded[xPDFs_, logPDFs_] :=
	Native`UncheckedBlock@MapThread[eGriddyGibbsSample, {xPDFs, logPDFs}]


eGriddyGibbsMakeGrid[xPDF_, logPDF_] :=
	Native`UncheckedBlock@Module[{xCDF, cdf},
		xCDF = pdfToCDFPositions[xPDF];
		cdf = Exp@logPDFToLogCDF[xCDF, logPDF];
		eLinearInterpolate[cdf, xCDF, #] &/@ Range[0,1,1./(Length[xPDF]-1)]
	]

eGriddyGibbsMakeGridThreaded[xPDFs_, logPDFs_] :=
	Native`UncheckedBlock@MapThread[eGriddyGibbsMakeGrid, {xPDFs, logPDFs}]


setDefs[] := {cGriddyGibbsSample, cGriddyGibbsSampleThreaded, cGriddyGibbsMakeGrid, cGriddyGibbsMakeGridThreaded} =
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
			FunctionDeclaration[eLinearInterpolate,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1],"MachineReal"}->"MachineReal"]@
				DownValuesFunction[eLinearInterpolate]
			],

			FunctionDeclaration[eGriddyGibbsSample,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1]}->"MachineReal"]@
				DownValuesFunction[eGriddyGibbsSample]
			],
			FunctionDeclaration[eGriddyGibbsSampleThreaded,
				Typed[{"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 2]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[eGriddyGibbsSampleThreaded]
			],

			FunctionDeclaration[eGriddyGibbsMakeGrid,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[eGriddyGibbsMakeGrid]
			],
			FunctionDeclaration[eGriddyGibbsMakeGridThreaded,
				Typed[{"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 2]}->"PackedArray"::["MachineReal", 2]]@
				DownValuesFunction[eGriddyGibbsMakeGridThreaded]
			]
		},
		{eGriddyGibbsSample, eGriddyGibbsSampleThreaded, eGriddyGibbsMakeGrid, eGriddyGibbsMakeGridThreaded},
		CompilerOptions -> {"AbortHandling" -> False}
	]

cGriddyGibbsSample := (setDefs[]; cGriddyGibbsSample)
cGriddyGibbsSampleThreaded := (setDefs[]; cGriddyGibbsSampleThreaded)
cGriddyGibbsMakeGrid := (setDefs[]; cGriddyGibbsMakeGrid)
cGriddyGibbsMakeGridThreaded := (setDefs[]; cGriddyGibbsMakeGridThreaded)


griddyGibbsSample[x_, logPDF_] /; ArrayDepth[logPDF] === 1 := Echo[cGriddyGibbsSample[x, logPDF], 1]
griddyGibbsSample[x_, logPDFs_] /; ArrayDepth[logPDFs] === 2 := Echo[cGriddyGibbsSampleThreaded[x, logPDFs], 2]


End[];
EndPackage[];
