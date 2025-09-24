BeginPackage["AstronomicalDiaries`Modeling`Interpolation`"];

linearInterpolateMonotonic
eLinearInterpolateMonotonic

Begin["`Private`"];

Needs["AstronomicalDiaries`"]


eLinearInterpolateMonotonic[x_, y_, p_] :=
	Module[{lastIndex=0},
		If[p <= x[[1]], Return[y[[1]]]];
		If[x[[-1]] <= p, Return[y[[-1]]]];
		Do[If[p < x[[i]], lastIndex=i-1;Break[]], {i,Length[x]}];
		(y[[lastIndex+1]] - y[[lastIndex]]) / (x[[lastIndex+1]] - x[[lastIndex]]) * (p - x[[lastIndex]]) + y[[lastIndex]]
	]

eLinearInterpolateMonotonicThreaded[x_, y_, p_] :=
	MapThread[eLinearInterpolateMonotonic, {x, y, p}]

eLinearInterpolateMonotonicList[x_, y_, p_] :=
	eLinearInterpolateMonotonic[x,y,#] &/@ p


setDefs[] := {cLinearInterpolateMonotonic, cLinearInterpolateMonotonicThreaded, cLinearInterpolateMonotonicList} =
	FunctionCompile[
		{
			FunctionDeclaration[eLinearInterpolateMonotonic,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1],"MachineReal"}->"MachineReal"]@
				DownValuesFunction[eLinearInterpolateMonotonic]
			],
			FunctionDeclaration[eLinearInterpolateMonotonicThreaded,
				Typed[{"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[eLinearInterpolateMonotonicThreaded]
			],
			FunctionDeclaration[eLinearInterpolateMonotonicList,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[eLinearInterpolateMonotonicList]
			]
		},
		{eLinearInterpolateMonotonic, eLinearInterpolateMonotonicThreaded, eLinearInterpolateMonotonicList}
	]

cLinearInterpolateMonotonic := (setDefs[]; cLinearInterpolateMonotonic)
cLinearInterpolateMonotonicThreaded := (setDefs[]; cLinearInterpolateMonotonicThreaded)
cLinearInterpolateMonotonicList := (setDefs[]; cLinearInterpolateMonotonicList)

linearInterpolateMonotonic[x_, y_, z_] :=
	cLinearInterpolateMonotonic[x,y,z]

linearInterpolateMonotonic[x_, y_, z_] /; ArrayDepth[x] === 2 :=
	cLinearInterpolateMonotonicThreaded[x,y,z]

linearInterpolateMonotonic[x_, y_, z_] /; ArrayDepth[x] === 1 && ListQ[z] :=
	eLinearInterpolateMonotonicList[x,y,z]


End[];
EndPackage[];