BeginPackage["AstronomicalDiaries`Modeling`Interpolation`"];

linearInterpolateMonotonic
eLinearInterpolateMonotonic

Begin["`Private`"];

Needs["AstronomicalDiaries`"]


eLinearInterpolateMonotonic[x_, y_, p_] :=
	Native`UncheckedBlock@Module[{lastIndex=1},
		Assert[Length[x] == Length[y]];
		If[p <= x[[1]], Return[y[[1]]]];
		If[x[[-1]] <= p, Return[y[[-1]]]];
		Do[If[p < x[[i]], lastIndex=i-1;Break[]], {i,2,Length[x]}];
		(y[[lastIndex+1]] - y[[lastIndex]]) / (x[[lastIndex+1]] - x[[lastIndex]]) * (p - x[[lastIndex]]) + y[[lastIndex]]
	]

eLinearInterpolateMonotonicThreaded[x_, y_, p_] :=
	IfCompiled[
		Assert[Length[x] === Length[y] === Length[p]];
		Table[eLinearInterpolateMonotonic[x[[i]],y[[i]],p[[i]]], {i, Length[x]}],
		MapThread[eLinearInterpolateMonotonic, {x, y, p}]
	]

eLinearInterpolateMonotonicThreadedDeep[x_, y_, p_] :=
	IfCompiled[
		Assert[Length[x] === Length[y] === Length[Transpose[p]]];
		Transpose[Table[eLinearInterpolateMonotonic[x[[i]],y[[i]],#[[i]]], {i, Length[x]}] &/@ Transpose[p]],
		Transpose[MapThread[eLinearInterpolateMonotonic, {x, y, #}] &/@ Transpose[p]]
	]

eLinearInterpolateMonotonicList[x_, y_, p_] :=
	eLinearInterpolateMonotonic[x,y,#] &/@ p


setDefs[] := {cLinearInterpolateMonotonic, cLinearInterpolateMonotonicThreaded, cLinearInterpolateMonotonicThreadedDeep, cLinearInterpolateMonotonicList} =
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
			FunctionDeclaration[eLinearInterpolateMonotonicThreadedDeep,
				Typed[{"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 2]}->"PackedArray"::["MachineReal", 2]]@
				DownValuesFunction[eLinearInterpolateMonotonicThreadedDeep]
			],
			FunctionDeclaration[eLinearInterpolateMonotonicList,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[eLinearInterpolateMonotonicList]
			]
		},
		{eLinearInterpolateMonotonic, eLinearInterpolateMonotonicThreaded, eLinearInterpolateMonotonicThreadedDeep, eLinearInterpolateMonotonicList},
		CompilerOptions -> {"AbortHandling" -> False}
	]

cLinearInterpolateMonotonic := (setDefs[]; cLinearInterpolateMonotonic)
cLinearInterpolateMonotonicThreaded := (setDefs[]; cLinearInterpolateMonotonicThreaded)
cLinearInterpolateMonotonicThreadedDeep := (setDefs[]; cLinearInterpolateMonotonicThreadedDeep)
cLinearInterpolateMonotonicList := (setDefs[]; cLinearInterpolateMonotonicList)

linearInterpolateMonotonic[x_, y_, z_] :=
	cLinearInterpolateMonotonic[x,y,z]

linearInterpolateMonotonic[x_, y_, z_] /; ArrayDepth[x] === 2 && ArrayDepth[z] === 1 :=
	cLinearInterpolateMonotonicThreaded[x,y,z]

linearInterpolateMonotonic[x_, y_, z_] /; ArrayDepth[x] === 2 && ArrayDepth[z] === 2 :=
	cLinearInterpolateMonotonicThreadedDeep[x,y,z]

linearInterpolateMonotonic[x_, y_, z_] /; ArrayDepth[x] === 1 && ArrayDepth[z] === 1 :=
	eLinearInterpolateMonotonicList[x,y,z]


End[];
EndPackage[];