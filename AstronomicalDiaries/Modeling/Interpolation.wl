BeginPackage["AstronomicalDiaries`Modeling`Interpolation`"];

linearInterpolate
eLinearInterpolate

Begin["`Private`"];

Needs["AstronomicalDiaries`"]


(* This function performs linear interpolation. It assumes that x is increasing (i.e. sorted), though y need not be. *)
eLinearInterpolate[x_, y_, p_] :=
	Native`UncheckedBlock@Module[{lastIndex=1},
		Assert[Length[x] == Length[y]];
		If[p <= First[x], Return[First[y]]];
		If[Last[x] <= p, Return[Last[y]]];
		Do[If[p < x[[i]], lastIndex=i-1;Break[]], {i,2,Length[x]}];
		(y[[lastIndex+1]] - y[[lastIndex]]) / (x[[lastIndex+1]] - x[[lastIndex]]) * (p - x[[lastIndex]]) + y[[lastIndex]]
	]

eLinearInterpolateThreaded[x_, y_, p_] :=
	IfCompiled[
		Assert[Length[x] === Length[y] === Length[p]];
		Table[eLinearInterpolate[x[[i]],y[[i]],p[[i]]], {i, Length[x]}],
		MapThread[eLinearInterpolate, {x, y, p}]
	]

eLinearInterpolateThreadedDeep[x_, y_, p_] :=
	IfCompiled[
		Assert[Length[x] === Length[y] === Length[Transpose[p]]];
		Transpose[Table[eLinearInterpolate[x[[i]],y[[i]],#[[i]]], {i, Length[x]}] &/@ Transpose[p]],
		Transpose[MapThread[eLinearInterpolate, {x, y, #}] &/@ Transpose[p]]
	]

eLinearInterpolateList[x_, y_, p_] :=
	eLinearInterpolate[x,y,#] &/@ p


setDefs[] := {clinearInterpolate, clinearInterpolateThreaded, clinearInterpolateThreadedDeep, clinearInterpolateList} =
	FunctionCompile[
		{
			FunctionDeclaration[eLinearInterpolate,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1],"MachineReal"}->"MachineReal"]@
				DownValuesFunction[eLinearInterpolate]
			],
			FunctionDeclaration[eLinearInterpolateThreaded,
				Typed[{"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[eLinearInterpolateThreaded]
			],
			FunctionDeclaration[eLinearInterpolateThreadedDeep,
				Typed[{"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 2],"PackedArray"::["MachineReal", 2]}->"PackedArray"::["MachineReal", 2]]@
				DownValuesFunction[eLinearInterpolateThreadedDeep]
			],
			FunctionDeclaration[eLinearInterpolateList,
				Typed[{"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1],"PackedArray"::["MachineReal", 1]}->"PackedArray"::["MachineReal", 1]]@
				DownValuesFunction[eLinearInterpolateList]
			]
		},
		{eLinearInterpolate, eLinearInterpolateThreaded, eLinearInterpolateThreadedDeep, eLinearInterpolateList},
		CompilerOptions -> {"AbortHandling" -> False}
	]

clinearInterpolate := (setDefs[]; clinearInterpolate)
clinearInterpolateThreaded := (setDefs[]; clinearInterpolateThreaded)
clinearInterpolateThreadedDeep := (setDefs[]; clinearInterpolateThreadedDeep)
clinearInterpolateList := (setDefs[]; clinearInterpolateList)

linearInterpolate[x_, y_, z_] :=
	clinearInterpolate[x,y,z]

linearInterpolate[x_, y_, z_] /; ArrayDepth[x] === 2 && ArrayDepth[z] === 1 :=
	clinearInterpolateThreaded[x,y,z]

linearInterpolate[x_, y_, z_] /; ArrayDepth[x] === 2 && ArrayDepth[z] === 2 :=
	clinearInterpolateThreadedDeep[x,y,z]

linearInterpolate[x_, y_, z_] /; ArrayDepth[x] === 1 && ArrayDepth[z] === 1 :=
	eLinearInterpolateList[x,y,z]


End[];
EndPackage[];