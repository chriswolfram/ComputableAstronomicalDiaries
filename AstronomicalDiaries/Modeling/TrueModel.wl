BeginPackage["AstronomicalDiaries`Modeling`TrueModel`"];

timeRange
objectDistanceApproxCache
objectDistanceApproxParams
objectDistanceApprox

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Astronomy`"]
Needs["AstronomicalDiaries`Modeling`Interpolation`"]


(* This is code for a piecewise-linear approximation of the relative positions of astronomical objects. *)

timeRange = Range[-12,12,2.];
If[!AssociationQ[objectDistanceApproxCache], objectDistanceApproxCache = <||>];

objectDistanceApproxParams[obj_, ref_, rel_, d_] :=
	Lookup[
		objectDistanceApproxCache,
		Key[{obj, ref, rel, d}],
		objectDistanceApproxCache[{obj, ref, rel, d}] =
			Table[
				objectDisplacement[obj, ref, DateObject[d, "Instant", TimeZone -> $timeZone] + Quantity[ti, "Hours"]][[relationAxes[rel]]],
				{ti, timeRange}
			]
	]

objectDistanceApproxParams[obj_List, ref_List, rel_List, d_List] :=
	MapThread[objectDistanceApproxParams, {obj, ref, rel, d}]


objectDistanceApprox[params_, t_] := linearInterpolate[timeRange, params, t]

objectDistanceApprox[paramsList_List, tList_List] /; ArrayDepth[paramsList] === 2 :=
	linearInterpolate[ConstantArray[timeRange, Length[paramsList]], paramsList, tList]

objectDistanceApprox[obj_, ref_, rel_, d_, t_] :=
	objectDistanceApprox[objectDistanceApproxParams[obj, ref, rel, d], t]


End[];
EndPackage[];