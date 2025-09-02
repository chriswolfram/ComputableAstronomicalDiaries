BeginPackage["AstronomicalDiaries`Modeling`TrueModel`"];

timeRange
objectDistanceApproxParamsCache
objectDistanceApproxParams
objectDistanceApprox

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Astronomy`"]


(* This is code for a linear approximation of the relative positions of astronomical objects. *)

timeRange = {-12, 12};
objectDistanceApproxParamsCache = <||>;

objectDistanceApproxParams[obj_, ref_, rel_, d_] :=
	Lookup[
		objectDistanceApproxParamsCache,
		Key[{obj, ref, rel, d}],
		objectDistanceApproxParamsCache[{obj, ref, rel, d}] =
			Table[
				objectDisplacement[obj, ref, DateObject[d, "Instant", TimeZone -> $timeZone] + Quantity[ti, "Hours"]][[relationAxes[rel]]],
				{ti, timeRange}
			]
	]

objectDistanceApproxParams[obj_List, ref_List, rel_List, d_List] := 
	MapThread[objectDistanceApproxParams, {obj, ref, rel, d}]


objectDistanceApprox[params_, t_] /; ArrayDepth[params] === 1 :=
 (params[[2]] - params[[1]]) / (timeRange[[2]] - timeRange[[1]]) * (t - timeRange[[1]]) + params[[1]]

objectDistanceApprox[params_, t_] /; ArrayDepth[params] === 2 :=
(params[[All, 2]] - params[[All, 1]]) / (timeRange[[2]] - timeRange[[1]]) * (t - timeRange[[1]]) + params[[All, 1]]

objectDistanceApprox[obj_, ref_, rel_, d_, t_] :=
	objectDistanceApprox[objectDistanceApproxParams[obj, ref, rel, d], t]


End[];
EndPackage[];