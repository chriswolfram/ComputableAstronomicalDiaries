BeginPackage["AstronomicalDiaries`Modeling`TrueModel`"];

timeRange
objectDistanceApproxCache
objectDistanceApproxFunction
objectDistanceApprox

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Astronomy`"]


(* This is code for a linear approximation of the relative positions of astronomical objects. *)

timeRange = Range[-12,12,1.];
objectDistanceApproxCache = <||>;

objectDistanceApproxFunction[obj_, ref_, rel_, d_] :=
	Lookup[
		objectDistanceApproxCache,
		Key[{obj, ref, rel, d}],
		objectDistanceApproxCache[{obj, ref, rel, d}] =
			Interpolation[
				Transpose@{
					timeRange,
					Table[
						objectDisplacement[obj, ref, DateObject[d, "Instant", TimeZone -> $timeZone] + Quantity[ti, "Hours"]][[relationAxes[rel]]],
						{ti, timeRange}
					]
				},
				Method -> "Hermite",
				InterpolationOrder -> 1
			]
	]

objectDistanceApproxFunction[obj_List, ref_List, rel_List, d_List] := 
	MapThread[objectDistanceApproxFunction, {obj, ref, rel, d}]


objectDistanceApprox[if_, t_] := if[t]
objectDistanceApprox[if_List, t_] := MapThread[#1[#2]&, {if,t}]

objectDistanceApprox[obj_, ref_, rel_, d_, t_] :=
	objectDistanceApprox[objectDistanceApproxFunction[obj, ref, rel, d], t]


End[];
EndPackage[];