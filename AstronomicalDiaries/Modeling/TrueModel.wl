BeginPackage["AstronomicalDiaries`Modeling`TrueModel`"];

timeIntervals
objectDistanceApproxParamsCache
objectDistanceApproxParams
objectDistanceApprox

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Astronomy`"]


(* This is code for a linear approximation of the relative positions of astronomical objects. *)

timeSamples = N@Table[t, {t, -12, 12, 4}];
timeIntervals = Partition[timeSamples, 2, 1];
objectDistanceApproxParamsCache = <||>;

pathParams[path_, intervals_] :=
	MapThread[
		With[{m = (#1[[2]] - #1[[1]])/(#2[[2]] - #2[[1]])},
			{m, #1[[1]] - m*#2[[1]]}
		] &,
		{
			Partition[path, 2, 1],
			intervals
		}
	]

objectDistanceApproxParams[obj_, ref_, rel_, d_] :=
	Lookup[
		objectDistanceApproxParamsCache,
		Key[{obj, ref, rel, d}],
		objectDistanceApproxParamsCache[{obj, ref, rel, d}] =
			pathParams[
				Table[
					objectDisplacement[obj, ref, DateObject[d, "Instant", TimeZone -> $timeZone] + Quantity[ti, "Hours"]][[relationAxes[rel]]],
					{ti, timeSamples}
				],
				timeIntervals
			]
	]

objectDistanceApproxParams[obj_List, ref_List, rel_List, d_List] := 
	MapThread[objectDistanceApproxParams, {obj, ref, rel, d}]


objectDistanceApprox[params_, t_] /; ArrayDepth[params] === 2 :=
	params[[Replace[First@FirstPosition[timeSamples, _?(GreaterThan[t]), {Length[timeSamples]}] - 1, 0 -> 1]]] . {t, 1}

objectDistanceApprox[params_, t_] /; ArrayDepth[params] === 3 :=
	MapThread[objectDistanceApprox, {params, t}]

objectDistanceApprox[obj_, ref_, rel_, d_, t_] :=
	objectDistanceApprox[objectDistanceApproxParams[obj, ref, rel, d], t]


End[];
EndPackage[];