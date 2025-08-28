Needs["AstronomicalDiaries`Dataset`Days`"]

TestCreate[
	findObservationDayRanges["Night of the 12th, beginning of the night, test observation here. Night of the 14th.", {{44, 45}}]
	,
	{{{1, 15} -> 12, {1, 15} -> 12}}
]

TestCreate[
	findObservationDayRanges["Night of the 12th, ..., beginning of the night, test observation here. Night of the 14th.", {{44, 45}}]
	,
	{{{1, 15} -> 12, {72, 86} -> 13}}
]

TestCreate[
	findObservationDayRanges["Night of the 12th, beginning of the night, test observation here. Night of the", {{44, 45}}]
	,
	{{{1, 15} -> 12, {1, 15} -> 12}}
]

TestCreate[
	findObservationDayRanges["Night of the 12th, ..., beginning of the night, test observation here. Night of the.", {{44, 45}}]
	,
	{{{1, 15} -> 12, Missing[] -> 30}}
]

TestCreate[
	findObservationDayRanges["Night of the, beginning of the night, test observation here. Night of the 14th.", {{44, 45}}]
	,
	{{Missing[] -> 1, {62, 76} -> 13}}
]

TestCreate[
	findObservationDayRanges["Night of the, beginning of the night, test observation here. Night of the.", {{44, 45}}]
	,
	{{Missing[] -> 1, Missing[] -> 30}}
]
