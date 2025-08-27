BeginPackage["AstronomicalDiaries`FuzzyAlignment`"];

FuzzyAlignments

Needs["AstronomicalDiaries`"]

Begin["`Private`"];

fuzzyAlignSingle[text_, frag_]:=
	Module[{alignment},
		alignment = SequenceAlignment[text, frag, MergeDifferences->True, Method->"Local"];
		{
			If[ListQ@First@alignment, StringLength[alignment[[1,1]]], 1],
			If[ListQ@Last@alignment, StringLength[text]-StringLength[alignment[[-1,1]]], StringLength[text]]
		}
	]

FuzzyAlignments[text_,frags_,delim_:"&"]:=
	Module[{alignment, splitStarts, delimPs, positions},
		If[Length[frags]===0,Return[{}]];
		alignment = SequenceAlignment[
				text,
				StringRiffle[frags, {delim,delim,delim}],
				MergeDifferences->True,
				Method->"Global",
				GapPenalty->2
				(*Method->"AlignByLongestCommonSequence"*)
				(*SimilarityRules->{{"",_}->-3}*)
				(*GapPenalty->1*)
			];
		splitStarts = Most@Prepend[Accumulate[StringLength@First[#,#]&/@alignment], 1];
		delimPs = Position[alignment, {_,s_/;StringContainsQ[s,"&"]}, {1}][[All,1]];
		positions = Take[splitStarts,{#1+1,#2}][[{1,-1}]]+{1,0}& @@@ Partition[delimPs,2,1];

		positions = MapThread[
			Function[{ps,f},
				If[EditDistance[StringTake[text,ps],f] === 0,
					ps,
					fuzzyAlignSingle[StringTake[text,ps],f]+ps[[1]]-1
				]],
				{positions, frags}
			];

		positions
	]

End[];
EndPackage[];