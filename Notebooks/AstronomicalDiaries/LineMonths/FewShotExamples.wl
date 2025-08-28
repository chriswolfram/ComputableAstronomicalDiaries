BeginPackage["AstronomicalDiaries`LineMonths`FewShotExamples`"];

examplePrompt

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Utilities`"]


examples := examples = {
	{
		"Month Guide: [ ] [VIII] IX X XI [
Year Guide: SE 33
Text:
" <> ADTextData["X102782"][[1]],
		<|
			"explanation" -> "Months implied by guide: IX, X, XI

Explicit months:
o 11': X
r 6' XI

Explanation:
The month [VIII] does not appear, per the month guide. The obverse \
starts with month XI, per the month guide. A new section starts on \
line o 11', and the text indicates that the section covers month X. \
The bottom edge of the tablet is preserved, and the text continues \
onto the reverse until line r 5'. It is a bit ambiguous which month \
lines r 1' through r 5' cover. However, line r 6' shows the start of \
month XI, and because the month guide indicates nothing between \
months X and XI, we can infer that the lines r 1' through r 5' are \
still part of month X. There are no more section breaks, and the rest \
of the tablet covers month XI (per the month guide).

Month line ranges:
IX: o 1' to o 10'
X: o 11' to r 5'
XI: r 6' to r 15'", 
     "results" -> {
					<|"line" -> "o 1'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 2'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 3'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 4'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 5'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 6'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 7'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 8'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 9'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 10'", "month" -> "IX", "year" -> "SE 33"|>,
					<|"line" -> "o 11'", "month" -> "X", "year" -> "SE 33"|>,
					<|"line" -> "o 12'", "month" -> "X", "year" -> "SE 33"|>,
					<|"line" -> "o 13'", "month" -> "X", "year" -> "SE 33"|>,
					<|"line" -> "o 14'", "month" -> "X", "year" -> "SE 33"|>,
					<|"line" -> "b.e. 1", "month" -> Null, "year" -> "SE 33"|>,
					<|"line" -> "r 1'", "month" -> "X", "year" -> "SE 33"|>,
					<|"line" -> "r 2'", "month" -> "X", "year" -> "SE 33"|>,
					<|"line" -> "r 3'", "month" -> "X", "year" -> "SE 33"|>,
					<|"line" -> "r 4'", "month" -> "X", "year" -> "SE 33"|>,
					<|"line" -> "r 5'", "month" -> "X", "year" -> "SE 33"|>,
					<|"line" -> "r 6'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r 7'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r 8'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r 9'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r 10'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r 11'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r 12'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r 13'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r 14'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r 15'", "month" -> "XI", "year" -> "SE 33"|>,
					<|"line" -> "r.e. 1", "month" -> Null, "year" -> "SE 33"|>
				}
		|>
	},
	{
    "Month Guide: I II [ ] VII
Year Guide: SE 193
Text:
" <> ADTextData["X301181"][[1]],
		<|
			"explanation" -> "Months implied by guide: I, II, VII

Explicit months:
o A23: II
r A23': VIII

Explanation:
The month guide indicates that months I, II, and VII are present in \
the text. The text starts with month I after a colophon. The text \
continues with observations that logically follow the sequence of \
days in month I. On line o A23, the text explicitly states that it is \
month II, which aligns with the month guide. The text continues with \
observations that logically follow the sequence of days in month II. \
There is no explicit mention of month VII, but the month guide \
indicates its presence. We can see that month VIII begins on line r \
A23', so month VII must appear before then. At the end of the obverse \
around line o A33 the text is talking about the 24th, but on the \
reverse it is talking about 'Night of the 7th'. This hints that a new \
month started when switching from the obverse to the reverse. From \
this, we deduce that month VII starts on line r A1'. The text on r \
A23' explicitly mentions month VIII, which is not in the month guide.

Month line ranges:
I: o A1 to o A22
II: o A23 to r A22'
VII: r A1'
VIII: r A23'", 
			"results" -> {
					<|"line" -> "o A1", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A2", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B1", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A3", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B2", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A4", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A5", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A6", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B4", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A7", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B5", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B6", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A8", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B7", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A9", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B8", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A10", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B9", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A11", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B10", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A12", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B11", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A13", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A14", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B12", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A15", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A16", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A17", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o B13", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A18", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A19", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A20", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A21", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A22", "month" -> "I", "year" -> "SE 193"|>,
					<|"line" -> "o A23", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A24", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A25", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A26", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A27", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A28", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A29", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A30", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A31", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A32", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "o A33", "month" -> "II", "year" -> "SE 193"|>,
					<|"line" -> "r A1'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B2'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A2'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B3'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A3'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B4'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A4'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A5'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B5'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B6'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A7'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B7'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A8'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B8'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A9'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A10'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A11'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B10'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A12'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A13'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A14'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A15'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B12'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A16'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r B13'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A17'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A18'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A19'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A20'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A21'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A22'", "month" -> "VII", "year" -> "SE 193"|>,
					<|"line" -> "r A23'", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "r A24'", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "r A25'", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "t.e. A1", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "t.e. B1'", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "t.e. B2'", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "t.e. B3'", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "t.e. B4'", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "t.e. B5'", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "t.e. B6'", "month" -> Null, "year" -> "SE 193"|>,
					<|"line" -> "l.e. A1'", "month" -> Null, "year" -> "SE 193"|>
				}
		|>
	}
};


examplePrompt := examplePrompt = 
	StringRiffle[
		"### Input\n\n" <> #1 <> "\n\n### Output\n\n" <> exportUTF8JSON[#2] & @@@ examples,
		"\n\n\n"
	]


End[];
EndPackage[];