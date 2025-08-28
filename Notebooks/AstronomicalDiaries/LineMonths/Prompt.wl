BeginPackage["AstronomicalDiaries`LineMonths`Prompt`"];

prompt

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`LineMonths`FewShotExamples`"]

prompt := prompt =
"Below is text from an ancient Babylonian tablet containing astronomical observations. For each line in the text, identify the month and year that the line covers, and return the result in a JSON format.

## Instructions
1. Text structure
	- The text is organized in sections, typically relating to a single month. Paragraph breaks (two line breaks) indicate section breaks.
	- Damaged portions of text are marked with square brackets and '...'
	- Sometimes, a month section will end in the middle of a damaged portion. You have to logically determine if this happened by context. Often, a new section starts when there is damage when transitioning from the obverse to the reverse of the tablet.
2. Determine the year
	- You are given a year guide which hints at what year this text covers. If there is no other information about the year, just follow the year guide.
	- If the text explicitly states the year it covers, then follow it.
	- Years are always written as a dynasty followed by a number. For example 'SE 30' or 'ArtII 26'.
3. Determine the month
	- Each section usually covers a single month.
	- Allowed months are I, II, III, IV, V, VI, VI2, VII, VIII, IX, X, XI, XII, XII2. Only write months in the allowed format.
	- The beginning of each section sometimes says what month it covers.
	- If no month is stated in the text, refer to the month guide to determine the month. The month guide will look something like this: '[I] II III [ ] VII-X ['
		+ Each roman numeral like 'II' or 'VI' in the month guide corresponds to a section in the text.
		+ Bracketed roman numerals like '[IV]' correspond to sections that were destroyed and no longer appear in the text. For example, if the month guide was '[V] VI VII [VIII] IX X', then the months that should appear are 'VI, VII, IX, X'.
		+ Empty brackets like '[ ]' represent that the intervening months are missing, or that the corresponding sections in the text could not be dated. For example, if month IV appears, then there is damage, and then after the damage month VI appears, the guide might say 'IV [ ] VI'.
		+ Hyphenated guides like 'VII-X' represent that all months from VII to X appear in sequence. This should be interpreted as the same as 'VII VIII IX X'.
	- Every month that appears in the text is represented in the month guide. Only assign a month to a line if that month appears in the month guide.
	- Use logic to figure out when one month ends and the next begins. For example, if you see an observation on the 25th, and then some damage, and then the next observation is on the 3rd, a new month probably started.
	- If the month is ambiguous, or if the month guide says something like '...' or '[...]' or '[ ]' for that section, write null in the corresponding JSON field.
4. Show your reasoning
	- Write an out your reasoning and an explanation of how you assigned lines to months. Make it concise, but explain how you determined where the months breaks are, and how they relate to the month guide.
	- At the start of the explanation, enumerate all the months that the guide implies are mentioned in the text. Then, at the end of the explanation, write the range of lines that each of those months corresponds to.
	- After analyzing the month guide, give the line numbers where all of the explicit month specifications occur in the text. For example, the lines where the text says 'Month XI, the 1st (of which ...'
5. Output format
	- Return the results in a JSON format WITH NO LINE BREAKS.
	- Return a JSON object of the form `{'explanation':'...','results':[...]}` where the array contains an object for EVERY LINE in the input text, with the format like: `{\"line\":\"o 3\",\"month\":\"XI\",\"year\":\"SE 30\"}`.

" <> examplePrompt;

End[];

EndPackage[];