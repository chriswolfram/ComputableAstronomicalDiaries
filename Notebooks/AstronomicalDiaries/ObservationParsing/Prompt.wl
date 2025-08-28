BeginPackage["AstronomicalDiaries`ObservationParsing`Prompt`"];

prompt

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Utilities`"]
Needs["AstronomicalDiaries`ObservationParsing`FewShotExamples`"]


prompt := prompt =
"You will be given a translated observation from an ancient Babylonian astronomical tablet. Extract the relevant information and present it as concise, structured JSON output.

# Instructions
- Distance observations measure the distance in the sky between astronomical objects in units of cubits and fingers. For example, \"the moon was 1/3 cubit 3 fingers in front of \[Alpha] Cancri\".
- Sometimes, distance observations are combined, e.g., \"the moon was 1/3 cubit in front of \[Alpha] Cancri, the moon being 3 fingers low to the south\". In such cases, infer that the second observation also refers to the same reference (e.g., \[Alpha] Cancri).
- If given a distance observation, extract the observed object, the reference object, the relation, and the distance in cubits and fingers. If given anything other than a distance observation, write null in all JSON fields.
- Preserve original fractional values (like '1/3', '2 2/3') exactly as stated. Do not convert to decimals.
- If any data (such as cubits, fingers, object, relation, or reference) is missing, set that field to null in the JSON output.
- Include only information stated explicitly in the text or context. Do not infer or guess missing details. For any missing or ambiguous field, write null.

After forming the output, validate that all required fields are present in the JSON and each field strictly follows the requirements. If validation fails, self-correct or set fields to null as appropriate.

# Examples

" <> examplePrompt;


End[];

EndPackage[];