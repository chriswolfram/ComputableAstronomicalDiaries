BeginPackage["AstronomicalDiaries`ObservationIdentification`Prompt`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Utilities`"]
Needs["AstronomicalDiaries`ObservationIdentification`FewShotExamples`"]

prompt

Begin["`Private`"];


prompt := prompt = "You will receive a translation of an ancient Babylonian tablet containing astronomical observations. Your task is to extract only the distance-based observations and present them in a concise, structured JSON format.

# Instructions
1. Extract only those observations that describe a distance between an astronomical object and a reference point in the sky, expressed in ancient units (cubits and fingers). Examples: 'the moon was 1/3 cubit 3 fingers in front of \[Alpha] Cancri'.

2. If a line contains multiple, related parts (e.g., 'the moon was 1/3 cubit in front of \[Alpha] Cancri, the moon being 3 fingers low to the south'), treat each as a separate observation.

3. If you are unsure whether a statement is a distance-based observation or not, include it anyway. It is better to include extra (possibly non-observational) entries than to miss real distance observations; err on the side of inclusion for any ambiguous or unclear cases.

4. For each identified observation, extract and return the exact substring corresponding to that observation. Preserve all content including any bracketed or emended sections.

5. If multiple distance observations are present in a single line, treat each as a distinct observation.

6. If there are no observations, return an empty 'observations' array.

After extracting the observations, validate your output to ensure that all potentially qualifying entries have been included per the instructions. If in doubt, include ambiguous cases and proceed.

## Output Format
Return a compact JSON object containing a single key: 'observations', which is an array of exact observation substrings extracted per above (preserving brackets and emendations verbatim).

If no observations are found, return:
{\"observations\":[]}

# Examples

" <> examplePrompt


End[];

EndPackage[];