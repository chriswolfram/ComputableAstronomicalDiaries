BeginPackage["AstronomicalDiaries`Utilities`"];

importUTF8JSON
exportUTF8JSON

Needs["AstronomicalDiaries`"]

Begin["`Private`"];

importUTF8JSON[str_] := ImportString[FromCharacterCode[ToCharacterCode[str, "UTF8"]], "RawJSON"]

exportUTF8JSON[json_] := FromCharacterCode[ToCharacterCode@ExportString[json /. _?MissingQ -> Null, "JSON", "Compact" -> True], "UTF8"]

End[];

EndPackage[];