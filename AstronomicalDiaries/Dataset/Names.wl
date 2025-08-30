BeginPackage["AstronomicalDiaries`Dataset`Names`"];

objectNames
normalStarNames
referenceNames
relationNames

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Dataset`"]


objectNames := objectNames =
	<|
		"Mercury" -> Entity["Planet", "Mercury"],
		"Venus" -> Entity["Planet", "Venus"],
		"Mars" -> Entity["Planet", "Mars"],
		"Jupiter" -> Entity["Planet", "Jupiter"],
		"Saturn" -> Entity["Planet", "Saturn"],
		"moon" -> Entity["PlanetaryMoon", "Moon"],
		"the moon" -> Entity["PlanetaryMoon", "Moon"],
		"Sirius" -> Entity["Star", "Sirius"]
	|>;


	(* TODO: Double check this list *)
normalStarNames := normalStarNames =
	<|
		"\[Eta] Piscium" -> Entity["Star", "EtaPiscium"],
		"\[Beta] Arietis" -> Entity["Star", "Sheratan"],
		"\[Alpha] Arietis" -> Entity["Star", "Hamal"],
		"\[Eta] Tauri" -> Entity["Star", "Alcyone"],
		"\[Alpha] Tauri" -> Entity["Star", "Aldebaran"],
		"\[Beta] Tauri" -> Entity["Star", "Alnath"],
		"\[Zeta] Tauri" -> Entity["Star", "ZetaTauri"],
		"\[Eta] Geminorum" -> Entity["Star", "Propus"],
		"\[Mu] Geminorum" -> Entity["Star", "Tejat"],
		"\[Gamma] Geminorum" -> Entity["Star", "Alhena"],
		"\[Alpha] Geminorum" -> Entity["Star", "Castor"],
		"\[Beta] Geminorum" -> Entity["Star", "Pollux"],
		"\[Eta] Cancri" -> Entity["Star", "EtaCancri"],
		"\[Theta] Cancri" -> Entity["Star", "ThetaCancri"],
		"\[Gamma] Cancri" -> Entity["Star", "AsellusBorealis"],
		"\[Delta] Cancri" -> Entity["Star", "AsellusAustralis"],
		"\[Epsilon] Leonis" -> Entity["Star", "EpsilonLeonis"],
		"\[Alpha] Leonis" -> Entity["Star", "Regulus"],
		"\[Rho] Leonis" -> Entity["Star", "RhoLeonis"],
		"\[Theta] Leonis" -> Entity["Star", "Chort"],
		"\[Beta] Virginis" -> Entity["Star", "Alaraph"],
		"\[Gamma] Virginis" -> Entity["Star", "Porrima"],
		"\[Alpha] Virginis" -> Entity["Star", "Spica"],
		"\[Alpha] Librae" -> Entity["Star", "Alpha1Librae"],
		"\[Beta] Librae" -> Entity["Star", "Zubeneshamali"],
		"\[Delta] Scorpii" -> Entity["Star", "Dschubba"],
		"\[Beta] Scorpii" -> Entity["Star", "Beta2Scorpii"],
		"\[Alpha] Scorpii" -> Entity["Star", "Antares"],
		"\[Theta] Ophiuchi" -> Entity["Star", "ThetaOphiuchi"],
		"\[Beta] Capricorni" -> Entity["Star", "Dabih"],
		"\[Gamma] Capricorni" -> Entity["Star", "Nashira"],
		"\[Delta] Capricorni" -> Entity["Star", "DenebAlgiedi"],
		"\[Pi] Scorpii" -> Entity["Star", "PiScorpii"] (*not mentioned in ADART intro, but appears in texts*)
   |>;


referenceNames := referenceNames =
	<|
		objectNames,
		normalStarNames,
		"\[CurlyEpsilon] Leonis" -> normalStarNames["\[Epsilon] Leonis"],
		"\[CurlyTheta] Ophiuchi" -> normalStarNames["\[Theta] Ophiuchi"],
		"\[CurlyTheta] Leonis" -> normalStarNames["\[Theta] Leonis"],
		"\[CurlyTheta] Cancri" -> normalStarNames["\[Theta] Cancri"],
		"a Virginis" -> normalStarNames["\[Alpha] Leonis"],
		"a Librae" -> normalStarNames["\[Alpha] Librae"]
	|>;


relationNames := relationNames =
	<|
		"above" -> "Above",
		"below" -> "Below",
		"in front of" -> "InFrontOf",
		"behind" -> "Behind",
		"north" -> "North",
		"south" -> "South",
		"east" -> "East",
		"west" -> "West"
	|>;



End[];
EndPackage[];