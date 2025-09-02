BeginPackage["AstronomicalDiaries`Modeling`Utilities`"];

normalArraySample
logNormalPDF
logSumExp
dirichletSample
logPMFSample

Begin["`Private`"];

Needs["AstronomicalDiaries`"]


(* Efficiently samples from normal distributions in a threaded way *)
normalArraySample[NormalDistribution[mu_List, sigma_List]] :=
	RandomVariate[NormalDistribution[], Length[mu]]*sigma + mu

normalArraySample[n_NormalDistribution] := RandomVariate@n


(* Compute the log pdf of a normal distribution *)
logNormalPDF[NormalDistribution[mu_, sigma_], x_] :=
	-((x - mu)^2/(2 sigma^2)) + 1/2 (-Log[2] - Log[Pi]) - Log[sigma]


logSumExp[m_, level_ : 0] := 
	With[{c = Map[Max, m, {level}]}, 
		c + Log[Total[Exp[m - c], {level + 1}]]
	]


(* Sample a point on a simplex from a Dirichlet distribution *)
dirichletSample[DirichletDistribution[a_]] :=
	With[{v = RandomVariate@DirichletDistribution[a]}, 
  	Append[v, 1 - Total[v]]
	]


(*
	Alternative version using the Gumbel-max trick:
	https://en.wikipedia.org/wiki/Categorical_distribution#Sampling_via_the_Gumbel_distribution
*)
logPMFSample[logPMF_] := Ordering[RandomVariate[ExtremeValueDistribution[], Length@logPMF] + logPMF, -1][[1]] - 1

logPMFSample[logPMF_, vals_] := vals[[logPMFSample[logPMF]+1]]

(* Direct version *)
(* logPMFSample[logPMF_] := RandomChoice[Exp[logPMF] -> Range[0,Length[logPMF]-1]] *)


End[];
EndPackage[];