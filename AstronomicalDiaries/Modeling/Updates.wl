BeginPackage["AstronomicalDiaries`Modeling`Updates`"];

normalNormalMeanSample
normalNormalVarianceSample
normalNormalRegressionSample
betaBernoulliSample
dirichletCategoricalSample
binaryNormalMixtureSample

Begin["`Private`"];

Needs["AstronomicalDiaries`"]
Needs["AstronomicalDiaries`Modeling`Utilities`"]


(* Normal mean *)

(*
	Samples mu in the following model:

	mu ~ Normal(mu0, sigma0)
	y ~ Normal(mu, sigma)
*)

normalNormalMeanSample[NormalDistribution[mu0_, sigma0_], sigma_, ys_] :=
	With[{sigman2 = (1/sigma0^2 + Length[ys]/sigma^2)^-1},
		RandomVariate@NormalDistribution[sigman2 (mu0/sigma0^2 + Total[ys]/sigma^2), Sqrt[sigman2]]
	]


(* Normal variance *)

(*
	Samples sigma2 (sigma^2) in the following model:

	sigma2 ~ InverseGamma(alpha, beta)
	y ~ Normal(mu, sigma)s
*)

normalNormalVarianceSample[InverseGammaDistribution[alpha_, beta_], mu_, ys_] :=
	RandomVariate@InverseGammaDistribution[alpha + (1/2) Length[ys], beta + (1/2) Total[(ys - mu)^2]]


(* Normal regression *)

(*
	Samples x in the following model:

	x ~ Normal(mu0, sigma0)
	y ~ Normal(m * x + b, sigma)
*)

(* For many observations for the same x: *)

normalNormalRegressionSample[NormalDistribution[mu0_, sigma0_], sigma_, m_, b_, y_] :=
	With[{sigman2 = (1/sigma0^2 + Total[m^2]/sigma^2)^-1},
		normalArraySample@NormalDistribution[sigman2 (mu0/sigma0^2 + m . (y - b)/sigma^2), Sqrt[sigman2]]
	]

(* For a vector of x_i: *)

normalNormalRegressionSample[NormalDistribution[mu0_List, sigma0_List], sigma_, m_, b_, y_] :=
	With[{sigman2 = (1/sigma0^2 + m^2/sigma^2)^-1},
		normalArraySample@NormalDistribution[sigman2 (mu0/sigma0^2 + (m*(y - b))/sigma^2), Sqrt[sigman2]]
	]


(* Beta-Bernoulli *)

(*
	Samples p in the following model:

	p ~ Beta(alpha, beta)
	x ~ Bernoulli(p)
*)

betaBernoulliSample[BetaDistribution[alpha_, beta_], xs_] :=
	With[{t = Total[xs]},
		RandomVariate@BetaDistribution[alpha + t, beta + Length[xs] - t]
	]


(* Dirichlet-categorical *)

(*
	Samples v in the following model:

	v ~ Dirichlet(a)
	catCounts ~ Categorical(v)
*)

dirichletCategoricalSample[DirichletDistribution[a_], catCounts_] :=
	dirichletSample@DirichletDistribution[a + catCounts]


(* Binary normal mixture *)

(*
	Samples m in the following model:

	m ~ Bernoulli(p)
	x ~ { Normal(mu1, sigma1)   m = 0 }
	    { Normal(mu2, sigma2)   m = 1 }
*)

binaryNormalMixtureSample[BernoulliDistribution[p_], n1_NormalDistribution, n2_NormalDistribution, xs_] :=
	With[{
			p1 = Log[p] + logNormalPDF[n1, xs],
			p2 = Log[1 - p] + logNormalPDF[n2, xs]
		}, {
			(*Exp[p1]/(Exp[p1]+Exp[p2])*)
			mLogProbs = p1 - logSumExp[Transpose@{p1, p2}, 1]
		},
		Boole@MapThread[Greater, {mLogProbs, Log@RandomReal[{0, 1}, Length[mLogProbs]]}]
	]


End[];
EndPackage[];