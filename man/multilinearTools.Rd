\name{Multilinear tools}

\alias{formulaMultilinear}
\alias{reconstructLinearEffects}
\alias{startingValues}
\alias{startingValuesReg}
\alias{startingValuesNothing}
\alias{startingValuesLinear}
\alias{startingValuesMultilinear}
\alias{bilinearStep}

\title{ Tools for the Multilinear Regression }
\description{
  The functions perform various tasks required by the multilinear 
  regression model.
}
\usage{
formulaMultilinear(nloc=2, max.level=2, max.dom=2, 
		e.unique=FALSE)
reconstructLinearEffects(noia.multilinear)
startingValues(phen, genZ, reference="noia", max.level=2, 
	max.dom=2, fast=FALSE, e.unique=FALSE, start.algo="linear", 
	bilinear.steps=NULL, ...) 
startingValuesReg(reg, max.level=2, max.dom=2, e.unique=FALSE, 
	nloc=NULL)
startingValuesNothing(nloc, max.level=2, max.dom=2, 
	e.unique=FALSE)
startingValuesLinear(noia.linear, max.level=2, max.dom=2, 
	e.unique=FALSE, e.init=TRUE)
startingValuesMultilinear(noia.multilinear, max.level=2, 
	max.dom=2, e.unique=FALSE)
bilinearStep(form, X, phen, marginal, interactions, ...)
}

\arguments{
  \item{nloc}{ Number of loci. }
  \item{max.level}{ Maximum order of interactions. }
  \item{max.dom}{ Maximum order for dominance. }
  \item{e.unique}{ Whether a single interaction term is used for all pairs. }
  \item{e.init}{ Whether starting values for epistatic effects should be calculated (if \code{FALSE}, all epistatic effects are set to 0). }
  \item{phen}{ Vector of phenotypes. }
  \item{genZ}{ The matrix of individual genotypic probabilities in the population. }
  \item{reference}{ The reference point from which the regression is performed. }
  \item{fast}{ Use of the "fast" algorithm. }
  \item{start.algo}{ Algorithm used to compute the starting values. Can be \code{"linear"}, \code{"multilinear"}, \code{"subset"} or \code{"bilinear"}.}
  \item{bilinear.steps}{ Number of calls of the \code{\link{bilinearStep}} function. Ignored if \code{start.algo} is not \code{"bilinear"}.}
  \item{noia.multilinear}{ Object of class \code{"noia.multilinear"} provided 
  by \code{\link{multilinearRegression}}. }
  \item{noia.linear}{ Object of class \code{"noia.linear"} provided by 
  \code{\link{linearRegression}}. }
  \item{reg}{ Object of class \code{"noia.linear"} or \code{"noia.multilinear"}.}
  \item{form}{ A multilinear formula as returned by \code{formulaMultilinear}.}
  \item{X}{ The product between a \code{Z} matrix and a \code{S} matrix. Can be provided directly by \code{\link{genZ2X}}.}
  \item{marginal}{ A list of marginal effects.}
  \item{interactions}{ A list of interaction effects. Should be complementary to \code{marginal}, together accounting for all genetic effects.}
  \item{...}{ Extra parameters to the non-linear regression function \code{\link{nls}}. }
  
}
\details{
  Because of the way the non-linear regression function \code{\link{nls}}
  works, the multilinear formula has to follow a specific form, with 
  specific names for parameters. \code{formulaMultilinear} provides this 
  formula.
  
  \code{reconstructLinearEffects} generates a vector of genetic effects, 
  including general interaction effects (e.g. Additive by Additive etc) from
  the result of a multilinear regression. This is necessary for further 
  computation of the Genotype-to-Phenotype map. 
  
  Finally, \code{startingValues} provide a vector of starting values for the
  multilinear regression, from the result of a linear regression (through the 
  function \code{startingValuesLinear}) or a simplier multilinear regression 
  (through \code{StartingValuesMultilinear}). \code{startingValuesNothing} 
  generates a list of starting values and sets all of them to 0. Starting 
  values are necessary to ensure the convergence of the non-linearRegression
  (\code{\link{nls}}).  
}

\references{
Hansen TF, Wagner G. (2001) Modeling genetic architecture: A multilinear 
	theory of gene interactions. Theoretical Population Biology 59:61-86.

Le Rouzic A, Alvarez-Castro JM. (2008). Estimation of genetic effects and
	genotype-phenotype maps. Evolutionary Bioinformatics, 4. 
}

\author{ Arnaud Le Rouzic <a.p.s.lerouzic@bio.uio.no> }

\seealso{ \code{\link{multilinearRegression}},   \code{\link{GPmap}}. }

\examples{
set.seed(123456789)

map <- c(0.25, -0.75, -0.75, -0.75, 2.25, 2.25, -0.75, 2.25, 2.25)
pop <- simulatePop(map, N=500, sigmaE=0.2, type="F2")

linear <- linearRegression(phen=pop$phen, gen=pop[2:3])
multilinear <- multilinearRegression(phen=pop$phen, 
	gen=cbind(pop$Loc1, pop$Loc2))

formulaMultilinear(nloc=2)
startingValuesReg(linear)
reconstructLinearEffects(multilinear)
}

\keyword{ nonlinear }
\keyword{ internal } 
