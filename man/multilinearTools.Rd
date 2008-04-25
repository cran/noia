\name{Multilinear tools}

\alias{formulaMultilinear}
\alias{reconstructLinearEffects}
\alias{startingValues}
\alias{startingValuesLinear}
\alias{startingValuesMultilinear}

\title{ Tools for the Multilinear Regression }
\description{
  The functions perform various tasks required by the multilinear 
  regression model.
}
\usage{
formulaMultilinear(nloc = 2, max.level = 2, max.dom = 2, e.unique=FALSE)
reconstructLinearEffects(noia.multilinear)
startingValues(reg, max.level = 2, max.dom = 2, e.unique = FALSE)
startingValuesLinear(noia.linear, max.level = 2, max.dom = 2, e.unique = FALSE)
startingValuesMultilinear(noia.multilinear, max.level = 2, max.dom = 2, e.unique = FALSE)
}

\arguments{
  \item{nloc}{ Number of loci. }
  \item{max.level}{ Maximum level of interactions. }
  \item{max.dom}{ Maximum level for dominance. }
  \item{noia.multilinear}{ Object of class \code{"noia.multilinear"} provided 
  by \code{\link{multilinearRegression}}. }
  \item{noia.linear}{ Object of class \code{"noia.linear"} provided by 
  \code{\link{linearRegression}}. }
  \item{reg}{ Object of class \code{"noia.linear"} or \code{"noia.multilinear"}.}
  \item{e.unique}{ Whether a single interaction term is used for all pairs. }
}
\details{
  Because of the way the non-linearRegression function \code{\link{nls}}
  works, the multilinear formula has to follow a specific form, with 
  specific names for parameters. \code{formulaMultilinear} provides this 
  formula.
  
  \code{reconstructLinearEffects} generates a vector of geneticEffects, 
  including general interaction effects (e.g. Additive by Additive etc) from
  the result of a multilinearRegression. This is necessary for further 
  computation of the Genotype-to-Phenotype map. 
  
  Finally, \code{startingValues} provide a vector of starting values for the
  multilinear regression, from the result of a linear regression (through the 
  function \code{startingValuesLinear}) or a simplier multilinear regression 
  (through \code{StartingValuesMultilinear}). Such starting 
  values are necessary to ensure the convergence of the non-linearRegression
  (\code{\link{nls}}).  
}

\references{
Hansen TF, Wagner G. (2001) Modeling genetic architecture: A multilinear 
	theory of gene interactions. Theoretical Population Biology 59:61-86.

Le Rouzic A, Alvarez-Castro JM. (2008). Estimation of genetic effects and
	genotype-phenotype maps. Evolutionary Bioinformatics, in press. 
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
startingValues(linear)
reconstructLinearEffects(multilinear)
}

\keyword{ regression }
\keyword{ nonlinear }

