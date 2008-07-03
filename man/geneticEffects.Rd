\name{Genetic effects}
\alias{geneticEffects}
\alias{effectsPvalues}
\alias{effectsVariances}

\title{ Genetic Effects }
\description{
  \code{geneticEffects} displays the genetic effects (and their standard 
  errors) from the result of \code{\link{linearRegression}}. If a new
  reference point is provided, a "change of reference" operation is performed
  (Alvarez-Castro and Carlborg 2007). 
  
  \code{effectsPvalues} and \code{effectsVariances} display respectively
  the P-value (probability for the effect to be = 0) and the part of 
  genetic variance due to this effect.  
}
\usage{
geneticEffects(obj, reference="P1", ref.genotype = NULL)
effectsVariances(obj)
effectsPvalues(reg)
}
\arguments{
  \item{obj}{ An object of class \code{"noia.linear"} provided by 
  \code{\link{linearRegression}}. }
  \item{reference}{ The new reference point. Can
  be \code{"F2"}, \code{"F1"}, \code{"Finf"}, \code{"P1"}, \code{"P2"} (see
  \code{\link{linearRegression}} for details. }
  \item{ref.genotype}{ The same as \code{reference}, provided for compatibility
  with older versions.}
  \item{reg}{ Output of a regression (object of class "\code{\link{lm}}" or 
  "\code{\link{nls}}"). }
}
\details{
  The P-values can be extracted from both linear and multilinear regressions. 
  However, variance decomposition and change of reference operation are not possible
  from the result of a multilinear regression.
}

\references{ 
Alvarez-Castro JM, Carlborg O. (2007). A unified model for functional 
	and statistical epistasis and its application in quantitative trait 
	loci analysis. Genetics 176(2):1151-1167.

Le Rouzic A, Alvarez-Castro JM. (2008). Estimation of genetic effects and
	genotype-phenotype maps. Evolutionary Bioinformatics, 4. 
}

\author{Arnaud Le Rouzic <a.p.s.lerouzic@bio.uio.no> }

\seealso{ \code{\link{linearRegression}},   \code{\link{multilinearRegression}}. }

\examples{
map <- c(0.25, -0.75, -0.75, -0.75, 2.25, 2.25, -0.75, 2.25, 2.25)
pop <- simulatePop(map, N=500, sigmaE=0.2, type="F2")

# Regressions

linear <- linearRegression(phen=pop$phen, gen=cbind(pop$Loc1, pop$Loc2))

geneticEffects(linear, "P1")
effectsVariances(linear)
}

\keyword{ regression }

