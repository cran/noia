\name{NOIA package}
\alias{noia-package}
\alias{noia}
\docType{package}
\title{
Implementation of the Natural and Orthogonal InterAction (NOIA) model
}
\description{
The NOIA model, as described extensively in Alvarez-Castro & 
	Carlborg (2007), is a framework facilitating the 
	estimation of geneticEffects and genotype-to-phenotype maps. This package
	provides the basic tools to perform linear and multilinear regressions from
	real populations, estimating the genetic effects from different reference
	points, the genotypic values, and the decomposition of genetic variances in
	a multi-locus, 2 alleles system. This package is extensively described in 
	Le Rouzic & Alvarez-Castro (2008).
}
\details{
\tabular{ll}{
Package: \tab noia\cr
Type: \tab Package\cr
Version: \tab 0.92\cr
Date: \tab 2008-07-03\cr
License: \tab GPL-2\cr
}

\item{Data set:}{ The user must provide (i) The vector of phenotypes 
	of all individuals measured in 
	the population, and (ii) The matrix of the genotypes. There are two input 
	formats for the genotype, see \code{\link{linearRegression}}}
\item{Regression functions:}{ \code{\link{linearRegression}} and 
	\code{\link{multilinearRegression}}}
\item{Change of reference:}{ \code{\link{geneticEffects}}}
\item{Genotype-to-phenotype map:}{ \code{\link{GPmap}}}
\item{Decomposition of genetic variance:}{ \code{\link{varianceDecomposition}}}

}

\author{
Arnaud Le Rouzic

Maintainer: Arnaud Le Rouzic <a.p.s.lerouzic@bio.uio.no>
}
\references{
Alvarez-Castro JM, Carlborg O. (2007). A unified model for functional 
	and statistical epistasis and its application in quantitative trait 
	loci analysis. Genetics 176(2):1151-1167.
	
Alvarez-Castro JM, Le Rouzic A, Carlborg O. (2008). How to perform meaningful
	estimates of genetic effects. PLoS Genetics 4(5):e1000062.
	
Le Rouzic A, Alvarez-Castro JM. (2008). Estimation of genetic effects and
	genotype-phenotype maps. Evolutionary Bioinformatics 4. 
}

\examples{
set.seed(123456789)

map <- c(0.25, -0.75, -0.75, -0.75, 2.25, 2.25, -0.75, 2.25, 2.25)
names(map) <- genNames(2)
pop <- simulatePop(map, N=500, sigmaE=0.2, type="F2")

# Regressions

linear <- linearRegression(phen=pop$phen, gen=pop[2:3])

multilinear <- multilinearRegression(phen=pop$phen, gen=cbind(pop$Loc1, 
	pop$Loc2))

# Linear effects, associated variances and stderr
linear

# Multilinear effects
multilinear

# Change of reference: geneticEffects in the "11" genotype (parental 1)
geneticEffects(linear, ref.genotype="P1")

# Variance decomposition
varianceDecomposition(linear)

# GP maps
maps <- cbind(map, GPmap(linear)[,1], GPmap(multilinear)[,1])
colnames(maps) <- c("Actual", "Linear", "Multilinear")
maps
}

\keyword{package}
\keyword{models}
