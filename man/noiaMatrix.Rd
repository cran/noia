\name{NOIA matrix manipulation}

\alias{gen2Z}
\alias{gen2genZ}
\alias{genZ2freq}
\alias{genZ2S}
\alias{genZ2Z}
\alias{Z2freq}
\alias{Sloc}

\title{ Matrix algebra in the NOIA framework. }
\description{
  These functions perform the matrix computation required for the computation
  of geneticEffects and Genotype-to-Phenotype mapping. 
}
\usage{
gen2Z(gen)
gen2genZ(gen)
genZ2freq(genZ)
genZ2S(type="F2", genZ=NULL, nloc=NULL)
genZ2Z(genZ)
Z2freq(Z)
Sloc(type="F2", i=NULL, genZ=NULL)
}

\arguments{
  \item{gen}{ The matrix of genotypes, one column per locus, the genotype is 
  coded \code{1}, \code{2}, \code{3}. Missing data are allowed. }
  \item{genZ}{ The matrix of genotypic probabilities, 3 columns per locus (one
  for the probability of each genotype). The sum of probabilities must be 1, 
  and missing data are not allowed. }
  \item{type}{ The type of the population. \code{"F2"}, \code{"F1"}, 
  \code{"Finf"}. \code{"P1"}, \code{"P2}, \code{"G2A"}, \code{"UWR"} and
  \code{"noia"} are possible. Default is \code{"F2"}. }
  \item{nloc}{ Number of loci. }
  \item{Z}{ A matrix reflecting the genotype of the corresponding observed 
  phenotypes, as defined in Alvarez-Castro and Carlborg 2007. }
  \item{i}{ Index of the locus. }
}
\details{
  \item{\code{gen2Z}}{ Transforms a \code{gen} data set into a \code{Z} matrix that
  is the data matrix in the regression. The function actually calls sequencially
  \code{gen2genZ} and \code{genZ2Z}. }
  \item{\code{gen2genZ}}{ Transforms a \code{gen} matrix into a \code{genZ} matrix. }
  \item{\code{genZ2freq}}{ Provides a vector representing the frequency of each
  genotypic form at each locus. The sum of the frequency is 1 for each locus. }
  \item{\code{genZ2S}}{ Provides the \code{S} matrix (see Alvarez-Castro and Carlborg 
  2007) for a given reference point. Some reference points are genotypic 
  frequency-dependent (\code{"G2A"} and \code{"noia"}), and the \code{genZ} 
  matrix must be provided. For the others, only the number of loci is 
  necessary. }
  \item{\code{genZ2Z}}{ Conmputes the \code{Z} matrix from the genotypic 
  probabilities. See Alvarez-Castro and Carlborg 2007 for more details. }
  \item{\code{Z2freq}}{ Computes the multi-locus genotypic frequency over all genotypic
  combinations. }
  \item{\code{Sloc}}{ Provides a 3x3 \code{S} matrix, corresponding to one locus. 
  Frequency-dependent reference points will require the \code{genZ} matrix 
  and the index of the locus. }
}

\references{ 
Alvarez-Castro JM, Carlborg O. (2007). A unified model for functional 
	and statistical epistasis and its application in quantitative trait 
	loci analysis. Genetics 176(2):1151-1167.
	
Le Rouzic A, Alvarez-Castro JM. (2008). Estimation of genetic effects and
	genotype-phenotype maps. Evolutionary Bioinformatics, in press. 
}
\author{ Arnaud Le Rouzic <a.p.s.lerouzic@bio.uio.no> }

\seealso{ \code{\link{linearRegression}},   \code{\link{multilinearRegression}} }
\examples{
set.seed(123456789)

map <- c(0.25, -0.75, -0.75, -0.75, 2.25, 2.25, -0.75, 2.25, 2.25)
names(map) <- genNames(2)
pop <- simulatePop(map, N=500, sigmaE=0.2, type="F2")

gen <- pop[2:3]
genZ <- gen2genZ(gen)
Z <- genZ2Z(genZ)
}
\keyword{ manip }

