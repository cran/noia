\name{NOIA matrix manipulation}

\alias{gen2Z}
\alias{gen2genZ}
\alias{genZ2freq}
\alias{genZ2S}
\alias{genZ2Z}
\alias{genZ2ZS}
\alias{genZ2X}
\alias{checkgenZ}
\alias{Z2freq}
\alias{partialX}
\alias{Sloc}

\title{ Matrix algebra in the NOIA framework. }
\description{
  These functions perform the matrix computation required for the computation
  of genetic effects and Genotype-to-Phenotype mapping. 
}
\usage{
gen2Z(gen)
gen2genZ(gen)
genZ2freq(genZ)
genZ2S(genZ=NULL, reference="F2", nloc=NULL, max.level=NULL, 
	max.dom=NULL)
genZ2Z(genZ)
genZ2ZS(genZ, reference="F2", max.level=NULL, max.dom=NULL, 
	threshold=0)
genZ2X(genZ, reference="F2", max.level=NULL, max.dom=NULL)
checkgenZ(genZ, tol=0.0001)
Z2freq(Z)
partialX(genZ, reference="F2", effect)
Sloc(reference="F2", i=NULL, genZ=NULL)
}

\arguments{
  \item{gen}{ The matrix of genotypes, one column per locus, the genotype is 
  coded \code{1}, \code{2}, \code{3}. Missing data are allowed. }
  \item{genZ}{ The matrix of genotypic probabilities, 3 columns per locus (one
  for the probability of each genotype). The sum of probabilities must be 1, 
  and missing data are not allowed. }
  \item{reference}{ The reference of the population. \code{"F2"}, \code{"F1"}, 
  \code{"Finf"}. \code{"P1"}, \code{"P2}, \code{"G2A"}, \code{"UWR"} and
  \code{"noia"} are possible. Default is \code{"F2"}. }
  \item{nloc}{ Number of loci. }
  \item{max.level}{ Maximum level of interactions. }
  \item{max.dom}{ Maximum level for dominance effects. }
  \item{threshold}{ Frequency threshold from which a genotype is taken into account. Values other than 0 might bias the results.} 
  \item{Z}{ A matrix reflecting the genotype of the corresponding observed 
  phenotypes, as defined in Alvarez-Castro and Carlborg 2007. }
  \item{tol}{A tolerance factor, featuring how much the sum of genotypic 
  frequencies can be different from 1.}
  \item{effect}{ The name of a genetic effect (such as \code{".ad"}). }
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
  \item{\code{genZ2Z}}{ Computes the \code{Z} matrix from the genotypic 
  probabilities. See Alvarez-Castro and Carlborg 2007 for more details. }
  \item{\code{genZ2ZS}}{ Computes \code{Z} and \code{S} matrices at the same time. This is highly efficient when many genotypes are not represented in the dataset. The function returns a list of two elements \code{"zmat"} and \code{"smat"}.}
  \item{\code{genZ2X}}{ Computes the product of \code{Z} and \code{S} matrices without building them. This is very efficient when considering only low-level interactions. }
  \item{\code{checkgenZ}}{ Checks the structure of the \code{genZ} matrix. } 
  \item{\code{Z2freq}}{ Computes the multi-locus genotypic frequency over all genotypic
  combinations. }
  \item{\code{partialX}}{ Computes the product of \code{Z} and \code{S} matrices, keeping \code{Z} and \code{S} as small as possible considering a given effect \code{effect}. }
  \item{\code{Sloc}}{ Provides a 3x3 \code{S} matrix, corresponding to one locus. 
  Frequency-dependent reference points will require the \code{genZ} matrix 
  and the index of the locus. }
}

\references{ 
Alvarez-Castro JM, Carlborg O. (2007). A unified model for functional 
	and statistical epistasis and its application in quantitative trait 
	loci analysis. Genetics 176(2):1151-1167.
	
Le Rouzic A, Alvarez-Castro JM. (2008). Estimation of genetic effects and
	genotype-phenotype maps. Evolutionary Bioinformatics, 4. 
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
\keyword{ internal }

