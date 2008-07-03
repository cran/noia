\name{Simulate population}

\alias{simulatePop}
\alias{drawGenotype}

\title{ Simulates a Population from a Genotype-Phenotype Map }
\description{
  The \code{simulatePop} function takes a Genotype-to-Phenotype map (i.e. a vector
  defining the genotypic value of all possible genotypes) and 
  returns a data frame containing the simulated population. \code{drawGenotype}
  generates a single-individual genotype, and is a sub-routine of 
  \code{simulatePop}.
}
\usage{
simulatePop(gmap, N = 100, sigmaE = 1, type = "F2")
drawGenotype(nloc = 1, type = "F2")
}

\arguments{
  \item{gmap}{ The Genotype-to-phenotype map: a vector of size 3$^L$, where L is
  the number of loci. The vector should be named with the code of each genotype
  (see \code{\link{genNames}}.}
  \item{N}{ Number of individuals. }
  \item{sigmaE}{ Standard deviation of the environmental noise (normally
  distributed). }
  \item{type}{ Type of population. \code{"F2"}, \code{"Finf"} and \code{"F1"}
  are possible. }
  \item{nloc}{ Number of loci. }
}

\value{
	Returns a data frame, in which the first column (\code{$phen}) contains the 
	phenotypes, and the following ones (\code{$Loc1}, \code{$loc2}, etc) the 
	genotypes of all individuals.
}


\references{ 	
Le Rouzic A, Alvarez-Castro JM. (2008). Estimation of genetic effects and
	genotype-phenotype maps. Evolutionary Bioinformatics, 4. 
}

\author{ Arnaud Le Rouzic <a.p.s.lerouzic@bio.uio.no} }

\seealso{ \code{\link{GPmap}},   \code{\link{genNames}} }

\examples{
set.seed(123456789)

map <- c(0.25, -0.75, -0.75, -0.75, 2.25, 2.25, -0.75, 2.25, 2.25)
pop <- simulatePop(map, N=500, sigmaE=0.2, type="F2")
str(pop)
}

\keyword{ datagen }

