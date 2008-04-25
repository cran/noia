\name{Effects names}

\alias{effectsNamesGeneral}
\alias{effectsNamesMultilinear}
\alias{effectsSelect}
\alias{effNames}
\alias{effectsNames}

\title{Names of Genetic Effects}

\description{
  Provides and manipulates labels of geneticEffects.
}
\usage{
effectsNamesGeneral(nloc = 2)	
effectsNamesMultilinear(nloc=2, max.level=2, max.dom=2)
effectsSelect(nloc, max.level=NULL, max.dom=NULL)
effNames(effects=NULL, loci=NULL, nloc=1)
}

\arguments{
  \item{nloc}{ Number of loci. }
  \item{max.level}{ Maximum level of interactions. }
  \item{max.dom}{ Maximum level of dominance. }
  \item{effects}{ Vector of characters. }
  \item{loci}{ Vector of locus positions. }
}
\details{
	The codes for geneticEffects are stored into a vector of length 4, 
	\code{effectsNames}. The first element of the vector is the code for 
	the absence of effect (default: \code{"."}). The three other elements are
	respectively additive effects (default: \code{"a"}) dominance effects 
	(default: \code{"d"}), and multilinear epistatic effects (default: 
	\code{"e"}). 
	
	The names of geneticEffects contains as many characters as the number of
	loci in the system. The additive effect of the first locus in a 3-locus
	system will be \code{"a.."}, and the "Dominance by Dominance" between loci 2 
	and 4 in a 5-locus system will be \code{".d.d."}. Directionality of epistasis
	between two (or more) loci is indicated by as many \code{"e"} as necessary 
	(e.g. \code{".ee."} for the interaction between loci 2 and 3 in a 4-locus
	case). 

  \code{effectsNamesGeneral} and \code{effectsNamesMultilinear} provide 
  a list of the names of the geneticEffects, in the right orders to be
  processed by the NOIA framework (Alvarez-Castro and Carlborg 2007). 
  
  \code{effNames} is a low-level routine, called by the other functions. It
  provides names "on demand", for instance \code{effNames(c("a","d"),c(2,4),5)}
  will generate \code{".a.d."}, i.e. an \code{"a"} at locus 2 and a \code{"d"}
  at locus 4, in a set of 5 loci. 
}

\seealso{ \code{\link{geneticEffects}},   \code{\link{genNames}},   
	\code{\link{linearRegression}},   \code{\link{multilinearRegression}}. }
	
\references{
Alvarez-Castro JM, Carlborg O. (2007). A unified model for functional 
	and statistical epistasis and its application in quantitative trait 
	loci analysis. Genetics 176(2):1151-1167.
	
Le Rouzic A, Alvarez-Castro JM. (2008). Estimation of genetic effects and
	genotype-phenotype maps. Evolutionary Bioinformatics, in press. 
}

\examples{
effectsNamesGeneral(3)
effectsSelect(nloc=3, max.level=1)
  }
}

\keyword{ internal }

