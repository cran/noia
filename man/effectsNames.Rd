\name{Effects names}

\alias{effectsNamesGeneral}
\alias{effectsNamesMultilinear}
\alias{effectsSelect}
\alias{statusMaxLevel}
\alias{statusMaxDom}
\alias{effNames}
\alias{effectsNames}

\title{Names of Genetic Effects}

\description{
  Provides and manipulates labels of genetic effects.
}
\usage{
effectsNamesGeneral(nloc = 2, max.level=NULL, max.dom=NULL)	
effectsNamesMultilinear(nloc=2, max.level=2, max.dom=2)
statusMaxLevel(effect, max.level=NULL)
statusMaxDom(effect, max.dom=NULL)
effectsSelect(nloc, max.level=NULL, max.dom=NULL, effects=NULL)
effNames(effects=NULL, loci=NULL, nloc=1)
}

\arguments{
  \item{nloc}{ Number of loci. }
  \item{max.level}{ Maximum order of interactions. }
  \item{max.dom}{ Maximum order for dominance. }
  \item{effect}{ A string or characters representing a genetic effect. }
  \item{effects}{ Vector of effects. }
  \item{loci}{ Vector of loci. }
}
\details{
	The codes for genetic effects are stored into a vector of length 4, 
	\code{effectsNames}. The first element of the vector is the code for 
	the absence of effect (default: \code{"."}). The three other elements are
	respectively additive effects (default: \code{"a"}) dominance effects 
	(default: \code{"d"}), and multilinear epistatic effects (default: 
	\code{"e"}). 
	
	The names of genetic effects contains as many characters as the number of
	loci in the system. The additive effect of the first locus in a 3-locus
	system will be \code{"a.."}, and the "Dominance by Dominance" between loci 2 
	and 4 in a 5-locus system will be \code{".d.d."}. Directionality of epistasis
	between two (or more) loci is indicated by as many \code{"e"} as necessary 
	(e.g. \code{".ee."} for the interaction between loci 2 and 3 in a 4-locus
	case). 

	\code{effectsNamesGeneral} and \code{effectsNamesMultilinear} provide 
	a list of the names of the genetic effects, in the correct order to be
	processed in the NOIA framework (Alvarez-Castro and Carlborg 2007). 
	\code{effectsSelect} returns a vector of effects matching the maximum levels.
	
	\code{statusMaxLevel} and \code{statusMaxDom} return \code{TRUE} if a given
	effect is under the level and dominance threshold, respectively.
  
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
	genotype-phenotype maps. Evolutionary Bioinformatics, 4. 
}

\examples{
effectsNamesGeneral(3)
effectsSelect(nloc=3, max.level=1)
  }


\keyword{ internal }
\keyword{ character }

