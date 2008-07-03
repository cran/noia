\name{Variance decomposition}
\alias{varianceDecomposition}

\title{ Decomposition of Genetic Variance }
\description{
  Variance decomposition in a classical operation in quantitative genetics
  (e.g. Fisher 1918, Lynch and Walsh 1998).
  The genetic variance, i.e. the part of phenotypic variance that can be
  identify as due to genetic factors, can be decomposed into several
  orthogonal components (generally, the part due to additive factors Var(A), 
  to dominance factors Var(D), and to genetic interactions Var(I)).
}
\usage{
varianceDecomposition(obj)
}

\arguments{
  \item{obj}{ An object of class \code{"noia.linear"}, the output of 
  \code{\link{linearRegression}}. }
}
\details{
  The details of the variance decomposition are provided for all levels of
  interaction: Var(A) and Var(D) for marginal effects, Var(AA), Var(AD) and Var(DD) 
  for 2nd order interactions, etc.
}
\value{
	The function only displays the variances in a nice way and does not return
	any object.
}
\references{ 
Alvarez-Castro JM, Carlborg O. (2007). A unified model for functional 
	and statistical epistasis and its application in quantitative trait 
	loci analysis. Genetics 176(2):1151-1167.

Fisher RA. (1918). The correlation between relatives on the supposition of 
	Mendelian inheritance. Thans. Roy. Soc. Edinburgh 52:339-433. 
	
Le Rouzic A, Alvarez-Castro JM. (2008). Estimation of genetic effects and
	genotype-phenotype maps. Evolutionary Bioinformatics, 4. 

Lynch M, Walsh B (1998) Genetics and Analysis of Quantitative Traits. 
Sunderland, MA; Sinauer Associates. 
 }
 
\author{ Arnaud Le Rouzic <a.p.s.lerouzic@bio.uio.no> }

\seealso{ \code{\link{linearRegression}} }
\examples{

map <- c(0.25, -0.75, -0.75, -0.75, 2.25, 2.25, -0.75, 2.25, 2.25)
pop <- simulatePop(map, N=500, sigmaE=0.2, type="F2")

# Regression

linear <- linearRegression(phen=pop$phen, gen=cbind(pop$Loc1, pop$Loc2))

# Variance decomposition
varianceDecomposition(linear)
}

\keyword{ models }
\keyword{ regression }

