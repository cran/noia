\name{matrix2list}
\alias{matrix2list}

\title{ Matrix to List Transformation }

\description{
  Turns a matrix into a list. Each column of the matrix will become an element 
  of the list.
}
\usage{
matrix2list(mat)
}

\arguments{
  \item{mat}{ A matrix, with named columns. }
}

\value{
	A list named according to the names of the columns of the matrix.
}

\note{
	This function is an internal function of the package \code{"noia"}, and is
	not designed to be used in another context.
}

\examples{
	m <- matrix (c(1,2,3,4,5,6), ncol=2)
	colnames(m) <- c("First", "Second")
	matrix2list(m)
}

\keyword{ list }
\keyword{ manip }

