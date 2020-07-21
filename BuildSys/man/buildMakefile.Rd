\name{buildMakefile}
\alias{buildMakefile}
\alias{\S4method{buildMakefile}{BSysProject}}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Build a GNU makefile
}
\description{
  Calling \code{buildMakefile} builds a GNU makefile based on the project specification in the S4 \code{BSysProject} object instance.
}
\usage{
  \S4method{buildMakefile}{BSysProject}(.Object, Force = FALSE)
}
\arguments{
  \item{.Object}{
    \code{.Object} is an object instance of class \code{BSysProject} that describes the code project.
}
  \item{Force}{
    \code{Force} is a boolean that when \code{TRUE} forces re-construction of the makefile.
  }
}
\details{
  \code{buildMakefile} constructs a makefile that represents the project specfication in the in the S4 \code{BSysProject} object instance.
  That instance is created with a call to \code{new("BSysProject", ...)} constructor (see \code{\link{initProjectFromFolder}}). If the makefile
  already exists and is in sync with \code{Object} then the makefile is left untouched, unless \code{Force=TRUE}, in which case it is written afresh.
  A makefile is determined to be in sync with the parent project by comparison of an \emph{md5 digest} of the \code{BSysProject} object instance
  and the digest stored in the header comment line of the makefile.
}
\value{
  There is no return value for this method.
}
\author{
  \packageAuthor{BuildSys}

  Maintainer: \packageMaintainer{BuildSys}
}

\seealso{
  \code{\link{make}}
  \code{\link{initProjectFromFolder}}
  \code{\link{BuildSys-package}}
}
\examples{
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
