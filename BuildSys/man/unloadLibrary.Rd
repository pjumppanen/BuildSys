\name{unloadLibrary}
\alias{unloadLibrary}
\alias{\S4method{unloadLibrary}{BSysProject}}
\title{
Unload the Built Shared Library
}
\description{
Calling \code{unloadLibrary} will unload the built library of the project specification in the S4 \code{BSysProject} object instance
from the R session.
}
\usage{
  \S4method{unloadLibrary}{BSysProject}(.Object)
}
\arguments{
  \item{.Object}{
    \code{.Object} is an object instance of class \code{BSysProject} that describes the code project.
  }
}
\details{
  Internally this method delagates to \code{dyn.unload}. Refer to the doucmentation of \code{\link{dyn.unload}} for further details.
}
\value{
  this method has no return value.
}
\author{
  \packageAuthor{BuildSys}

  Maintainer: \packageMaintainer{BuildSys}
}
\seealso{
  \code{\link{loadLibrary}}
  \code{\link{dyn.unload}}
}
\note{
  see \code{\link{BuildSys-package}} for examples of use.
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
