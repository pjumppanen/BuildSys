\name{loadLibrary}
\alias{loadLibrary}
\alias{\S4method{loadLibrary}{BSysProject}}
\title{
  Load the Built Shared Library
}
\description{
  Calling \code{loadLibrary} will load the built library of the project specification in the S4 \code{BSysProject} object instance
  into the R session.
}
\usage{
  \S4method{loadLibrary}{BSysProject}(.Object)
}
\arguments{
  \item{.Object}{
    \code{.Object} is an object instance of class \code{BSysProject} that describes the code project.
  }
}
\details{
  Internally this method delagates to \code{dyn.load}. Refer to the doucmentation of \code{\link{dyn.load}} for further details.
}
\value{
  returns an object of class \code{DLLInfo}.
}
\author{
  \packageAuthor{BuildSys}

  Maintainer: \packageMaintainer{BuildSys}
}
\seealso{
  \code{\link{unloadLibrary}}
  \code{\link{dyn.load}}
}
\note{
  see \code{\link{BuildSys-package}} for examples of use.
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
