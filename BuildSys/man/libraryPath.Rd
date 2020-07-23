\name{libraryPath}
\alias{libraryPath}
\alias{\S4method{libraryPath}{BSysProject}}
\title{
  Get the Path to the Built Shared Library
}
\description{
  Calling \code{libraryPath} will return the fully qualified path to the built
  shared library of the project specification in the S4 \code{BSysProject} object instance.
}
\usage{
  \S4method{libraryPath}{BSysProject}(.Object)
}
\arguments{
  \item{.Object}{
    \code{.Object} is an object instance of class \code{BSysProject} that describes the code project.
  }
}
\value{
  returns the fully qualified path to the built shared library of the project specification.
}
\author{
  \packageAuthor{BuildSys}

  Maintainer: \packageMaintainer{BuildSys}
}
\seealso{
  \code{\link{sourcePath}}
  \code{\link{includePath}}
  \code{\link{objPath}}
  \code{\link{installLibraryPath}}
  \code{\link{installIncludePath}}
}
\note{
  see \code{\link{BuildSys-package}} for examples of use.
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
