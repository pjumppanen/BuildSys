\name{objPath}
\alias{objPath}
\alias{\S4method{objPath}{BSysProject}}
\title{
  Get the Path to the Project Object Files
}
\description{
  Calling \code{objPath} will return the fully qualified path to the object
  file folder of the project specification in the S4 \code{BSysProject} object instance.
  The object file folder will contain the makefile and compiled (object) \code{.o} files as
  well as the linked shared library.
}
\usage{
  \S4method{objPath}{BSysProject}(.Object)
}
\arguments{
  \item{.Object}{
    \code{.Object} is an object instance of class \code{BSysProject} that describes the code project.
  }
}
\value{
  returns the fully qualified path to the object file folder of the project specification.
}
\author{
  \packageAuthor{BuildSys}

  Maintainer: \packageMaintainer{BuildSys}
}
\seealso{
  \code{\link{libraryPath}}
  \code{\link{sourcePath}}
  \code{\link{includePath}}
  \code{\link{installLibraryPath}}
  \code{\link{installIncludePath}}
}
\note{
  see \code{\link{BuildSys-package}} for examples of use.
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
