\name{includePath}
\alias{includePath}
\alias{\S4method{includePath}{BSysProject}}
\title{
  Get the Path to the Project Include Files
}
\description{
  Calling \code{includePath} will return the fully qualified path to the include
  file folder of the project specification in the S4 \code{BSysProject} object instance.
}
\usage{
  \S4method{includePath}{BSysProject}(.Object)
}
\arguments{
  \item{.Object}{
    \code{.Object} is an object instance of class \code{BSysProject} that describes the code project.
  }
}
\value{
  returns the fully qualified path to the include file folder of the project.
}
\author{
  \packageAuthor{BuildSys}

  Maintainer: \packageMaintainer{BuildSys}
}
\seealso{
  \code{\link{libraryPath}}
  \code{\link{sourcePath}}
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
