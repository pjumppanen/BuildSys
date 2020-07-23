\name{installLibraryPath}
\alias{installLibraryPath}
\alias{\S4method{installLibraryPath}{BSysProject}}
\title{
  Get the Install Path for the Project Shared Library File
}
\description{
  Calling \code{installLibraryPath} will return the fully qualified path to the install library
  file folder of the project specification in the S4 \code{BSysProject} object instance.
  This is the folder where the shared library will be copied to when calling \code{make(Project, "install")}.
}
\usage{
  \S4method{installLibraryPath}{BSysProject}(.Object)
}
\arguments{
  \item{.Object}{
    \code{.Object} is an object instance of class \code{BSysProject} that describes the code project.
  }
}
\value{
  returns the fully qualified path to the install library file folder of the project specification.
}
\author{
  \packageAuthor{BuildSys}

  Maintainer: \packageMaintainer{BuildSys}
}
\seealso{
  \code{\link{libraryPath}}
  \code{\link{sourcePath}}
  \code{\link{includePath}}
  \code{\link{objPath}}
  \code{\link{installIncludePath}}
}
\note{
  see \code{\link{BuildSys-package}} example code for creation of \code{Project} in example below.
}
\examples{
  # display project install library path
  installLibraryPath(Project)
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
