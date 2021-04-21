\name{make}
\alias{make}
\alias{\S4method{make}{BSysProject}}
\title{
  Make the Shared Library
}
\description{
  Calling \code{make} will \emph{build}, \emph{clean} or \emph{install} your C/C++ library.
}
\usage{
  \S4method{make}{BSysProject}(.Object, Operation = "", Debug = NULL)
}
\arguments{
  \item{.Object}{
    S4 \code{BSysProject} object instance defining the library to be made. See \code{\link{BuildSys-package}} for more information.
  }
  \item{Operation}{
    the \code{make} operation to be carried out. Can be one of,
    \itemize{
      \item{\code{""}} build the library by compiling and linking all source files
      \item{\code{"clean"}} clean the library by deleting all object files (\code{.o} files) and the library itself (\code{.dll} or \code{.so} files)
      \item{\code{"install"}} copy the built library file and/or header files to the install locations (see \code{\link{initProjectFromFolder}} and \code{\link{BuildSys-package}})
    }
  }
  \item{Debug}{
    A \code{Boolean} indicating whether to make a debug build. If \code{NULL} the \code{IsDebug} attribute in the \code{BSysProject} determines
    if a debug build is being made. If non-null the \code{IsDebug} attribute is updated using the \code{Debug} argument.
  }
}
\details{
  Calling \code{make} results in the project described by the \code{Object} instance being transformed into a \emph{GNU makefile} and then
  GNU make being called with the appropriate operation as determined by the \code{Operation} argument. The makefile will be
  written to the sub-folder specified by the \code{ObjName} attribute of the \code{Object} instance. If the makefile already exists it
  will only be re-created if the existing one in not in sync with the project definition. This is determined by an \emph{md5 digest} of
  the project definition which is stored as the header comment line in the makefile. If the makefile is re-written then a \emph{make clean}
  operation will be automatically carried out to ensure the built library remains in sync with the makefile.
}
\value{
  This method returns an updated S4 \code{BSysProject} object instance. If the any change to \code{Debug} state is to be preserved then the
  returned result should be assigned to the calling \code{BSysProject} object instance passed in the \code{Object} argument.
}
\author{
  \packageAuthor{BuildSys}

  Maintainer: \packageMaintainer{BuildSys}
}

\seealso{
  \code{\link{buildMakefile}}
  \code{\link{initProjectFromFolder}}
  \code{\link{BuildSys-package}}
}
\note{
  see \code{\link{BuildSys-package}} for examples of use.
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
