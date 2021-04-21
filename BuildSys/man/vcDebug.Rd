\name{vcDebug}
\alias{vcDebug}
\alias{\S4method{vcDebug}{BSysProject}}
\title{
  Debug the Shared Library
}
\description{
  Calling \code{vcDebug} will initiate a debug enabled \emph{Microsoft Visual Code} instance
  that can be use as a GUI debugger of your C/C++ code.
}
\usage{
  \S4method{vcDebug}{BSysProject}(.Object, LaunchEditor=TRUE)
}
\arguments{
  \item{.Object}{
    \code{BSysProject} object instance defining the library to be debugged. See \code{\link{BuildSys-package}} for more information.
  }
  \item{LaunchEditor}{
    \code{LaunchEditor} is a reserved argument that should not be used by the user. A call to it with \code{LaunchEditor=FALSE} is used 
    in the \emph{.Rprofile} file for the debug R session to do the necessary tasks to initialise the R session state.
  }
}
\details{
  \code{vcDebug} creates the necessary \code{c_cpp_properties.json} and \code{launch.json} files needed for \emph{Microsoft Visual Code}
  to effectively debug the project dynamic library and launches an instance of \emph{Visual Code}. It is assumed that the dynamic
  library has already been compiled with \code{\link{make}} and \code{Debug} support enabled. R needs to be configured to be able to build
  C/C++ libraries (RTools installed on Windows) and it is assumed that \emph{Microsoft Visual Code} is installed. These requirements
  are discussed at length in \code{\link{BuildSys-package}}.

  Within \emph{Visual Code} you can open the source file/s of your library, set breakpoints and run a debug session. To run a debug
  session select the \emph{run/Start Debugging} menu item. Doing so will result in a new instance of \emph{R} being launched which contains
  the same environment (including loaded packages and loaded dynamic libraries) as the parent \emph{R} session where \code{vcDebug} was first
  called. This \emph{R} session is your \emph{sandbox} to safely debug your library in and leaves the parent \emph{R} session safe from loss should
  your code crash \emph{R} completely.

  Debugging a new library will typically require some \emph{R} setup code to be executed before using and/or testing it. With \code{BuildSys} you 
  should perform this initial setup of \emph{R} in the parent \emph{R} session before calling \code{vcDebug} so that you never have to initialise 
  \emph{R} in your debug session. The debug \emph{R} session will inherit the state of the parent \emph{R} session at the time that \code{vcDebug} 
  was called.
}
\value{
  this method returns no value.
}
\author{
  \packageAuthor{BuildSys}

  Maintainer: \packageMaintainer{BuildSys}
}

\seealso{
  \code{\link{initProjectFromFolder}}
  \code{\link{make}}
  \code{\link{BuildSys-package}}
}

\note{
  see \code{\link{BuildSys-package}} for examples of use.

  If the absolute path to source and dynamic library files contains spaces then debugging in \emph{Visual Code} fails to function correctly.
  To ensure code is debuggable users will need to avoid putting code in folders with whitespace in the folder names.
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
