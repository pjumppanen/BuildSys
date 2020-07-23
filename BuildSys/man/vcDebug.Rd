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

  Typically when debugging a new library the library requires \emph{R} setup code to initialise as a basis for running some for of library testing.
  With \code{BuildSys} you should perform this initial setup of \emph{R} in the parent \emph{R} session before calling \code{vcDebug} so that you
  never have to initialise \emph{R} in your debug session. Its correct state will have been initialised from the parent \emph{R} session.
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
  see \code{\link{BuildSys-package}} example code for creation of \code{Project} in example below.
}
\examples{
  # open a debug session - assumes Visual Studio Code is installed as directed in the 
  # package documentation. This will open a Visual Studio Code session. In that session
  # open the convolve.c source file and set breakpoints, then select the "run/start debugging"
  # menu to debug. This will start a new R session with the same state as the parent 
  # R session where vcDebug() is first called. In the new debug session run the following
  # command:
  #
  # conv(a, b)
  vcDebug(Project)
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
