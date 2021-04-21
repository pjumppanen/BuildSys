\name{initProjectFromFolder}
\alias{initProjectFromFolder}
\alias{\S4method{initProjectFromFolder}{BSysProject}}
\title{
  Initialise an S4 \code{BSysProject} Instance based on a Project Folder
}
\description{
  Calling \code{initProjectFromFolder} initialises a project specification in an S4 \code{BSysProject} object instance. That
  object instance can then be used to compile and debug the resulting shared library. A new \code{BSysProject} object instance
  can be constructed by calling \code{new("BSysProject", ...)} where the argument list (...) is the same as for \code{initProjectFromFolder}.
}

\usage{
\S4method{initProjectFromFolder}{BSysProject}(.Object,
                      WorkingFolder = "NULL",
                      Name = "",
                      SourceFiles = "NULL",
                      SourceName = "src",
                      IncludeName = "include",
                      ObjName = "obj",
                      InstallLibraryName = as.character(NULL),
                      InstallIncludeName = as.character(NULL),
                      Flat = TRUE,
                      Packages = as.character(c()),
                      Includes = as.character(c()),
                      Defines = as.character(c()),
                      Libraries = as.character(c()),
                      CFLAGS = as.character(c()),
                      CXXFLAGS = as.character(c()),
                      FFLAGS = as.character(c()),
                      LDFLAGS = as.character(c()),
                      LDLIBS = as.character(c()),
                      DEFINES = as.character(c()),
                      Debug = TRUE)
}

\arguments{
  \item{.Object}{
\code{.Object} is an S4 object instance of class \code{BSysProject} that the code project will be based off.
}
  \item{WorkingFolder}{
The path to the root folder of the project, either relative or absolute and must be supplied.
}
  \item{Name}{
The name given to the project and will form the basename of the shared library.
}
  \item{SourceFiles}{
The a list of file names in WorkingFolder that should be compiled as part of the project or \code{NULL}. If \code{NULL} 
then all compilable source files in WorkingFolder are included. 
}
  \item{SourceName}{
The sub-path / sub-drectory of the source folder (containing \code{.c}, \code{.cpp} files) in the project.
}
  \item{IncludeName}{
The sub-path / sub-drectory of the include folder (containing \code{.h}, \code{.hpp} files) in the project.
}
  \item{ObjName}{
The sub-path / sub-drectory of the object folder (containing the compiled \code{.o} files) in the project.
}
  \item{InstallLibraryName}{
The sub-path / sub-drectory of the folder to install the compiled shared library to.
}
  \item{InstallIncludeName}{
The sub-path / sub-drectory of the folder to install the shared library header files to.
}
  \item{Flat}{
A \code{Boolean} determining if the project is assumed \emph{flat} or \emph{heirarchical}. If \emph{flat}
then all the source and include files are assumed to reside in the \code{WorkingFolder}, otherwise they
are assumed to be in the sub-folders descibed by the \code{SourceName}, \code{IncludeName} and \code{ObjName}
arguments.
}
  \item{Packages}{
A \code{string list} that names the packages that this library is depenedent on (C/C++ include dependencies)
}
  \item{Includes}{
A \code{string list} that contains the include paths of external dependencies of the library code
}
  \item{Defines}{
A \code{string list} that contains preprocessor (\emph{#defines}) defines required to compile the code
}
  \item{Libraries}{
A \code{string list} that names the additional library dependencies needed to link the library. The format is as required
by \emph{gcc}. For instance, \code{"-L/usr/local/lib -lexpat"} would constitute a single item in the list.
}
  \item{CFLAGS}{
A \code{string list} naming additional \emph{gcc} flags to apply when compiling \code{.c} files
}
  \item{CXXFLAGS}{
A \code{string list} naming additional \emph{gcc} flags to apply when compiling \code{.cpp} files
}
  \item{FFLAGS}{
A \code{string list} naming additional \emph{gcc} flags to apply when compiling \code{.f} files
}
  \item{LDFLAGS}{
A \code{string list} naming additional \emph{gcc} flags to apply when linking \code{.o} files
}
  \item{LDLIBS}{
A \code{string list} naming additional \emph{gcc} linker flags to apply when linking \code{.o} files
}
  \item{DEFINES}{
A \code{string list} that contains preprocessor (\emph{#defines}) defines supplied when compiling the code
}
  \item{Debug}{
A \code{Boolean} that indicates whether to create a debug build
}
}
\details{
\code{initProjectFromFolder} constructs a project specification by scanning the \code{WorkingFolder}
and relevant \code{sourceName} sub path for source files to add to the project. If the \code{SourceFiles}
is included the the source file scan is omitted and only the named source files included. The named files
are assumed relative to the Working Folder so if a hierarchical project is being created then the relative
path to the source folder will need to be included in the \code{SourceFiles} list. Any \code{.c}, \code{.cpp}
and \code{.f} files it finds are added to the project. In the case of Fortran files the following extensions
are scanned; \code{.f}, \code{.for}, \code{.f90}, \code{.f95} and \code{.f77}. Any added files are scanned 
for include directives and those includes are added as project dependencies. If the dependency is not found 
in the path described by the \code{includeName} it is regarded as an external dependency.

If the external dependency is known (\code{TMB.hpp} for example) then the appropriate \code{Includes},
\code{Defines} and \code{Libraries} are added to the project definition. \code{BuildSys} is currently
aware of \code{TMB}, \code{Rcpp}, \code{RcppEigen} and the \code{BLAS} library support in R. External
dependencies not know to \code{BuildSys} need to be handle by adding the appropriate \code{Includes},
\code{Defines} and \code{Libraries} to the Project manually with the arguments provided above.

\code{initProjectFromFolder} has useful defaults meaning in most cases very few arguments need be provided.
At most, the \code{WorkingFolder} and \code{Name} arguments will be required but if the project has only
a single source file then even \code{Name} can be omitted as the name will be inferred from the source file name.
}
\value{
returns an S4 object instance of class \code{BSysProject} that describes the code project.
}

\author{
\packageAuthor{BuildSys}

Maintainer: \packageMaintainer{BuildSys}
}

\seealso{
\code{\link{make}}
\code{\link{buildMakefile}}
\code{\link{vcDebug}}
\code{\link{loadLibrary}}
\code{\link{unloadLibrary}}
\code{\link{libraryPath}}
\code{\link{sourcePath}}
\code{\link{includePath}}
\code{\link{objPath}}
\code{\link{installLibraryPath}}
\code{\link{installIncludePath}}
\code{\link{BuildSys-package}}
}
\note{
  see \code{\link{BuildSys-package}} for examples of use.
}

\keyword{debugging}
\keyword{programming}
\keyword{utilities}
