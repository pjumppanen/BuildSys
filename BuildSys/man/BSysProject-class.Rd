\name{BSysProject-class}
\Rdversion{1.1}
\docType{class}
\alias{BSysProject-class}
\alias{buildMakefile,BSysProject-method}
\alias{includePath,BSysProject-method}
\alias{initProjectFromFolder,BSysProject-method}
\alias{installIncludePath,BSysProject-method}
\alias{installLibraryPath,BSysProject-method}
\alias{libraryPath,BSysProject-method}
\alias{loadLibrary,BSysProject-method}
\alias{make,BSysProject-method}
\alias{objPath,BSysProject-method}
\alias{sourcePath,BSysProject-method}
\alias{unloadLibrary,BSysProject-method}
\alias{vcDebug,BSysProject-method}
\alias{clean,BSysProject-method}

\title{Class \code{"BSysProject"}}
\description{
\code{BSysProject} implements a build system based on GNU make that creates and
maintains (simply) makefiles in an R session and provides GUI debugging support 
through Microsoft Visual Code.
}
\section{Objects from the Class}{
Objects can be created by calls of the form:

\preformatted{
  new("BSysProject", 
      WorkingFolder,
      Name, 
      SourceFiles, 
      SourceName, 
      IncludeName, 
      ObjName, 
      InstallLibraryName, 
      InstallIncludeName, 
      Flat, 
      Packages, 
      Includes, 
      Defines, 
      Libraries, 
      CFLAGS, 
      CXXFLAGS, 
      FFLAGS, 
      LDFLAGS, 
      LDLIBS, 
      DEFINES, 
      Debug)}

For more information consult \code{\link{initProjectFromFolder}}.
}
\section{Slots}{
  \describe{
    \item{\code{WorkingFolder}:}{
      Object of class \code{"character"} that is the absolute path to the root folder of the project.
    }
    \item{\code{ProjectName}:}{
      Object of class \code{"character"} that is the name given to the project and will form the basename of the shared library.
    }
    \item{\code{SourceName}:}{
      Object of class \code{"character"} that is the sub-path / sub-drectory of the source folder (containing \code{.c}, \code{.cpp} files)
      in the project.
    }
    \item{\code{IncludeName}:}{
      Object of class \code{"character"} that is the sub-path / sub-drectory of the include folder (containing \code{.h}, \code{.hpp} files)
      in the project.
    }
    \item{\code{ObjName}:}{
      Object of class \code{"character"} that is he sub-path / sub-drectory of the object folder (containing the compiled \code{.o}
      files) in the project.
    }
    \item{\code{InstallLibraryName}:}{
      Object of class \code{"character"} that is the sub-path / sub-drectory of the folder to install the compiled shared library to.
    }
    \item{\code{InstallIncludeName}:}{
      Object of class \code{"character"} that is the sub-path / sub-drectory of the folder to install the shared library header files to.
    }
    \item{\code{Flat}:}{
      Object of class \code{"logical"} that determines if the project is assumed \emph{flat} or \emph{heirarchical}. If \emph{flat}
      then all the source and include files are assumed to reside in the \code{WorkingFolder}, otherwise they
      are assumed to be in the sub-folders descibed by the \code{SourceName}, \code{IncludeName} and \code{ObjName}
      arguments.
    }
    \item{\code{SourceFiles}:}{
      Object of class \code{"list"} is a \code{"BSysSourceFile" list} that names the source files of the project.
    }
    \item{\code{Packages}:}{
      Object of class \code{"character"} is a \code{string list} that names the packages that this library is depenedent
      on (C/C++ include dependencies).
    }
    \item{\code{Includes}:}{
      Object of class \code{"character"} is a \code{string list} that contains the include paths of external dependencies
      of the library code.
    }
    \item{\code{Defines}:}{
      Object of class \code{"character"} is a \code{string list} that contains preprocessor (\emph{#defines}) defines
      required to compile the code.
    }
    \item{\code{Libraries}:}{
      Object of class \code{"character"} is a \code{string list} that names the additional library dependencies needed
      to link the library. The format is as required by \emph{gcc}. For instance, \code{"-L/usr/local/lib -lexpat"} would
      constitute a single item in the list.
    }
    \item{\code{CFLAGS}:}{
      Object of class \code{"character"} is a \code{string list} naming additional \emph{gcc} flags to apply when
      compiling \code{.c} files.
    }
    \item{\code{CXXFLAGS}:}{
      Object of class \code{"character"} is a \code{string list} naming additional \emph{gcc} flags to apply when
      compiling \code{.cpp} files.
    }
    \item{\code{FFLAGS}:}{
      Object of class \code{"character"} is a \code{string list} naming additional \emph{gcc} flags to apply when
      compiling \code{.f} files.
    }
    \item{\code{LDFLAGS}:}{
      Object of class \code{"character"} is a \code{string list} naming additional \emph{gcc} flags to apply when
      linking \code{.o} files.
    }
    \item{\code{LDLIBS}:}{
      Object of class \code{"character"} is a \code{string list} naming additional \emph{gcc} linker flags to apply
      when linking \code{.o} files.
    }
    \item{\code{DEFINES}:}{
      Object of class \code{"character"} is a \code{string list} that contains preprocessor (\emph{#defines}) defines
      supplied when compiling the code.
    }
    \item{\code{IsDebug}:}{
      Object of class \code{"logical"} is a \code{Boolean} that indicates whether to create a debug build.
    }
    \item{\code{DebugState}:}{
      Object of class \code{"list"} used internally to manage debugging.
    }
  }
}
\section{Methods}{
  \describe{
    \item{buildMakefile}{\code{signature(.Object = "BSysProject")}: see \code{\link{buildMakefile}} }
    \item{includePath}{\code{signature(.Object = "BSysProject")}: see \code{\link{includePath}} }
    \item{initProjectFromFolder}{\code{signature(.Object = "BSysProject")}: see \code{\link{initProjectFromFolder}} }
    \item{installIncludePath}{\code{signature(.Object = "BSysProject")}: see \code{\link{installIncludePath}} }
    \item{installLibraryPath}{\code{signature(.Object = "BSysProject")}: see \code{\link{installLibraryPath}} }
    \item{libraryPath}{\code{signature(.Object = "BSysProject")}: see \code{\link{libraryPath}} }
    \item{loadLibrary}{\code{signature(.Object = "BSysProject")}: see \code{\link{loadLibrary}} }
    \item{make}{\code{signature(.Object = "BSysProject")}: see \code{\link{make}} }
    \item{objPath}{\code{signature(.Object = "BSysProject")}: see \code{\link{objPath}} }
    \item{sourcePath}{\code{signature(.Object = "BSysProject")}: \code{\link{sourcePath}} }
    \item{unloadLibrary}{\code{signature(.Object = "BSysProject")}: \code{\link{unloadLibrary}} }
    \item{vcDebug}{\code{signature(.Object = "BSysProject")}: \code{\link{vcDebug}} }
    \item{clean}{\code{signature(.Object = "BSysProject")}: \code{\link{clean}} }
	 }
}
\author{
\packageAuthor{BuildSys}

Maintainer: \packageMaintainer{BuildSys}
}
\keyword{classes}
