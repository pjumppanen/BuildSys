\name{BSysSourceFile-class}
\Rdversion{1.1}
\docType{class}
\alias{BSysSourceFile-class}

\title{Class \code{"BSysSourceFile"}}
\description{
This class is for internal use only and is used to track the source files and
corresponding dependecies that make up a \code{BSysProject}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BSysSourceFile", Filename, SrcFolder, IncludeFolder, Type)}. Users
should not be using this class directly. Instances of this class are used to track the source files and
corresponding dependecies that make up a \code{BSysProject}. 
}
\section{Slots}{
  \describe{
    \item{\code{Filename}:}{Object of class \code{"character"} is the file name and path of a file in a \code{BSysProject} object instance. }
    \item{\code{Type}:}{Object of class \code{"character"} is a string naming the source file type (\code{"c"}, \code{"cpp"} or \code{"f"}). }
    \item{\code{Dependencies}:}{Object of class \code{"list"} is a list of fully qualified include file dependencies for this source file. }
    \item{\code{Externals}:}{Object of class \code{"list"} is a list of external include file dependencies (the file is not in the \code{\link{includePath}}. }
  }
}
\section{Methods}{
No methods defined with class "BSysSourceFile" in the signature.
}
\author{
\packageAuthor{BuildSys}

Maintainer: \packageMaintainer{BuildSys}
}
\keyword{classes}
