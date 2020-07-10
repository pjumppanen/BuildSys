\name{initProjectFromFolder}
\alias{initProjectFromFolder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialise a \code{BSysCodeProject} instance based on a project folder
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
initProjectFromFolder(Object, Name = "", WorkingFolder = "", SourceName = "src", IncludeName = "include", ObjName = "obj", InstallName = "obj", Flat = TRUE, Packages = as.character(c()), Includes = as.character(c()), Defines = as.character(c()), Libraries = as.character(c()), CFLAGS = as.character(c()), CXXFLAGS = as.character(c()), FFLAGS = as.character(c()), LDFLAGS = as.character(c()), LDLIBS = as.character(c()), DEFINES = as.character(c()), Debug = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Object}{
%%     ~~Describe \code{Object} here~~
}
  \item{Name}{
%%     ~~Describe \code{Name} here~~
}
  \item{WorkingFolder}{
%%     ~~Describe \code{WorkingFolder} here~~
}
  \item{SourceName}{
%%     ~~Describe \code{SourceName} here~~
}
  \item{IncludeName}{
%%     ~~Describe \code{IncludeName} here~~
}
  \item{ObjName}{
%%     ~~Describe \code{ObjName} here~~
}
  \item{InstallLibraryName}{
%%     ~~Describe \code{InstallName} here~~
}
  \item{InstallIncludeName}{
%%     ~~Describe \code{InstallName} here~~
}
  \item{Flat}{
%%     ~~Describe \code{Flat} here~~
}
  \item{Packages}{
%%     ~~Describe \code{Packages} here~~
}
  \item{Includes}{
%%     ~~Describe \code{Includes} here~~
}
  \item{Defines}{
%%     ~~Describe \code{Defines} here~~
}
  \item{Libraries}{
%%     ~~Describe \code{Libraries} here~~
}
  \item{CFLAGS}{
%%     ~~Describe \code{CFLAGS} here~~
}
  \item{CXXFLAGS}{
%%     ~~Describe \code{CXXFLAGS} here~~
}
  \item{FFLAGS}{
%%     ~~Describe \code{FFLAGS} here~~
}
  \item{LDFLAGS}{
%%     ~~Describe \code{LDFLAGS} here~~
}
  \item{LDLIBS}{
%%     ~~Describe \code{LDLIBS} here~~
}
  \item{DEFINES}{
%%     ~~Describe \code{DEFINES} here~~
}
  \item{Debug}{
%%     ~~Describe \code{Debug} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Object, Name = "", WorkingFolder = "", SourceName = "src", 
    IncludeName = "include", ObjName = "obj", InstallName = "obj", 
    Flat = TRUE, Packages = as.character(c()), Includes = as.character(c()), 
    Defines = as.character(c()), Libraries = as.character(c()), 
    CFLAGS = as.character(c()), CXXFLAGS = as.character(c()), 
    FFLAGS = as.character(c()), LDFLAGS = as.character(c()), 
    LDLIBS = as.character(c()), DEFINES = as.character(c()), 
    Debug = TRUE) 
buildMakefile <- function(Object, Force = FALSE) {
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
