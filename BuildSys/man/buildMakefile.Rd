\name{buildMakefile}
\alias{buildMakefile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build a GNU makefile
}
\description{
This method builds a GNU makefile based on the project specification in the \code{BSysCodeProject} object instance.
}
\usage{
buildMakefile(Object, Force = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Object}{
    \code{Object} is an object instance of class \code{BSysCodeProject} that describes the code project.
}
  \item{Force}{
    \code{Force} is a boolean that when \code{TRUE} forces re-construction of the makefile. 
}
}
\details{
  \code{buildMakefile} constructs a makefile that represents the project specfication in the in the \code{BSysCodeProject} object instance.
  That instance is created with a call to \code{new("BSysCodeProject", ...)} constructor. If the makefile already exists and is in sync with
  \code{Object} then the makefile is left untouched, unless \code{Force=TRUE}, in which case it is written afresh.
}
\value{
  There is no return value for this method.
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
function (Object, Force = FALSE) 
{
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
