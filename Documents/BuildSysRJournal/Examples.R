# This file contains complete code to reproduce the examples given in the paper,
# "Introduction to BuildSys: An R Package for Compiling and Debugging R Dynamic Libraries"
# 
# R Extensions convolve example case
# ----------------------------------
# R extensions example code
convolve.c <- c(
"#include <R.h>",
"#include <Rinternals.h>",
"",
"SEXP convolve2(SEXP a, SEXP b)",
"{",
"    int na, nb, nab;",
"    double *xa, *xb, *xab;",
"    SEXP ab;",
"",
"    a = PROTECT(coerceVector(a, REALSXP));",
"    b = PROTECT(coerceVector(b, REALSXP));",
"    na = length(a); nb = length(b); nab = na + nb - 1;",
"    ab = PROTECT(allocVector(REALSXP, nab));",
"    xa = REAL(a); xb = REAL(b); xab = REAL(ab);",
"    for(int i = 0; i < nab; i++) xab[i] = 0.0;",
"    for(int i = 0; i < na; i++)",
"        for(int j = 0; j < nb; j++) xab[i + j] += xa[i] * xb[j];",
"    UNPROTECT(3);",
"    return ab;",
"}"
)

# create folder for project and save example code to it
dir.create("~/ConvolveRext")
writeLines(convolve.c, "~/ConvolveRext/convolve.c")

# load BuildSys
library(BuildSys)

# create project and build library
Project <- new("BSysProject", WorkingFolder="~/ConvolveRext", Name="ConvolveRext")
make(Project)
loadLibrary(Project)

# create .Call wrapper
conv <- function(a, b) .Call("convolve2", a, b)

# create test data and execute library implemented convolve
a <- rnorm(100)
b <- rep(1.0, times=10)
conv(a, b)

# debug the library (assumes Visual Studio Code set up as per requirements detailed in ?BuildSys)
vcDebug(Project)


# Rcpp convolve example case
# --------------------------
# Rcpp example code
convolve.cpp <- c(
"#include <Rcpp.h>",
"",
"using namespace Rcpp;",
"",
"NumericVector convolve2(const NumericVector& a,", 
"                        const NumericVector& b)",
"{",
"    int i, j;",
"    int na = a.size();",
"    int nb = b.size();",
"    int nab = na + nb - 1;",
"",
"    NumericVector ab(nab);",
"",
"    for (i = 0 ; i < na ; i++)",
"    {",
"        for (j = 0 ; j < nb ; j++)",
"        {",
"            ab[i + j] += a[i] * b[j];",
"        }",
"    }",
"",
"    return ab;",
"}",
"",
"// Dynamic lib entry point code",
"RcppExport SEXP convolveCpp(SEXP aSEXP, SEXP bSEXP)",
"{",
"    Rcpp::RObject rcpp_result_gen;",
"    Rcpp::RNGScope rcpp_rngScope_gen;",
"    Rcpp::traits::input_parameter< const NumericVector& >::type a(aSEXP);",
"    Rcpp::traits::input_parameter< const NumericVector& >::type b(bSEXP);",
"    rcpp_result_gen = Rcpp::wrap(convolve2(a, b));",
"    return rcpp_result_gen;",
"}"
)

# create folder for project and save example code to it
dir.create("~/ConvolveRcpp")
writeLines(convolve.cpp, "~/ConvolveRcpp/convolve.cpp")

# create project and build library
library(BuildSys)
library(Rcpp)
Project <- new("BSysProject", WorkingFolder="~/ConvolveRcpp", Name="ConvolveRcpp")
make(Project)
loadLibrary(Project)

# create .Call wrapper
conv <- function(a, b) .Call("convolveCpp", a, b)

# Call conv using same a,b as before
#a <- rnorm(100)
#b <- rep(1.0, times=10)
conv(a, b)

# debug the library (assumes Visual Studio Code set up as per requirements detailed in ?BuildSys)
# Use watch expressions:
#   ((a).cache).start[i]
#   ((b).cache).start[j]
#   (((ab).cache).start)[i+j]
vcDebug(Project)


# TMB example case
# --------------------------
# TMB example code
thetalog.cpp <- c(
"// Theta logistic population model from Pedersen et al 2012, Ecol. Modelling.",
"#include <TMB.hpp>",
"template<class Type>",
"Type objective_function<Type>::operator() ()",
"{",
"  /* Data section */",
"  DATA_VECTOR(Y);",
"",
"  /* Parameter section */",
"  PARAMETER_VECTOR(X);",
"  PARAMETER(logr0);",
"  PARAMETER(logtheta);",
"  PARAMETER(logK);",
"  PARAMETER(logQ);",
"  PARAMETER(logR);",
"",
"  /* Procedure section */",
"  Type r0       = exp(logr0);",
"  Type theta    = exp(logtheta);",
"  Type K        = exp(logK);",
"  Type Q        = exp(logQ);",
"  Type R        = exp(logR);",
"  int timeSteps = Y.size();",
"  Type ans      = 0;",
"",
"  for (int i = 1 ; i <= timeSteps ; i++)",
"  {",
"    Type m = X[i - 1] + r0 * (1.0 - pow(exp(X[i - 1]) / K, theta));",
"    ans -= dnorm(X[i], m, sqrt(Q), true);",
"  }",
"",
"  for (int i = 0 ; i <= timeSteps ; i++)",
"  {",
"    ans -= dnorm(Y[i], X[i], sqrt(R), true);",
"  }",
"",
"  return ans;",
"}"
)

thetalog.dat <- c(
"## Number of data points",
"200",
"## Data points",
" 3.023146 3.219844 3.654241 3.604655 3.542303 4.431661 4.291311 4.367869 4.37504 3.945371 4.291201 4.59584 4.302166 4.151933 4.428207 4.305366 4.548615 4.632054 4.784911 4.799618 4.926257 5.136269 5.148587 4.648586 5.29066 4.678948 5.080761 5.223667 5.261118 4.760974 4.789399 5.221183 5.207064 5.20803 5.182091 5.412485 5.815396 5.20875 5.787764 6.08957 5.900544 5.940576 6.214075 5.841425 6.420394 6.466935 6.737423 6.764397 6.806826 6.331149 6.779501 6.721639 6.741977 7.00933 6.658492 6.518948 6.295724 6.34795 6.683175 6.914437 6.275365 6.417207 6.826017 6.587109 6.505463 6.859157 6.678419 6.835149 6.888164 6.829884 6.623776 6.8927 6.609411 6.802324 6.888346 6.928283 7.121756 6.661365 6.7966 6.429909 6.591986 6.775834 6.491397 6.525561 6.956582 6.571743 6.62285 6.405347 6.524295 6.645468 6.148118 6.581335 6.248974 6.047809 6.51518 6.215392 6.994722 6.404407 6.997952 6.822027 6.735724 6.748609 6.308172 6.535844 6.370927 6.475951 6.292657 6.353783 6.538954 6.243774 6.493041 6.300049 6.568226 6.525136 6.751022 6.270681 6.833939 6.488327 6.235697 6.718966 6.614604 6.713407 6.620666 6.501483 6.384192 6.727767 6.769247 6.66819 6.311557 7.059169 6.690371 6.09881 6.803202 6.308663 6.563545 6.390233 6.59533 6.692878 6.682184 6.919849 6.900628 6.514584 6.882142 7.049829 7.076877 7.127657 6.595289 6.714436 6.956282 6.493041 6.764563 6.449598 6.522969 6.47359 6.720563 6.671342 6.198196 6.478438 6.918452 6.791604 6.964608 6.726904 6.490726 6.975198 7.013087 7.254823 6.792741 7.050987 6.951828 6.982148 7.106363 6.980717 7.327641 7.017561 6.512386 6.987826 6.643562 6.671664 7.217902 6.732841 6.509157 6.327128 6.901743 6.584902 6.719805 6.441681 6.742182 6.641601 6.875713 6.726997 7.130124 7.014675 6.922911 6.680622 7.072831 6.649716 7.01744 6.989642 6.710071 6.835827"
)


# create folder for project and save example code to it
dir.create("~/thetalog")
writeLines(thetalog.cpp, "~/thetalog/thetalog.cpp")
writeLines(thetalog.dat, "~/thetalog/thetalog.dat")


# create project, compile library and load into R
library(BuildSys)
library(TMB)
Project <- new("BSysProject", WorkingFolder="~/thetalog", Name="thetalog")
make(Project)
loadLibrary(Project)

# setup test case 
Y          <- scan("~/thetalog/thetalog.dat", skip = 3, quiet = TRUE)
data       <- list(Y = Y)
parameters <- list(X        = data$Y * 0,
                    logr0    = 0,
                    logtheta = 0,
                    logK     = 6,
                    logQ     = 0,
                    logR     = 0)

# start debugging  
vcDebug(Project)

# In debug session execute this
obj <- MakeADFun(data, parameters, random = "X", DLL = "thetalog")
