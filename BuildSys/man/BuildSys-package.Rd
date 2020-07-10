\name{BuildSys-package}
\alias{BuildSys-package}
\alias{BuildSys}
\docType{package}
\title{
\packageTitle{BuildSys}
}
\description{
\packageDescription{BuildSys}
}
\details{
In the standard R approach dynamic libraries are typically built using the R CMD interface. 
That interface in turn uses the GNU make system to build the library but only in a transient fashion.
In other words, a makefile is constructucted based off a bundled template, GNU make is executed to 
compile and link the library and the makefile deleted post completion. Whilst this is a reasonable
approach for simple one source file projects it bypasses the benefits of a makefile in the case
of more complex ones.

For example, consider the case of developing a library with multiple interface functions contained 
within, with each function implementation implemented in a separate source file. Lets say there are 
five of these. With the traditional R approach with each change in code to one of the source files
all the source files will be re-built and linked because the makefile is transient and re-created
every time. If we were to construct a traditional makefile and use make in the more typical manner 
then only the files that need re-compilation will be re-compiled. In complex projects this saves
a considerable amount of time. 

Moving beyond that, it is common to have build issues with libraries not linking because of missing
dependencies. Resolving these link related issues is made more complex by having a transient makefile
because it is no longer possible to simply look at the makefile definition and make modifications
to it in an attempt to resolve the build problem. By having a system approach whereby makefiles are 
the centre of the build process and are not transient, the process of debugging build issues is made
simpler. 

Finally, a major part of developing library code is debugging and standard R provides little support
for that. Developers typically rely on gdb / lldb command line driven debugger support, which is 
daunting for an inexperienced user, so much so that many will simply resort to print statement based
debugging approaches, which whilst capable of resolving bugs is a cumbersome and slow approach. What 
is required is an easy to use and full featured GUI based debugger and to that end, Microsoft Visual code
coupled with the machine interface (/MI) support in gdb / lldb provide the perfect solution. 

BuildSys provides a simple but complete and flexible way to create project makefiles, build libraries 
and launch GUI based debug sessions, all from within R. The makefiles it creates are permanent rather
than transitory so in the event of build troubles it is east to resort to tinkering with the makefile 
to resolve the cause of the build problem and then reflect the necessary fixes in the R based project 
definition.

\subsection{Project Creation}{
BuildSys encapsulates a C/C++ dynamic library project definition into an S4 class \code{BSysProject}. 
To create a new project we simply create a new instance of that class and BuildSys will construct
the appropriate project definition. For example,

\code{Project <- new("BSysProject")}

will search the R working directory for source files (\cod{.c, .cpp} etc) and add them to the project definition.
In searching for source files it will also scan the source for include statements and any found includes
will be saved as dependencies for the give source file. No attempt is made to properly parse the source so
if the code uses conditional preprocessing statements the situation may arrise that an include file is 
marked as a dependency when for the given build configuration it may not be. In any case, this will not 
affect the ability to correctly build the library but just means the dependencies are marked large than true.

Another feature of the dependency check is that any include files not found are considered externalities and 
if they belong to a known set, will result in the automatic addition of library and include path dependencies 
for the project. Currently BuildSys knows about \code{TMB.hpp, Rcpp.hpp} and \code{RcppEigen.hpp} so if any of these are
included into the source file then the necessary include file and define dependencies will be added. This added
feature means TMB and Rcpp users have minimalistic usage requirements to construct libraries with BuildSys and
need not provide any additional information other than the source files being compiled. 

A typical project will also need to be given a name but if there is only one source file then the name 
can be omitted and is inferred from the filename of the source file. For instance, we can explicitly
name the project with,

\code{Project <- new("BSysProject", Name="MyProject")}

If our source code resides in a different folder we can specify that as well, either as an absolute path or one 
relative to the R working directory. For example,

\code{Project <- new("BSysProject", Name="MyProject", WorkingFolder="MyFolder")}

Finally, if our project is hierarchical in its structure (eg. having source in one folder, header files in another folder, 
and binary output in yet another) BuildSys can accomodate that too. In such case we set \code{Flat=FALSE} with,

\code{Project <- new("BSysProject", Name="MyProject", WorkingFolder="MyFolder", Flat=FALSE)}

For this case the relevent folders are,

\code{MyFolder/src}

\code{MyFolder/include}

\code{MyFolder/obj}

These sub folder names are the default behaviour. If these sub folder names are not as required they can be renamed 
using the \code{SourceName}, \code{IncludeName} and \code{ObjName} arguments in the \code{BSysProject} initilizer. 
That is,

\code{Project <- new("BSysProject", Name="MyProject", WorkingFolder="MyFolder", Flat=FALSE, SourceName="source", IncludeName="header")}

Given an existing project, we can also re-initialize it using the \code{initProjectFromFolder} method which has the same 
argument list as the \code{initialize} method (it is acutally called from the \code{initialize} method). For instance, as with the above,

\code{Project <- initProjectFromFolder(Project, Name="MyProject", WorkingFolder="MyFolder", Flat=FALSE, SourceName="source", IncludeName="header")}
}

\subsection{Project Compilation}{
Once we have constructed a Project object compilation is simply a matter of calling the /code{make} method. As with 
traditional GNU makefiles, calling \code{make} with no arguments compiles the dynamic library. Calling make with a 
\code{"clean"} argument erases all object files and the built dynamic library. For example,

\code{make(Project) # Compiles the source files and links the dynamic library}

\code{make(Project, "clean") # deletes all .o object files and the dynamic library}

Since the makefile is permanent a subsequent call to \code{make} for an already built project will return immediately 
as the library is already present and up to date. If a source file is altered then only the files with stale
dependencies will be recompiled and linked into a new dynamic library build. 

Internally \code{make} will call the \code{buildMakefile} method to first construct an up to date makefile representing
the project. Each makefile has a md5 digest in it which will be updated should you alter the project at all and 
this stamp is then used to determine if the makefile needs re-construction. If the makefile is re-constructure during 
a call to \code{make} then a make clean operation will be carried out to ensure the entire project is re-built. This 
should ensure that the state of the project, makefile and the build remains in sync.

Finally, if your project needs to install the library and/or include files to a specific location that is also 
catered for in BuildSys. Simply specify the install locations with the \code{InstallLibraryName} and \code{InstallIncludeName} 
arguments in the BSysProject initializer. For instance,

\code{Project <- new("BSysProject", Flat=F, InstallLibraryName="inst/lib", InstallIncludeName="inst/include")}

}

\subsection{Loading and Unloading the Library}{
Once the project is made using the \code{make} method the dynamic library can be loaded into R memory. We do so using the
\code{loadLibrary} method and we can similarly unload the library using the \code{unloadLibrary} method. The library
name is based apon the project name and the path to the library can be obtained with the \code{libraryPath} method. For example,

\code{loadLibrary(Project) # load the library built by Project}

\code{unloadLibrary(Project) # unload the library built by Project}

\code{libraryPath(Project) # return the path to the library built by Project}
}

\subsection{Debugging the Library}{

}
}
\author{
\packageAuthor{BuildSys}

Maintainer: \packageMaintainer{BuildSys}
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{package}
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}
