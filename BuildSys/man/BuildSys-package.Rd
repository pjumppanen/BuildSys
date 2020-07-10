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
In the standard \emph{R} approach dynamic libraries are typically built using the \emph{R CMD} interface. 
That interface in turn uses the \emph{GNU make} system to build the library but only in a transient fashion.
In other words, a makefile is constructucted based off a bundled template, \emph{GNU make} is executed to 
compile and link the library and the makefile deleted post completion. Whilst this is a reasonable
approach for simple one source file projects it bypasses the benefits of a makefile in the case
of more complex ones.

For example, consider the case of developing a library with multiple interface functions contained 
within, with each function implementation implemented in a separate source file. Lets say there are 
five of these. With the traditional \emph{R} approach with each change in code to one of the source files
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

Finally, a major part of developing library code is debugging and standard emph{R} provides little support
for that. Developers typically rely on \emph{gdb / lldb} command line driven debugger support, which is 
daunting for an inexperienced user, so much so that many will simply resort to print statement based
debugging approaches, which whilst capable of resolving bugs is a cumbersome and slow approach. What 
is required is an easy to use and full featured GUI based debugger and to that end, \emph{Microsoft Visual Code}
coupled with the machine interface (/MI) support in \emph{gdb / lldb} provide the perfect solution. 

\code{BuildSys} provides a simple but complete and flexible way to create project makefiles, build libraries 
and launch GUI based debug sessions, all from within \emph{R}. The makefiles it creates are permanent rather
than transitory so in the event of build troubles it is east to resort to tinkering with the makefile 
to resolve the cause of the build problem and then reflect the necessary fixes in the \emph{R} based project 
definition.

\subsection{Project Creation}{
\code{BuildSys} encapsulates a C/C++ dynamic library project definition into an S4 class \code{BSysProject}. 
To create a new project we simply create a new instance of that class and \code{BuildSys} will construct
the appropriate project definition. For example,

\code{Project <- new("BSysProject")}

will search the \emph{R} working directory for source files (\code{.c, .cpp} etc) and add them to the project definition.
In searching for source files it will also scan the source for include statements and any found includes
will be saved as dependencies for the give source file. No attempt is made to properly parse the source so
if the code uses conditional preprocessing statements the situation may arrise that an include file is 
marked as a dependency when for the given build configuration it may not be. In any case, this will not 
affect the ability to correctly build the library but just means the dependencies are marked large than true.

Another feature of the dependency check is that any include files not found are considered externalities and 
if they belong to a known set, will result in the automatic addition of library and include path dependencies 
for the project. Currently \code{BuildSys} knows about \code{TMB.hpp, Rcpp.hpp} and \code{RcppEigen.hpp} so if any of these are
included into the source file then the necessary include file and define dependencies will be added. This added
feature means \emph{TMB} and \emph{Rcpp} users have minimalistic usage requirements to construct libraries with \code{BuildSys} and
need not provide any additional information other than the source files being compiled. 

A typical project will also need to be given a name but if there is only one source file then the name 
can be omitted and is inferred from the filename of the source file. For instance, we can explicitly
name the project with,

\code{Project <- new("BSysProject", Name="MyProject")}

If our source code resides in a different folder we can specify that as well, either as an absolute path or one 
relative to the \emph{R} working directory. For example,

\code{Project <- new("BSysProject", Name="MyProject", WorkingFolder="MyFolder")}

Finally, if our project is hierarchical in its structure (eg. having source in one folder, header files in another folder, 
and binary output in yet another) \code{BuildSys} can accomodate that too. In such case we set \code{Flat=FALSE} with,

\code{Project <- new("BSysProject", Name="MyProject", WorkingFolder="MyFolder", Flat=FALSE)}

For this case the relevent folders are,

\preformatted{
MyFolder/src
MyFolder/include
MyFolder/obj
}

These sub folder names are the default behaviour. If these sub folder names are not as required they can be renamed 
using the \code{SourceName}, \code{IncludeName} and \code{ObjName} arguments in the \code{BSysProject} initilizer. 
That is,

\code{Project <- new("BSysProject", Name="MyProject", WorkingFolder="MyFolder", Flat=FALSE, SourceName="source", IncludeName="header")}

Given an existing project, we can also re-initialize it using the \code{initProjectFromFolder} method which has the same 
argument list as the \code{initialize} method (it is acutally called from the \code{initialize} method). For instance, as with the above,

\code{Project <- initProjectFromFolder(Project, Name="MyProject", WorkingFolder="MyFolder", Flat=FALSE, SourceName="source", IncludeName="header")}
}

\subsection{Project Compilation}{
Once we have constructed a \code{Project} object compilation is simply a matter of calling the \code{make} method. As with 
traditional GNU makefiles, calling \code{make} with no arguments compiles the dynamic library. Calling make with a 
\code{"clean"} argument erases all object files and the built dynamic library. For example,

\preformatted{
make(Project) # Compiles the source files and links the dynamic library

make(Project, "clean") # deletes all .o object files and the dynamic library
}

Since the makefile is permanent a subsequent call to \code{make} for an already built project will return immediately 
as the library is already present and up to date. If a source file is altered then only the files with stale
dependencies will be recompiled and linked into a new dynamic library build. 

Internally \code{make} will call the \code{buildMakefile} method to first construct an up to date makefile representing
the project. Each makefile has a \emph{md5 digest} in it which will be updated should you alter the project at all and 
this stamp is then used to determine if the makefile needs re-construction. If the makefile is re-constructure during 
a call to \code{make} then a make clean operation will be carried out to ensure the entire project is re-built. This 
should ensure that the state of the project, makefile and the build remains in sync.

Finally, if your project needs to install the library and/or include files to a specific location that is also 
catered for in \code{BuildSys}. Simply specify the install locations with the \code{InstallLibraryName} and \code{InstallIncludeName} 
arguments in the \code{BSysProject} initializer. For instance,

\code{Project <- new("BSysProject", Flat=F, InstallLibraryName="inst/lib", InstallIncludeName="inst/include")}

}

\subsection{Loading and Unloading the Library}{
Once the project is made using the \code{make} method the dynamic library can be loaded into \emph{R} memory. We do so using the
\code{loadLibrary} method and we can similarly unload the library using the \code{unloadLibrary} method. The library
name is based apon the project name and the path to the library can be obtained with the \code{libraryPath} method. For example,

\preformatted{
loadLibrary(Project) # load the library built by Project

unloadLibrary(Project) # unload the library built by Project

libraryPath(Project) # return the path to the library built by Project
}
}

\subsection{Debugging the Library}{
To debug a dynamic library a debug build must be made to include the symbolic information needed with the dynamic library. 
By default projects constructed using \code{BuildSys} will be created as debug builds. This is controlled by the \code{Debug} argument
supplied in the call to the \code{BSysProject} initilizer. For instance,

\code{Project <- new("BSysProject", Debug=TRUE)}

creates a debug enabled project whereas,

\code{Project <- new("BSysProject", Debug=FALSE)}

create a release / optimised project. For an existing project we can make a debug build by supplying the \code{Debug} argument
to the call to the \code{make} method as with,

\code{Project <- make(Project, Debug=TRUE)}

Recall that for \emph{S4 class methods} to change the object state we need to assign the returned modified object to the original one,
which is why the above example assigns the \code{make} call to \code{Project}.

Debugging is handled through \emph{Microsoft Visual Code} and \emph{gdb / lldb} and it is assumed that these components are installed and
functional. For information regarding the installation of these components consult the next section. 

To debug an existing dynamic library (built from a \code{BSysProject}) simply call the \code{vcDebug} method as follows.

\code{vcDebug(Project) # Debug the library created by project}

Calling this method will open an instance of \emph{Microsoft Visual Code} that is correctly initialised to debug your library code. Within
\emph{Visual Code} you can open the source file/s of your library, set breakpoints and run a debug session. To run a debug 
session select the \emph{run/Start Debugging} menu item. Doing so will result in a new instance of \emph{R} being launched which contains
the same environment (including loaded packages and loaded dynamic libraries) as the parent \emph{R} session where \code{vcDebug} was first
called. This \emph{R} session is your \emph{sandbox} to safely debug your library in and leaves the parent \emph{R} session safe from loss should 
your code crash \emph{R} completely. 

Typically when debugging a new library the library requires \emph{R} setup code to initialise as a basis for running some for of library testing.
With \code{BuildSys} you should perform this initial setup of \emph{R} in the parent \emph{R} session before calling \code{vcDebug} so that you
never have to initialise \emph{R} in your debug session. Its correct state will have been initialised from the parent \emph{R} session. 
}

\subsection{Software Installation Requirements for Debugging}{
In order to debug code with \code{BuildSys} \emph{Microsoft Visual Code} must be installed. You can download and install this software from
here:

\href{https://code.visualstudio.com/download}{Download Visual Studio Code}

Choose the appropriate installer for your operating system. The installer should include \emph{Code} in the \code{PATH} environment variable.
Verify this by opening a new shell/command prompt and typing,

\code{Code}

and enter. If \emph{Visual Code} starts then the \code{PATH} enviroment variable is correctly set. If it fails to start then add it to the
system \code{PATH} environment variable.

On \emph{Windows} you will have to install \emph{Rtools} which you can find here:

\href{https://cran.r-project.org/bin/windows/Rtools/}{Using Rtools40 on Windows}

You will need to add \emph{R} and \emph{Rtools} to the system \emph{Path}. For instance add, 

\preformatted{
C:\rtools40\mingw64\bin\
C:\rtools40\usr\bin\
C:\Program Files\R\R-4.0.1\bin\x64\
}

but be aware that the locations may differ in your case depending on which version of \emph{R} and \emph{Rtools} and where these products were installed. 
To set environment variables in \emph{Windows 10},

\enumerate{
\item Open the Start Search, type in \dQuote{env}, and choose \dQuote{Edit the system environment variables}:
\item Click the \dQuote{Environment Variables...} button.
\item Set the environment variables as needed. The New button adds an additional variable.
\item Dismiss all of the dialogs by choosing \dQuote{OK}. Your changes are saved!
}

In Windows after installing \emph{Rtools} we also need to install \emph{gdb}. To install \emph{gdb}, open an \emph{MSYS2} shell (In explorer migrate to the Rtools40 folder 
and double click on \emph{msys2.exe}) and enter the following command,

\code{pacman -S mingw-w64-x86_64-gdb}

After installing verify that \emph{gdb} is installed by opening a new \emph{DOS box} and typing,

\code{gdb -v}

If all is correct it should respond with something like,

\preformatted{
GNU gdb (GDB) 8.3.1
Copyright (C) 2019 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
}

On \emph{MacOS / OS X} you will need to install \emph{Xcode} to have access to both clang and lldb. Go to \emph{Mac App Store} and download and install \emph{Xcode}.

On \emph{Linux} you will need to have \emph{gcc} installed. Installation of \emph{gcc} on \emph{Linux} is omitted because of the 
many and varied ways to do so depending on the distribution being used. Please consult \emph{Google} on this topic for more information
on installing development tools on \emph{Linux}. 

Finally, you will also need to install the \emph{Microsoft C/C++ IntelliSense, debugging, and code browsing} extension in 
\emph{Microsoft Visual Code}. To install simply start \emph{Visual Code}, open a C or C++ file and \emph{Visual Code} will
prompt you about installing the extension. Simply install it when prompted to do so. 
}
}
\author{
\packageAuthor{BuildSys}

Maintainer: \packageMaintainer{BuildSys}
}
\keyword{BuildSys}
\seealso{
\href{https://code.visualstudio.com/docs/cpp/cpp-debug}{Debug C++ in Visual Studio Code}  
}
\examples{
}
