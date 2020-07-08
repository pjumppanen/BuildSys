# -----------------------------------------------------------------------------
# BuildSys.R
# -----------------------------------------------------------------------------
# Implements an R based build system for making and debugging C/C++ dlls
#
# By Paavo Jumppanen
# Copyright (c) 2020, CSIRO Marine and Atmospheric Research
# All Rights Reserved.
# -----------------------------------------------------------------------------


dynlib <- function(BaseName)
{
  SysName <- Sys.info()["sysname"]

  if (SysName == "Windows")
  {
    LibName <- paste(BaseName, ".dll", sep="")
  }
  else if (SysName == "Linux")
  {
    LibName <- paste(BaseName, ".so", sep="")
  }
  else if (SysName == "Darwin")
  {
    LibName <- paste(BaseName, ".dylib", sep="")
  }
  else
  {
    stop("Unsupported OS")
  }
}


# -----------------------------------------------------------------------------
# class representing a source file and its dependencies
# -----------------------------------------------------------------------------
setClass("BSysSourceFile",
  slots = c(
    Filename      = "character",
    Type          = "character",
    Dependencies  = "list",
    Externals     = "list"
  )
)


# -----------------------------------------------------------------------------
# Constructor for SourceFile class
# -----------------------------------------------------------------------------
setMethod("initialize", "BSysSourceFile",
  function(.Object, Filename, IncludeFolder, Type)
  {
    .Object@Filename     <- Filename
    .Object@Type         <- Type
    .Object@Dependencies <- list()
    .Object@Externals    <- list()

    buildDependencies <- function(.Object, Filename)
    {
      if ((Type == "c") || (Type == "cpp"))
      {
        MatchExp      <- "#include[\t ]*"
        StripExp      <- "<|>|\""
        CommentExp    <- "//.*"
        CaseSensitive <- TRUE
      }
      else if (Type == "f")
      {
        MatchExp      <- "INCLUDE[\t ]*"
        StripExp      <- "'"
        CommentExp    <- "!.*"
        CaseSensitive <- FALSE
      }

      Lines <- readLines(Filename)

      for (Line in Lines)
      {
        # Find include statements to figure out dependencies.
        # This has no proper pre-processing component so if you are
        # using pre-processor conditionals then this will pull out
        # more dependencies than your code may have. 
        if (grepl(MatchExp, Line))
        {
          Include         <- sub(CommentExp, "", gsub(StripExp, "", sub(MatchExp, "", Line)))
          PrefixedInclude <- paste(IncludeFolder, Include, sep="")

          if (file.exists(PrefixedInclude))
          {
            .Object@Dependencies <- c(.Object@Dependencies, Include)

            # look for nested includes
            .Object <- buildDependencies(.Object, PrefixedInclude)
          }
          else
          {
            .Object@Externals <- c(.Object@Externals, Include)
          }
        }
      }

      return (.Object)
    }

    return (buildDependencies(.Object, Filename))
  }
)


# -----------------------------------------------------------------------------
# method to return build rule for source file
# -----------------------------------------------------------------------------
setGeneric("makeBuildRule", function(.Object, ...) standardGeneric("makeBuildRule"))

setMethod("makeBuildRule", "BSysSourceFile",
  function(.Object, RelativePath="")
  {
    if (.Object@Type == "c")
    {
      BuildRule <- paste("\t$(CC) $(CFLAGS) -c ", RelativePath, .Object@Filename, sep="")
    }
    else if (.Object@Type == "cpp")
    {
      BuildRule <- paste("\t$(CXX) $(CXXFLAGS) -c ", RelativePath, .Object@Filename, sep="")
    }
    else if (.Object@Type == "f")
    {
      BuildRule <- paste("\t$(FC) $(FFLAGS) -c ", RelativePath, .Object@Filename, sep="")
    }

    return (BuildRule)
  }
)


# -----------------------------------------------------------------------------
# class reprensenting a project and the files that define it
# -----------------------------------------------------------------------------
setClass("BSysCodeProject",
  slots = c(
    ProjectName   = "character",
    MakeID        = "integer",
    WorkingFolder = "character",
    SourceName    = "character",
    IncludeName   = "character",
    ObjName       = "character",
    InstallName   = "character",
    Flat          = "logical",
    SourceFiles   = "list",
    Packages      = "character",
    Includes      = "character",
    Defines       = "character",
    Libraries     = "character",
    CFLAGS        = "character",
    CXXFLAGS      = "character",
    FFLAGS        = "character",
    LDFLAGS       = "character", 
    LDLIBS        = "character", 
    DEFINES       = "character", 
    IsDebug       = "logical",
    DebugState    = "list"
  )
)


# -----------------------------------------------------------------------------
# Constructor for CodeProject class
# -----------------------------------------------------------------------------
setMethod("initialize", "BSysCodeProject",
  function(.Object, 
           Name="",
           WorkingFolder="", 
           SourceName="src",
           IncludeName="include",
           ObjName="obj",
           InstallName="obj",
           Flat=TRUE,
           Packages=as.character(c()), 
           Includes=as.character(c()), 
           Defines=as.character(c()),
           Libraries=as.character(c()),
           CFLAGS=as.character(c()),
           CXXFLAGS=as.character(c()),
           FFLAGS=as.character(c()),
           LDFLAGS=as.character(c()), 
           LDLIBS=as.character(c()), 
           DEFINES=as.character(c()), 
           Debug=TRUE)
  {
    .Object@MakeID        <- as.integer(trunc(runif(1,0,2^31)))
    .Object@ProjectName   <- ""
    .Object@WorkingFolder <- ""
    .Object@SourceName    <- ""
    .Object@IncludeName   <- ""
    .Object@ObjName       <- ""
    .Object@InstallName   <- ""
    .Object@Flat          <- TRUE
    .Object@SourceFiles   <- list()
    .Object@Packages      <- Packages
    .Object@Includes      <- c(R.home("include"), Includes)
    .Object@Defines       <- Defines
    .Object@Libraries     <- Libraries
    .Object@CFLAGS        <- CFLAGS
    .Object@CXXFLAGS      <- CXXFLAGS
    .Object@FFLAGS        <- FFLAGS
    .Object@LDFLAGS       <- LDFLAGS
    .Object@LDLIBS        <- LDLIBS
    .Object@DEFINES       <- DEFINES 
    .Object@IsDebug       <- Debug
    .Object@DebugState    <- list()

    return (initProjectFromFolder(.Object,
                                  Name, 
                                  WorkingFolder, 
                                  SourceName,
                                  IncludeName,
                                  ObjName,
                                  InstallName,
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
                                  Debug))
  }
)


# -----------------------------------------------------------------------------
# Method to initialise Project based on folder contents 
# -----------------------------------------------------------------------------
setGeneric("initProjectFromFolder", function(.Object, ...) standardGeneric("initProjectFromFolder"))

setMethod("initProjectFromFolder", "BSysCodeProject",
  function(.Object, 
           Name="",
           WorkingFolder="", 
           SourceName="src",
           IncludeName="include",
           ObjName="obj",
           InstallName="obj",
           Flat=TRUE,
           Packages=as.character(c()), 
           Includes=as.character(c()), 
           Defines=as.character(c()),
           Libraries=as.character(c()),
           CFLAGS=as.character(c()),
           CXXFLAGS=as.character(c()),
           FFLAGS=as.character(c()),
           LDFLAGS=as.character(c()), 
           LDLIBS=as.character(c()), 
           DEFINES=as.character(c()), 
           Debug=TRUE)
  {
    if (Sys.info()["sysname"] == "Windows")
    {
      RLIBPATH <- R.home("bin")

      KnownLibDependencies <- list(BLAS.h="Rblas", Lapack.h="Rlapack", iconv.h="Riconv")
    }
    else
    {
      RLIBPATH <- paste(R.home(), "/lib", Sys.getenv("R_ARCH"), sep="")

      KnownLibDependencies <- list(BLAS.h="blas", Lapack.h="lapack", iconv.h="iconv")
    }

    addExternalDependencies <- function(SourceFile, CodeProject)
    {
      if (CodeProject@IsDebug)
      {
        DefTMB_SafeBounds <- "TMB_SAFEBOUNDS" 
      }
      else
      {
        DefTMB_SafeBounds <- "" 
      }

      DefLIB_UNLOAD   <- paste("LIB_UNLOAD=R_unload_", CodeProject@ProjectName, sep="")
      DefTMB_LIB_INIT <- paste("TMB_LIB_INIT=R_init_", CodeProject@ProjectName, sep="")

      RcppDep      <- list(pkg="Rcpp",      
                           defs=c(DefLIB_UNLOAD), 
                           libs=as.character(c()))
      
      RcppEigenDep <- list(pkg="RcppEigen", 
                           defs=c(DefLIB_UNLOAD), 
                           libs=as.character(c()))

      TMBDep       <- list(pkg="TMB",       
                           defs=c(DefTMB_SafeBounds, DefLIB_UNLOAD, DefTMB_LIB_INIT), 
                           libs=as.character(c()))

      KnownPackageDependencies <- list(Rcpp.hpp      =list(RcppDep), 
                                       RcppEigen.hpp =list(RcppDep, RcppEigenDep), 
                                       TMB.hpp       =list(TMBDep, RcppDep, RcppEigenDep))

      for (External in SourceFile@Externals)
      {
        PackageDependencies <- KnownPackageDependencies[[External]]

        if (!is.null(PackageDependencies))
        {
          for (PackageDependency in PackageDependencies)
          {
            if (!PackageDependency$pkg %in% CodeProject@Packages)
            {
              CodeProject@Packages <- c(CodeProject@Packages, PackageDependency$pkg)
              IncludePath          <- getPackagePath(PackageDependency$pkg, "/Include")

              if (!IncludePath %in% CodeProject@Includes)
              {
                CodeProject@Includes <- c(CodeProject@Includes, IncludePath)
              }

              for (Define in PackageDependency$defs)
              {
                if (!Define %in% CodeProject@Defines)
                {
                  CodeProject@Defines <- c(CodeProject@Defines, Define)
                }
              }

              for (Lib in PackageDependency$libs)
              {
                if (!Define %in% CodeProject@Defines)
                {
                  CodeProject@Defines <- c(CodeProject@Defines, Define)
                }
              }              
            }
          }
        }

        LibDependency <- KnownLibDependencies[[External]]

        if (!is.null(LibDependency))
        {
          CodeProject@Libraries <- c(CodeProject@Libraries, LibDependency)
        }
      }

      return (CodeProject)
    }

    addSlash <- function(Path)
    {
      if (grepl("\\\\[^ ]+|\\\\$", Path))
      {
        stop(paste("'", Path, "' uses \\ as delimiter. Please use / instead.", sep=""))
      }

      if ((nchar(Path) != 0) && !grepl("/$", Path))
      {
        Path <- paste(Path, "/", sep="")
      }

      return (Path)
    }

    FullPath <- addSlash(normalizePath(WorkingFolder, winslash="/", mustWork=FALSE))

    if (nchar(FullPath) == 0)
    {
      FullPath <- addSlash(getwd())
    }

    .Object@MakeID        <- as.integer(trunc(runif(1,0,2^31)))
    .Object@WorkingFolder <- FullPath
    .Object@ProjectName   <- Name
    .Object@SourceName    <- ""
    .Object@IncludeName   <- ""
    .Object@ObjName       <- ""
    .Object@InstallName   <- ""
    .Object@Flat          <- Flat
    .Object@SourceFiles   <- list()
    .Object@Packages      <- Packages
    .Object@Includes      <- c(R.home("include"), Includes)
    .Object@Defines       <- Defines
    .Object@Libraries     <- c(paste(RLIBPATH, "/R", sep=""), Libraries)
    .Object@CFLAGS        <- CFLAGS
    .Object@CXXFLAGS      <- CXXFLAGS
    .Object@FFLAGS        <- FFLAGS
    .Object@LDFLAGS       <- LDFLAGS
    .Object@LDLIBS        <- LDLIBS
    .Object@DEFINES       <- DEFINES
    .Object@IsDebug       <- Debug
    .Object@DebugState    <- list()

    if (!Flat)
    {
      .Object@SourceName  <- addSlash(SourceName)
      .Object@IncludeName <- addSlash(IncludeName)
      .Object@ObjName     <- addSlash(ObjName)
      .Object@InstallName <- addSlash(InstallName)
    } 

    SrcFolder     <- paste(.Object@WorkingFolder, .Object@SourceName, sep="")
    IncludeFolder <- paste(.Object@WorkingFolder, .Object@IncludeName, sep="")
    AllFiles      <- dir(SrcFolder)

    for (File in AllFiles)
    {
      if (grepl("\\.c$", File))
      {
        # c source file
        SourceFile <- new("BSysSourceFile", File, IncludeFolder, "c")

        .Object@SourceFiles[[length(.Object@SourceFiles) + 1]] <- SourceFile 
      }
      else if (grepl("\\.cpp$", File))
      {
        # c++ source file
        SourceFile <- new("BSysSourceFile", File, IncludeFolder, "cpp")

        .Object@SourceFiles[[length(.Object@SourceFiles) + 1]] <- SourceFile 
      }
      else if ((grepl("\\.f$|\\.for$|\\.f95$|\\.f90$|\\.f77$", File)))
      {
        # fortran source file
        SourceFile <- new("BSysSourceFile", File, IncludeFolder, "f")

        .Object@SourceFiles[[length(.Object@SourceFiles) + 1]] <- SourceFile 
      }
      else
      {
        # Other file
      }
    }

    if (nchar(.Object@ProjectName) == 0)
    {
      # If project has only 1 source file use it to initialise project name
      if (length(.Object@SourceFiles) == 1)
      {
        .Object@ProjectName <- gsub("\\..*$", "", .Object@SourceFiles[[1]]@Filename)
      }
      else
      {
        stop("You must supply a 'Name' for this project object")
      }
    }

    # This step needs to happen after setting ProjectName
    for (SourceFile in .Object@SourceFiles)
    {
      .Object <- addExternalDependencies(SourceFile, .Object)
    }

    return (.Object)
  }
)


# -----------------------------------------------------------------------------
# Method to build makefile 
# -----------------------------------------------------------------------------
setGeneric("buildMakefile", function(.Object, ...) standardGeneric("buildMakefile"))

setMethod("buildMakefile", "BSysCodeProject",
  function(.Object, Force=FALSE)
  {
    # -------------------------------------------------------------------------
    # idStamp() creates a stamp to check staleness of makefile
    # -------------------------------------------------------------------------
    idStamp <- function()
    {
      IdStamp <- paste("# MakeID:", .Object@MakeID, "--Do not edit this line")

      return (IdStamp)
    }

    # -------------------------------------------------------------------------
    # checkMakefile() checks if makefile up to date
    # -------------------------------------------------------------------------
    checkMakefile <- function(MakefilePath)
    {
      UptoDate <- FALSE

      # Check if makefile exists.
      if (file.exists(MakefilePath))
      {
        # Check if makefile up to date.
        Lines <- readLines(MakefilePath, n=1)

        if (Lines[1] == idStamp())
        {
          UptoDate <- TRUE
        }
      }

      return (UptoDate)
    }

    # -------------------------------------------------------------------------
    # createMakefile() creates a new makefile based on project
    # -------------------------------------------------------------------------
    createMakefile <- function(MakefilePath)
    {
      dropLeadingTrailingSlashes <- function(Path)
      {
        Path <- gsub("/+$", "", Path)
        Path <- gsub("^/+", "", Path)

        return (Path)
      }

      DlibName <- dynlib(.Object@ProjectName)
      LDLIBS   <- .Object@LDLIBS

      RootRelativePath    <- ""
      SrcRelativePath     <- ""
      IncludeRelativePath <- ""
      InstallRelativePath <- ""

      ObjName     <- dropLeadingTrailingSlashes(.Object@ObjName)
      SourceName  <- dropLeadingTrailingSlashes(.Object@SourceName)
      IncludeName <- dropLeadingTrailingSlashes(.Object@IncludeName)
      InstallName <- dropLeadingTrailingSlashes(.Object@InstallName)

      if (nchar(ObjName) != 0)
      {
        depth <- length(which(as.integer(gregexpr("/", ObjName)[[1]]) != -1)) + 1

        for (cx in 1:depth)
        {
          RootRelativePath <- paste(RootRelativePath, "../", sep="")
        }
      }

      for (Library in .Object@Libraries)
      {
        if (grepl("\\\\[^ ]+|\\\\$", Library))
        {
          stop(paste("'", Library, "' uses \\ as delimiter. Please use / instead.", sep=""))
        }

        if (grepl("/", Library))
        {
          LibPath <- gsub("{1}/[^/]*$", "", Library)
          Lib     <- gsub(paste(LibPath, "/", sep=""), "", Library)

          LDLIBS <- c(LDLIBS, paste("-L", LibPath, " -l", Lib, sep=""))
        }
        else
        {
          LDLIBS <- c(LDLIBS, paste("-l", Library, sep=""))
        }
      }

      if (.Object@IsDebug)
      {
        COMMONFLAGS <- c("-O0", "-g", "-DDEBUG", "-D_DEBUG")
      }
      else
      {
        COMMONFLAGS <- c("-O2", "-DNDEBUG")
      }

      for (Define in .Object@Defines)
      {
        if (nchar(Define) > 0)
        {
          COMMONFLAGS <- c(COMMONFLAGS, paste("-D", Define, sep=""))
        }
      }

      for (Define in .Object@DEFINES)
      {
        if ((nchar(Define) > 0) && !(Define %in% .Object@Define))
        {
          COMMONFLAGS <- c(COMMONFLAGS, paste("-D", Define, sep=""))
        }
      }

      if (nchar(SourceName) != 0)
      {
        SrcRelativePath <- paste(RootRelativePath, SourceName, "/", sep="")
      }

      if (nchar(IncludeName) != 0)
      {
        IncludeRelativePath <- paste(RootRelativePath, IncludeName, "/", sep="")
        COMMONFLAGS         <- c(COMMONFLAGS, paste("-I", IncludeRelativePath, sep=""))
      }

      if (nchar(InstallName) != 0)
      {
        InstallRelativePath <- paste(RootRelativePath, InstallName, "/", sep="")
      }
      
      for (Include in .Object@Includes)
      {
        COMMONFLAGS <- c(COMMONFLAGS, paste("-I", Include, sep=""))
      }

      LDFLAGS  <- c("-shared", .Object@LDFLAGS) 
      CFLAGS   <- c("$(COMMONFLAGS)", .Object@CFLAGS)
      CXXFLAGS <- c("$(COMMONFLAGS)", "-Wno-ignored-attributes", .Object@CXXFLAGS)
      FFLAGS   <- c("$(COMMONFLAGS)", .Object@FFLAGS)

      # Build makefile
      MakefileTxt <-c(
        idStamp(),
        "CC=gcc",
        "CXX=g++",
        "FC=gfortran",
        paste("COMMONFLAGS=", paste(COMMONFLAGS, collapse="\\\n"), sep=""),
        paste("CFLAGS=", paste(CFLAGS, collapse="\\\n"), sep=""),
        paste("CXXFLAGS=", paste(CXXFLAGS, collapse="\\\n"), sep=""),
        paste("FFLAGS=", paste(FFLAGS, collapse="\\\n"), sep=""),
        paste("LDFLAGS=", paste(LDFLAGS, collapse="\\\n"), sep=""),
        paste("LDLIBS=", paste(LDLIBS, collapse="\\\n"), sep=""),
        paste("objects=", 
              paste(sapply(.Object@SourceFiles, function(item){ gsub("\\..*$", ".o", item@Filename)}), collapse=" \\\n"), sep=""),
        "",
        paste(DlibName, " : $(objects)", sep=""),
        paste("\t$(CXX) -o ", DlibName, " $(LDFLAGS) $(objects) $(LDLIBS)", sep=""),
        ""
      )

      for (SourceFile in .Object@SourceFiles)
      {
        BaseName <- gsub("\\..*$", "", SourceFile@Filename)
        MakeRule <- paste(BaseName, ".o : ",
                          SrcRelativePath, 
                          SourceFile@Filename,
                          " ",
                          paste(sapply(SourceFile@Dependencies, function(dep) {paste(IncludeRelativePath, dep, sep="")}), collapse=" "),
                          sep="")

        if (nchar(SrcRelativePath) > 0)
        {
          BuildRule   <- makeBuildRule(SourceFile, SrcRelativePath)
          MakefileTxt <- c(MakefileTxt,
                          MakeRule,
                          BuildRule,
                          "")
        }
        else
        {
          MakefileTxt <- c(MakefileTxt,
                          MakeRule,
                          "")
        }
      } 

      MakefileTxt <- c(MakefileTxt,
                       paste("clean : \n\trm ", DlibName, " $(objects)", sep=""),
                       "")

      makefile <- file(MakefilePath, "wt")
      writeLines(MakefileTxt, makefile)
      close(makefile)
    }

    if (nchar(.Object@ObjName) != 0)
    {
      MakefilePath <- paste(.Object@WorkingFolder, .Object@ObjName, "/makefile", sep="")
    }
    else
    {
      MakefilePath <- paste(.Object@WorkingFolder, "makefile", sep="")
    }

    if (!checkMakefile(MakefilePath) || Force)
    {
      createMakefile(MakefilePath)
    }   
  }
)


# -----------------------------------------------------------------------------
# Method to build dynamic library project 
# -----------------------------------------------------------------------------
setGeneric("make", function(.Object, ...) standardGeneric("make"))

setMethod("make", "BSysCodeProject",
  function(.Object, Operation="", Debug=NULL)
  {
    DlibName <- dynlib(.Object@ProjectName)

    # if Debug provided and different from current update and increment MakeID 
    if (!is.null(Debug) && (Debug != .Object@IsDebug))
    {
      .Object@IsDebug <- Debug
      .Object@MakeID  <- as.integer(.Object@MakeID + 1)
    }

    # build makefile
    buildMakefile(.Object)

    ObjFolder    <- paste(.Object@WorkingFolder, .Object@ObjName, sep="")
    CapturePath  <- paste(.Object@WorkingFolder, .Object@ProjectName, ".log", sep="")
    ScriptPath   <- paste(.Object@WorkingFolder, .Object@ProjectName, ".sh", sep="")
    FinishedFile <- paste(.Object@WorkingFolder, .Object@ProjectName, ".fin", sep="")

    # run make
    if (Operation == "clean")
    {
      operation <- paste("make -C", ObjFolder, "clean | tee", CapturePath)
    } 
    else if (Operation == "")
    {
      operation <- paste("make -C", ObjFolder, "| tee", CapturePath)
    }
    else
    {
      stop("Undefined make() Operation")
    }

    # construct caller script
    BashScript <- c("#!/bin/bash",
                    operation,
                    paste("echo finished >", FinishedFile))

    ScriptFile <- file(ScriptPath, "wt")
    writeLines(BashScript, ScriptFile)
    close(ScriptFile)

    command.line <- paste(Sys.which("bash"), ScriptPath)
    unlink(FinishedFile)

    unloadLibrary(.Object)
    system(command.line, wait=FALSE, invisible=FALSE, intern=FALSE)

    # test for completion of script. We do this rather than using
    # wait=TRUE in system call so that we can make a visible shell that 
    # shows live progress. With wait=TRUE a visible shell shows no
    # output. 
    while (!file.exists(FinishedFile))
    {
      Sys.sleep(1)
    }

    unlink(FinishedFile)

    CaptureFile <- file(CapturePath, "rt")
    writeLines(readLines(CaptureFile))
    close(CaptureFile)
    unlink(CapturePath)

    return (.Object)   
  }
)


# -----------------------------------------------------------------------------
# Method to get library path
# -----------------------------------------------------------------------------
setGeneric("libraryPath", function(.Object, ...) standardGeneric("libraryPath"))

setMethod("libraryPath", "BSysCodeProject",
  function(.Object)
  {
    DlibName <- dynlib(.Object@ProjectName)

    if (nchar(.Object@ObjName) != 0)
    {
      DlibPath <- paste(.Object@WorkingFolder, .Object@ObjName, DlibName, sep="")
    }
    else
    {
      DlibPath <- paste(.Object@WorkingFolder, DlibName, sep="")
    }

    return (DlibPath)
  }
)


# -----------------------------------------------------------------------------
# Method to get source path
# -----------------------------------------------------------------------------
setGeneric("sourcePath", function(.Object, ...) standardGeneric("sourcePath"))

setMethod("sourcePath", "BSysCodeProject",
  function(.Object)
  {
    SourcePath <- .Object@WorkingFolder

    if (nchar(.Object@SourceName) != 0)
    {
      SourcePath <- paste(SourcePath, .Object@SourceName, sep="")
    }

    return (SourcePath)
  }
)


# -----------------------------------------------------------------------------
# Method to get include path
# -----------------------------------------------------------------------------
setGeneric("includePath", function(.Object, ...) standardGeneric("includePath"))

setMethod("includePath", "BSysCodeProject",
  function(.Object)
  {
    IncludePath <- .Object@WorkingFolder

    if (nchar(.Object@IncludeName) != 0)
    {
      IncludePath <- paste(IncludePath, .Object@IncludeName, sep="")
    }

    return (IncludePath)
  }
)


# -----------------------------------------------------------------------------
# Method to get obj path
# -----------------------------------------------------------------------------
setGeneric("objPath", function(.Object, ...) standardGeneric("objPath"))

setMethod("objPath", "BSysCodeProject",
  function(.Object)
  {
    ObjPath <- .Object@WorkingFolder

    if (nchar(.Object@ObjName) != 0)
    {
      ObjPath <- paste(ObjPath, .Object@ObjName, sep="")
    }

    return (ObjPath)
  }
)


# -----------------------------------------------------------------------------
# Method to get install path
# -----------------------------------------------------------------------------
setGeneric("installPath", function(.Object, ...) standardGeneric("installPath"))

setMethod("installPath", "BSysCodeProject",
  function(.Object)
  {
    InstallPath <- .Object@WorkingFolder

    if (nchar(.Object@InstallName) != 0)
    {
      InstallPath <- paste(InstallPath, .Object@InstallName, sep="")
    }

    return (InstallPath)
  }
)


# -----------------------------------------------------------------------------
# Method to load library 
# -----------------------------------------------------------------------------
setGeneric("loadLibrary", function(.Object, ...) standardGeneric("loadLibrary"))

setMethod("loadLibrary", "BSysCodeProject",
  function(.Object)
  {
    return (dyn.load(libraryPath(.Object)))
  }
)


# -----------------------------------------------------------------------------
# Method to unload library 
# -----------------------------------------------------------------------------
setGeneric("unloadLibrary", function(.Object, ...) standardGeneric("unloadLibrary"))

setMethod("unloadLibrary", "BSysCodeProject",
  function(.Object)
  {
    libPath <- libraryPath(.Object)
    tr      <- try(dyn.unload(libraryPath(.Object)), silent=TRUE)

    if (!is(tr, "try-error")) 
    {
      cat("Note: Library", libPath, "was unloaded.\n")
    }
  }
)


# -----------------------------------------------------------------------------
# Helper to find and check for package includes
# -----------------------------------------------------------------------------
getPackagePath <- function(PackageName, SubPath)
{
  PackagePath <- ""

  for (path in .libPaths())
  {
    path <- paste(path, "/", PackageName, SubPath, sep="")

    if (file.exists(path))
    {
      PackagePath <- path
      break
    }
  }

  if (nchar(PackagePath) == 0)
  {
    stop(paste("Cannot find include path ", paste(PackageName, SubPath, sep=""), ". Check that the", PackageName, "package is installed."))
  }

  return (PackagePath)
}


# -----------------------------------------------------------------------------
# Helper to list loaded libraries not bound to packages
# -----------------------------------------------------------------------------
getLoadedSharedLibraries <- function()
{
  packages.paths <- .libPaths()
  packages.paths <- c(packages.paths, R.home())
  loadList <- getLoadedDLLs()
  sharedLibrariesList <- c()
  exclusions <- c("base", "(embedding)")

  for (item in loadList)
  {
    item <- unlist(item)

    if (!(item$name %in% exclusions))
    {
      is.package <- FALSE 
      
      for (packages.path in packages.paths)
      {
        is.package <- is.package || grepl(packages.path, item$path)
      }

      if (!is.package)
      {
        path <- sub("\\.dylib", "", sub("\\.so", "", sub("\\.dll", "", item$path)))
        sharedLibrariesList <- c(sharedLibrariesList, path)
      }
    }
  }

  return (sharedLibrariesList)
}


# -----------------------------------------------------------------------------
# Method to debug library 
# -----------------------------------------------------------------------------
setGeneric("vcDebug", function(.Object, ...) standardGeneric("vcDebug"))

setMethod("vcDebug", "BSysCodeProject",
  function(.Object, LaunchEditor=TRUE)
  {
    debugProjectPath <- paste(sourcePath(.Object), .Object@ProjectName, "_DebugProject.RData", sep="")
    debugSessionPath <- paste(sourcePath(.Object), .Object@ProjectName, "_DebugSession.RData", sep="")
    Rprofile.path    <- paste(sourcePath(.Object), .Object@ProjectName, "_DebugRprofile.txt", sep="")

    if (LaunchEditor)
    {
      # create .vscode folder if needed
      vsCodeFolder <- paste(sourcePath(.Object), ".vscode", sep="")
      file.attr    <- file.info(vsCodeFolder)
      IsDarwin     <- (Sys.info()["sysname"] == "Darwin")

      if (is.na(file.attr$size))
      {
        dir.create(vsCodeFolder)
      }
      else if (!file.attr$isdir)
      {
        stop("Cannot create .vscode folder as .vscode file exists.")
      }

      debug.app <- "gdb"

      # get needed paths
      if (IsDarwin)
      {
        R.path <- "/Applications/R.app/Contents/MacOS/R"

        if (!file.exists(R.path))
        {
          stop(paste("Cannot find R.app. We have assumed it is installed at", R.path))
        }
        
        gdb.path  <- "/Applications/Xcode.app/Contents/Developer/usr/bin/lldb-mi"
        debug.app <- "lldb"

        if (!file.exists(gdb.path))
        {
          stop("Cannot find lldb-mi. Ensure Xcode is installed.")
        }
      }
      else
      {
        R.path <- normalizePath(Sys.which("Rgui"), "/", mustWork=FALSE)

        if (nchar(R.path) == 0)
        {
          R.path <- paste(R.home(), "/bin/exec/R", sep="")
        }

        gdb.path <- normalizePath(Sys.which(debug.app), "/", mustWork=FALSE)

        if (nchar(gdb.path) == 0)
        {
          stop(paste("Cannot find path to gdb. Check that", debug.app, "is accessible via the PATH environment variable."))
        }
      }

      gcc.path <- normalizePath(Sys.which("gcc"), "/", mustWork=FALSE)

      if (nchar(gcc.path) == 0)
      {
        stop("Cannot find path to gcc. Check that gcc is accessible via the PATH environment variable.")
      }

      # build intellisense include paths
      R.include             <- R.home("include")
      intellisense.includes <- ""
      intellisense.defines  <- paste(sapply(.Object@Defines, function(str) {paste("\"", str, "\"", sep="")}), collapse=",", sep="")

      if (grepl("mingw", gcc.path))
      {
        # windows
        rtools.path           <- sub("/mingw.*", "/", gcc.path)
        gcc.include           <- sub("/bin/gcc.*", "/include", gcc.path)
        intellisense.includes <- paste(intellisense.includes, ",\"", gcc.include, "/**\"", sep="")

        root.include <- paste(rtools.path, "usr/include", sep="")
        root.user.include <- paste(rtools.path, "usr/local/include", sep="")

        if (file.exists(root.include))
        {
          intellisense.includes <- paste(intellisense.includes, ",\"", root.include, "/**\"", sep="")
        }

        if (file.exists(root.user.include))
        {
          intellisense.includes <- paste(intellisense.includes, ",\"", root.user.include, "/**\"", sep="")
        }
      }
      else
      {
        # linux
        gcc.include       <- "/usr/include"
        gcc.local.include <- "/usr/local/include"

        if (file.exists(gcc.include))
        {
          intellisense.includes <- paste(intellisense.includes, ",\"", gcc.include, "/**\"", sep="")
        }

        if (file.exists(gcc.local.include))
        {
          intellisense.includes <- paste(intellisense.includes, ",\"", gcc.local.include, "/**\"", sep="")
        }
      }

      intellisense.includes <- paste(intellisense.includes, ",\"", R.include, "/**\"", sep="")
      intellisense.includes <- paste(intellisense.includes, ",\"", includePath(.Object), "**\"", sep="")

      # add other include paths
      for (include in .Object@Includes)
      {
        intellisense.includes <-  paste(intellisense.includes, ",\"", include, "/**\"", sep="")
      }

      # create debugRprofile.txt environment setup file for gdb debug session
      working.dir <- getwd()

      Rprofile_lines <- c(
      paste("setwd(\"", working.dir, "\")", sep=""),
      paste("load(\"", debugSessionPath, "\")", sep=""),
      paste("load(\"", debugProjectPath, "\")", sep=""),
      "vcDebug(BSysDebugProject, FALSE)"
      )

      Rprofile_file <- file(Rprofile.path, "wb")
      writeLines(Rprofile_lines, Rprofile_file)
      close(Rprofile_file)

      # create launch.json
      launch_lines <- c(
      "{",
      "  \"version\": \"0.2.0\",",
      "  \"configurations\": [",
      "    {",
      paste("      \"name\": \"(", debug.app, ") Launch\",", sep=""),
      "      \"type\": \"cppdbg\",",
      "      \"request\": \"launch\",",
      "      \"targetArchitecture\":\"x86_64\",",
      paste("      \"program\": \"", R.path, "\",", sep=""),
      "      \"args\": [\"--no-save\", \"--no-restore\"],",
      "      \"stopAtEntry\": false,",
      paste("      \"cwd\": \"", working.dir, "\",", sep=""),
      paste("      \"environment\": [{\"name\":\"R_PROFILE_USER\",\"value\":\"",Rprofile.path,"\"}, {\"name\":\"R_HOME\",\"value\":\"",R.home(),"\"}],", sep=""),
      paste("      \"externalConsole\": ", if (IsDarwin) "false" else "true",",", sep=""),
      paste("      \"MIMode\": \"", debug.app, "\",", sep=""),
      paste("      \"miDebuggerPath\": \"", gdb.path, "\",", sep=""),
      "      \"setupCommands\": [",
      "        {",
      paste("          \"description\": \"Enable pretty-printing for ", debug.app, "\",", sep=""),
      "          \"text\": \"-enable-pretty-printing\",",
      "          \"ignoreFailures\": true",
      "        }",
      "      ]",
      "    }",
      "  ]",
      "}")

      launch_file <- file(paste(vsCodeFolder, "/launch.json", sep=""), "wb")
      writeLines(launch_lines, launch_file)
      close(launch_file)

      # Helper to obtain c_cpp_properties.json name attribute
      getIntellisenseTargetName <- function()
      {
        TargetName <- ""
        OS         <- Sys.info()["sysname"]

        if (OS == "Windows")
        {
          TargetName <- "Win32"
        }
        else if (OS == "Linux")
        {
          TargetName <- "Linux"
        }
        else if (OS == "Darwin")
        {
          TargetName <- "Mac"
        }
        else
        {
          warning(paste("Unsupported intellisense target:", OS))
        }

        return (TargetName)
      }

      # create c_cpp_properties.json
      if (IsDarwin)
      {
        c_cpp_properties_lines <- c(
        "{",
        "  \"configurations\": [",
        "  {",
        paste("    \"name\": \"", getIntellisenseTargetName(), "\",", sep=""), 
        "    \"intelliSenseMode\": \"clang-x64\",",
        paste("    \"includePath\": [\"${workspaceFolder}\"", intellisense.includes, "],", sep=""),
        "    \"macFrameworkPath\": [\"/System/Library/Frameworks\"],",
        paste("    \"defines\": [", intellisense.defines, "],", sep=""),
        paste("    \"compilerPath\": \"", gcc.path, "\",", sep=""), 
        "    \"cStandard\": \"c89\",",
        "    \"cppStandard\": \"c++14\",",
        "    \"browse\": {",
        "       \"limitSymbolsToIncludedHeaders\": true,",
        "       \"databaseFilename\": \"\"",
        "      }",
        "    }",
        "  ],",
        "  \"version\": 4",
        "}")
      }
      else
      {
        c_cpp_properties_lines <- c(
        "{",
        "  \"configurations\": [",
        "  {",
        paste("    \"name\": \"", getIntellisenseTargetName(), "\",", sep=""), 
        "    \"intelliSenseMode\": \"gcc-x64\",",
        paste("    \"includePath\": [\"${workspaceFolder}\"", intellisense.includes, "],", sep=""),
        paste("    \"defines\": [", intellisense.defines, "],", sep=""),
        paste("    \"compilerPath\": \"", gcc.path, "\",", sep=""), 
        "    \"cStandard\": \"c89\",",
        "    \"cppStandard\": \"c++14\",",
        "    \"browse\": {",
        "       \"limitSymbolsToIncludedHeaders\": true,",
        "       \"databaseFilename\": \"\"",
        "      }",
        "    }",
        "  ],",
        "  \"version\": 4",
        "}")
      }

      c_cpp_properties_file <- file(paste(vsCodeFolder, "/c_cpp_properties.json", sep=""), "wb")
      writeLines(c_cpp_properties_lines, c_cpp_properties_file)
      close(c_cpp_properties_file)

      # save session state / loaded packages and DLLs
      .Object@DebugState <- list(session.packages=(.packages()), session.sharedLibraries=getLoadedSharedLibraries())
      BSysDebugProject   <- .Object

      save(BSysDebugProject, file=debugProjectPath)

      # save the current R session for use in debug session 
      save.image(file=debugSessionPath)

      # spawn Visual Studio Code
      system(paste("code ", sourcePath(.Object) ,".", sep=""), wait=FALSE)
    }
    else
    {
      current.packages <- (.packages())

      for (package in .Object@DebugState$session.packages)
      {
        if (!(package %in% current.packages))
        {
          library(package, character.only=TRUE)
        }
      }

      for (sharedLibrary in .Object@DebugState$session.sharedLibraries)
      {
        dyn.load(dynlib(sharedLibrary))      
      }
    }
  }
)

