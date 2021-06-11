# -----------------------------------------------------------------------------
# BuildSys.R
# -----------------------------------------------------------------------------
# Implements an R based build system for making and debugging C/C++ dlls
#
# By Paavo Jumppanen
# Copyright (c) 2020-2021, CSIRO Marine and Atmospheric Research
# License: GPL-2
# -----------------------------------------------------------------------------


dynlib <- function(BaseName)
{
  LibName <- paste(BaseName, .Platform$dynlib.ext, sep="")

  return (LibName)
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
  function(.Object, Filename, SrcFolder, IncludeFolder, Type)
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

    FilePath <- paste0(SrcFolder, Filename)

    return (buildDependencies(.Object, FilePath))
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
setClass("BSysProject",
  slots = c(
    ProjectName         = "character",
    WorkingFolder       = "character",
    SourceName          = "character",
    IncludeName         = "character",
    ObjName             = "character",
    InstallLibraryName  = "character",
    InstallIncludeName  = "character",    
    Flat                = "logical",
    SourceFiles         = "list",
    Packages            = "character",
    Includes            = "character",
    Defines             = "character",
    Libraries           = "character",
    CFLAGS              = "character",
    CXXFLAGS            = "character",
    FFLAGS              = "character",
    LDFLAGS             = "character", 
    LDLIBS              = "character", 
    DEFINES             = "character", 
    IsDebug             = "logical",
    DebugState          = "list"
  )
)


# -----------------------------------------------------------------------------
# Constructor for CodeProject class
# -----------------------------------------------------------------------------
setMethod("initialize", "BSysProject",
  function(.Object, 
           WorkingFolder=NULL, 
           Name="",
           SourceFiles=NULL,
           SourceName="src",
           IncludeName="include",
           ObjName="obj",
           InstallLibraryName=as.character(NULL),
           InstallIncludeName=as.character(NULL),
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
    .Object@ProjectName         <- ""
    .Object@WorkingFolder       <- ""
    .Object@SourceName          <- ""
    .Object@IncludeName         <- ""
    .Object@ObjName             <- ""
    .Object@InstallLibraryName  <- as.character(NULL)
    .Object@InstallIncludeName  <- as.character(NULL)
    .Object@Flat                <- TRUE
    .Object@SourceFiles         <- list()
    .Object@Packages            <- Packages
    .Object@Includes            <- c(R.home("include"), Includes)
    .Object@Defines             <- Defines
    .Object@Libraries           <- Libraries
    .Object@CFLAGS              <- CFLAGS
    .Object@CXXFLAGS            <- CXXFLAGS
    .Object@FFLAGS              <- FFLAGS
    .Object@LDFLAGS             <- LDFLAGS
    .Object@LDLIBS              <- LDLIBS
    .Object@DEFINES             <- DEFINES 
    .Object@IsDebug             <- Debug
    .Object@DebugState          <- list()

    return (initProjectFromFolder(.Object,
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
                                  Debug))
  }
)


# -----------------------------------------------------------------------------
# Method to initialise Project based on folder contents 
# -----------------------------------------------------------------------------
setGeneric("initProjectFromFolder", function(.Object, ...) standardGeneric("initProjectFromFolder"))

setMethod("initProjectFromFolder", "BSysProject",
  function(.Object, 
           WorkingFolder=NULL,
           Name="",
           SourceFiles=NULL, 
           SourceName="src",
           IncludeName="include",
           ObjName="obj",
           InstallLibraryName=as.character(NULL),
           InstallIncludeName=as.character(NULL),
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
    if (is.null(WorkingFolder))
    {
      stop("WorkingFolder not specified. Please specify a WorkingFolder.")
    }
    
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

    testFolder <- function(Path)
    {
      if (!dir.exists(Path))
      {
        warning("The folder", Path, "does not exist.\n")
      }
    }

    addExternalDependencies <- function(SourceFile, SrcFolder, IncludeFolder, CodeProject)
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
                           add.abort=TRUE,      
                           defs=c(DefLIB_UNLOAD), 
                           libs=as.character(c()))
      
      RcppEigenDep <- list(pkg="RcppEigen", 
                           add.abort=TRUE,      
                           defs=c(DefLIB_UNLOAD), 
                           libs=as.character(c()))

      TMBDep       <- list(pkg="TMB",       
                           add.abort=TRUE,      
                           defs=c(DefTMB_SafeBounds, DefLIB_UNLOAD, DefTMB_LIB_INIT), 
                           libs=as.character(c()))

      KnownPackageDependencies <- list(Rcpp.h      =list(RcppDep), 
                                       RcppEigen.h =list(RcppDep, RcppEigenDep), 
                                       TMB.hpp     =list(TMBDep, RcppDep, RcppEigenDep))

      AddAbort <- FALSE

      for (External in SourceFile@Externals)
      {
        PackageDependencies <- KnownPackageDependencies[[External]]

        if (!is.null(PackageDependencies))
        {
          for (PackageDependency in PackageDependencies)
          {
            AddAbort <- AddAbort || PackageDependency$add.abort

            if (!PackageDependency$pkg %in% CodeProject@Packages)
            {
              CodeProject@Packages <- c(CodeProject@Packages, PackageDependency$pkg)
              IncludePath          <- getPackagePath(PackageDependency$pkg, "/include")

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

      if (AddAbort)
      {
        AbortOverrideCode <- c("//----------------------------------------------",
                               "// BuildSys standard C library abort() override.", 
                               "//----------------------------------------------",
                               "",
                               "#include <stdexcept>",
                               "",
                               "extern \"C\" void abort(void)",
                               "{",
                               "  // If you are here then your code has called abort.", 
                               "  // We throw bad_alloc() cos that is what is caught in TMB,",
                               "  // Rcpp and RcppEigen exception handling.", 
                               "  throw std::bad_alloc();",
                               "}",
                               "")

        AbortSourceFilePath <- paste0(SrcFolder, "bsys_abort.cpp")

        if (!file.exists(AbortSourceFilePath))
        {
          abortSourceFile <- file(AbortSourceFilePath, "wt")
          writeLines(AbortOverrideCode, abortSourceFile)
          close(abortSourceFile)
        }

        SourceFile <- new("BSysSourceFile", "bsys_abort.cpp", SrcFolder, IncludeFolder, "cpp")

        CodeProject@SourceFiles[[length(.Object@SourceFiles) + 1]] <- SourceFile
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

    .Object@WorkingFolder       <- FullPath
    .Object@ProjectName         <- Name
    .Object@SourceName          <- ""
    .Object@IncludeName         <- ""
    .Object@ObjName             <- ""
    .Object@InstallLibraryName  <- as.character(NULL)
    .Object@InstallIncludeName  <- as.character(NULL)
    .Object@Flat                <- Flat
    .Object@SourceFiles         <- list()
    .Object@Packages            <- Packages
    .Object@Includes            <- c(R.home("include"), Includes)
    .Object@Defines             <- Defines
    .Object@Libraries           <- Libraries
    .Object@CFLAGS              <- CFLAGS
    .Object@CXXFLAGS            <- CXXFLAGS
    .Object@FFLAGS              <- FFLAGS
    .Object@LDFLAGS             <- LDFLAGS
    .Object@LDLIBS              <- LDLIBS
    .Object@DEFINES             <- DEFINES
    .Object@IsDebug             <- Debug
    .Object@DebugState          <- list()

    testFolder(.Object@WorkingFolder)

    if (!Flat)
    {
      .Object@SourceName  <- addSlash(SourceName)
      .Object@IncludeName <- addSlash(IncludeName)
      .Object@ObjName     <- addSlash(ObjName)
      ObjectFolder        <- paste(.Object@WorkingFolder, .Object@ObjName, sep="")

      testFolder(ObjectFolder)
    } 

    if (!identical(character(0), InstallLibraryName))
    {
      .Object@InstallLibraryName <- addSlash(InstallLibraryName)
      InstallLibraryFolder       <- paste(.Object@WorkingFolder, .Object@InstallLibraryName, sep="")

      testFolder(InstallLibraryFolder)
    }

    if (!identical(character(0), InstallIncludeName))
    {
      .Object@InstallIncludeName <- addSlash(InstallIncludeName)
    }

    SrcFolder     <- paste(.Object@WorkingFolder, .Object@SourceName, sep="")
    IncludeFolder <- paste(.Object@WorkingFolder, .Object@IncludeName, sep="")
    
    testFolder(SrcFolder)
    testFolder(IncludeFolder)

    if (is.null(SourceFiles))
    {
      AllFiles <- dir(SrcFolder)
    }
    else 
    {
      AllFiles <- SourceFiles
    }

    for (File in AllFiles)
    {
      if (grepl("bsys_abort.cpp$", File))
      {
        # ignore as this is auto-created and will be re-created
      }
      else if (grepl("\\.c$", File))
      {
        # c source file
        SourceFile <- new("BSysSourceFile", File, SrcFolder, IncludeFolder, "c")

        .Object@SourceFiles[[length(.Object@SourceFiles) + 1]] <- SourceFile 
      }
      else if (grepl("\\.cpp$", File))
      {
        # c++ source file
        SourceFile <- new("BSysSourceFile", File, SrcFolder, IncludeFolder, "cpp")

        .Object@SourceFiles[[length(.Object@SourceFiles) + 1]] <- SourceFile 
      }
      else if ((grepl("\\.f$|\\.for$|\\.f95$|\\.f90$|\\.f77$", File)))
      {
        # fortran source file
        SourceFile <- new("BSysSourceFile", File, SrcFolder, IncludeFolder, "f")

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
      .Object <- addExternalDependencies(SourceFile, SrcFolder, IncludeFolder, .Object)
    }

    return (.Object)
  }
)


# -----------------------------------------------------------------------------
# Method to supress printing entire object 
# -----------------------------------------------------------------------------
setMethod("show", "BSysProject",
  function(object)
  {
    cat(paste("BuildSys Project:", 
              object@ProjectName, 
              "\nWorking Folder:",
              object@WorkingFolder,
              "\n"))
  }
)


# -----------------------------------------------------------------------------
# Method to build makefile 
# -----------------------------------------------------------------------------
setGeneric("buildMakefile", function(.Object, ...) standardGeneric("buildMakefile"))

setMethod("buildMakefile", "BSysProject",
  function(.Object, Force=FALSE)
  {
    # -------------------------------------------------------------------------
    # idStamp() creates a stamp to check staleness of makefile
    # -------------------------------------------------------------------------
    idStamp <- function()
    {
      MakeID  <- digest::digest(.Object, algo="md5")
      IdStamp <- paste("# MakeID:", MakeID, "--Do not edit this line")

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

      ObjName             <- dropLeadingTrailingSlashes(.Object@ObjName)
      SourceName          <- dropLeadingTrailingSlashes(.Object@SourceName)
      IncludeName         <- dropLeadingTrailingSlashes(.Object@IncludeName)
      InstallLibraryName  <- NULL
      InstallIncludeName  <- NULL

      if (!identical(character(0), .Object@InstallLibraryName))
      {
        InstallLibraryName <- dropLeadingTrailingSlashes(.Object@InstallLibraryName)
      }

      if (!identical(character(0), .Object@InstallIncludeName))
      {
        InstallIncludeName <- dropLeadingTrailingSlashes(.Object@InstallIncludeName)
      }

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

      LDFLAGS <- c("-shared")
      WinSub  <- ""

      if (Sys.info()["sysname"] == "Windows")
      {
        if (Sys.info()["machine"]=="x86-64")
        {
          WinSub <- "x64/"
        }
        else
        {
          WinSub <- "x32/"
        }
      }
      else
      {
        COMMONFLAGS <- c(COMMONFLAGS, "-fPIC")
        LDFLAGS     <- c(LDFLAGS, "-fPIC")
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

      IncludeRelativePath <- ""

      if (nchar(IncludeName) != 0)
      {
        IncludeRelativePath <- paste(RootRelativePath, IncludeName, "/", sep="")
        COMMONFLAGS         <- c(COMMONFLAGS, paste("-I", IncludeRelativePath, sep=""))
      }

      for (Include in .Object@Includes)
      {
        COMMONFLAGS <- c(COMMONFLAGS, paste("-I", Include, sep=""))
      }

      LDFLAGS  <- c(LDFLAGS, .Object@LDFLAGS)
      CFLAGS   <- c("$(COMMONFLAGS)", .Object@CFLAGS)
      CXXFLAGS <- c("$(COMMONFLAGS)", "-Wno-ignored-attributes", .Object@CXXFLAGS)
      FFLAGS   <- c("$(COMMONFLAGS)", .Object@FFLAGS)

      gcc.path <- normalizePath(Sys.which("gcc"), "/", mustWork=FALSE)
      gcc.dir  <- ""

      if (grepl("mingw", gcc.path))
      {
        # windows
        # Need the correct compiler for the architecture. Sys.which() just picks up whichever one is in the PATH
        if (Sys.info()["machine"]=="x86-64")
        {
          # mingw64
          gcc.path <- sub("/mingw\\d\\d", "/mingw64", gcc.path)
          WinSub   <- "x64/"
        }
        else
        {
          # mingw32
          gcc.path <- sub("/mingw\\d\\d", "/mingw32", gcc.path)
          WinSub   <- "x32/"
        }

        gcc.dir <- sub("/gcc.*", "/", gcc.path)
      }
      
      # Build makefile
      MakefileTxt <-c(
        idStamp(),
        paste("R_SHARE_DIR=", R.home("share"), sep=""),
        paste("R_HOME=", R.home(), sep=""),
        paste("include $(R_HOME)/etc/", WinSub, "Makeconf", sep=""), 
        paste("CC=", gcc.dir, "gcc", sep=""),
        paste("CXX=", gcc.dir, "g++", sep=""),
        paste("FC=", gcc.dir, "gfortran", sep=""),
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
        paste("\t$(CXX) -o ", DlibName, " $(LDFLAGS) $(objects) $(LDLIBS) $(LIBR)", sep=""),
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

      # if install paths are define then create install rule      
      if (!is.null(InstallLibraryName) || !is.null(InstallIncludeName))
      {
        MakefileTxt <- c(MakefileTxt, "install :")
      
        if (!is.null(InstallLibraryName))
        {
          InstallLibraryRelativePath <- paste(RootRelativePath, InstallLibraryName, sep="")

          MakefileTxt <- c(MakefileTxt, 
                           paste("\tcp", DlibName, InstallLibraryRelativePath))
        }

        if (!is.null(InstallIncludeName))
        {
          InstallIncludeRelativePath <- paste(RootRelativePath, InstallIncludeName, sep="")

          MakefileTxt <- c(MakefileTxt, 
                           paste("\tcp ", IncludeRelativePath, "* ", InstallIncludeRelativePath, sep=""))
        }

        MakefileTxt <- c(MakefileTxt, "")
      }

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

    Created <- FALSE

    if (!checkMakefile(MakefilePath) || Force)
    {
      createMakefile(MakefilePath)

      Created <- TRUE
    }

    return (Created)   
  }
)


# -----------------------------------------------------------------------------
# Method to build dynamic library project 
# -----------------------------------------------------------------------------
setGeneric("make", function(.Object, ...) standardGeneric("make"))

setMethod("make", "BSysProject",
  function(.Object, Operation="", Debug=NULL)
  {
    runMake <- function(.Object, Operation)
    {
      quoteArg <- function(arg)
      {
        return (paste0("\"", arg, "\""))
      }

      IsWindows <- (Sys.info()["sysname"] == "Windows")
      DlibName  <- dynlib(.Object@ProjectName)

      ObjFolder    <- paste(.Object@WorkingFolder, .Object@ObjName, sep="")
      CapturePath  <- paste(.Object@WorkingFolder, .Object@ProjectName, ".log", sep="")
      ScriptPath   <- paste(.Object@WorkingFolder, .Object@ProjectName, ".sh", sep="")
      FinishedFile <- paste(.Object@WorkingFolder, .Object@ProjectName, ".fin", sep="")

      hasTee <- function()
      {
        TestCmd <- "tee --version &>/dev/null"

        # construct test script to see if tee is present
        BashScript <- c("#!/bin/bash",
                        TestCmd)

        ScriptFile <- file(ScriptPath, "wt")
        writeLines(BashScript, ScriptFile)
        close(ScriptFile)

        command.line <- paste(Sys.which("bash"), quoteArg(ScriptPath))
        result <- try(system(command.line, wait=TRUE), silent=TRUE)
        unlink(ScriptFile)

        hasTee <- ((class(result) != "try-error") && (result == 0))

        return (hasTee)
      }

      HasTee      <- hasTee()
      CaptureCmd  <- if (IsWindows && HasTee) paste("2>&1 | tee", quoteArg(CapturePath)) else ""
      
      # run make
      if (Operation == "clean")
      {
        operation <- paste("cd", quoteArg(ObjFolder), "\nmake clean", CaptureCmd)
      } 
      else if (Operation == "install")
      {
        operation <- paste("cd", quoteArg(ObjFolder), "\nmake install", CaptureCmd)
      } 
      else if (Operation == "")
      {
        operation <- paste("cd", quoteArg(ObjFolder), "\nmake", CaptureCmd)
      }
      else
      {
        stop("Undefined make() Operation")
      }

      # construct caller script
      BashScript <- c("#!/bin/bash",
                      operation,
                      paste("echo finished >", quoteArg(FinishedFile)))

      ScriptFile <- file(ScriptPath, "wt")
      writeLines(BashScript, ScriptFile)
      close(ScriptFile)

      command.line <- paste(Sys.which("bash"), quoteArg(ScriptPath))
      unlink(FinishedFile)
      
      unloadLibrary(.Object)

      if (IsWindows && HasTee)
      {
        system(command.line, wait=FALSE, invisible=FALSE)
      }
      else
      {
        system(command.line, wait=TRUE)
      }

      # test for completion of script. We do this rather than using
      # wait=TRUE in system call so that we can make a visible shell that 
      # shows live progress. With wait=TRUE a visible shell shows no
      # output. 
      while (!file.exists(FinishedFile))
      {
        Sys.sleep(1)
      }

      unlink(FinishedFile)
      unlink(ScriptPath)

      if (IsWindows && HasTee)
      {
        CaptureFile <- file(CapturePath, "rt")
        writeLines(readLines(CaptureFile))
        close(CaptureFile)
        unlink(CapturePath)
      }
    }

    # if Debug provided and different from current update 
    if (!is.null(Debug) && (Debug != .Object@IsDebug))
    {
      .Object@IsDebug <- Debug
    }

    # build makefile
    if (buildMakefile(.Object))
    {
      if (Operation != "clean")
      {
        # as the makefile has changed do a clean to force a complete re-build
        runMake(.Object, "clean")
      }
    }

    runMake(.Object, Operation)

    return (.Object)   
  }
)


# -----------------------------------------------------------------------------
# Method to get library path
# -----------------------------------------------------------------------------
setGeneric("libraryPath", function(.Object, ...) standardGeneric("libraryPath"))

setMethod("libraryPath", "BSysProject",
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

setMethod("sourcePath", "BSysProject",
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

setMethod("includePath", "BSysProject",
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

setMethod("objPath", "BSysProject",
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
# Method to get install library path
# -----------------------------------------------------------------------------
setGeneric("installLibraryPath", function(.Object, ...) standardGeneric("installLibraryPath"))

setMethod("installLibraryPath", "BSysProject",
  function(.Object)
  {
    InstallLibraryPath <- NULL

    if (!identical(character(0), .Object@InstallLibraryName))
    {
      InstallLibraryPath <- .Object@WorkingFolder

      if (nchar(.Object@InstallLibraryName) != 0)
      {
        InstallLibraryPath <- paste(InstallLibraryPath, .Object@InstallLibraryName, sep="")
      }
    }

    return (InstallLibraryPath)
  }
)


# -----------------------------------------------------------------------------
# Method to get install include path
# -----------------------------------------------------------------------------
setGeneric("installIncludePath", function(.Object, ...) standardGeneric("installIncludePath"))

setMethod("installIncludePath", "BSysProject",
  function(.Object)
  {
    InstallIncludePath <- NULL

    if (!identical(character(0), .Object@InstallIncludeName))
    {
      InstallIncludePath <- .Object@WorkingFolder

      if (nchar(.Object@InstallIncludeName) != 0)
      {
        InstallIncludePath <- paste(InstallIncludePath, .Object@InstallIncludeName, sep="")
      }
    }

    return (InstallIncludePath)
  }
)


# -----------------------------------------------------------------------------
# Method to load library 
# -----------------------------------------------------------------------------
setGeneric("loadLibrary", function(.Object, ...) standardGeneric("loadLibrary"))

setMethod("loadLibrary", "BSysProject",
  function(.Object)
  {
    return (dyn.load(libraryPath(.Object)))
  }
)


# -----------------------------------------------------------------------------
# Method to unload library 
# -----------------------------------------------------------------------------
setGeneric("unloadLibrary", function(.Object, ...) standardGeneric("unloadLibrary"))

setMethod("unloadLibrary", "BSysProject",
  function(.Object)
  {
    libPath <- libraryPath(.Object)
    tr      <- try(dyn.unload(libraryPath(.Object)), silent=TRUE)

    if (!is(tr, "try-error")) 
    {
      message("Note: Library", libPath, "was unloaded.\n")
    }
  }
)


# -----------------------------------------------------------------------------
# Method to cleanup project of created files and folders 
# -----------------------------------------------------------------------------
setGeneric("clean", function(.Object, ...) standardGeneric("clean"))

setMethod("clean", "BSysProject",
  function(.Object)
  {
    # remove makefile
    if (nchar(.Object@ObjName) != 0)
    {
      MakefilePath <- paste(.Object@WorkingFolder, .Object@ObjName, "/makefile", sep="")
    }
    else
    {
      MakefilePath <- paste(.Object@WorkingFolder, "makefile", sep="")
    }

    unlink(MakefilePath)

    # remove debug related files and folders
    RprofileFolder        <- paste(sourcePath(.Object), .Object@ProjectName, ".Rprof", sep="")
    debugProjectPath      <- paste(RprofileFolder, "/", .Object@ProjectName, "_DebugProject.RData", sep="")
    debugSessionPath      <- paste(RprofileFolder, "/", .Object@ProjectName, "_DebugSession.RData", sep="")
    debugCmdTxtPath       <- paste(RprofileFolder, "/debugCmd.txt", sep="")
    Rprofile.path         <- paste(RprofileFolder, "/.Rprofile", sep="")
    bsysAbortPath         <- paste(sourcePath(.Object), "bsys_abort.cpp", sep="")

    unlink(debugProjectPath)
    unlink(debugSessionPath)
    unlink(Rprofile.path)
    unlink(bsysAbortPath)
    unlink(debugCmdTxtPath)
    unlink(RprofileFolder, recursive=TRUE, force=TRUE)

    vsCodeFolder          <- paste(sourcePath(.Object), ".vscode", sep="")
    launch.file           <- paste(vsCodeFolder, "/launch.json", sep="")
    c_cpp_properties.file <- paste(vsCodeFolder, "/c_cpp_properties.json", sep="")

    unlink(launch.file)
    unlink(c_cpp_properties.file)
    unlink(vsCodeFolder, recursive=TRUE, force=TRUE)
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

setMethod("vcDebug", "BSysProject",
  function(.Object, LaunchEditor=TRUE)
  {
    RprofileFolder   <- paste(sourcePath(.Object), .Object@ProjectName, ".Rprof", sep="")
    debugProjectPath <- paste(RprofileFolder, "/DebugProject.RData", sep="")
    debugSessionPath <- paste(RprofileFolder, "/DebugSession.RData", sep="")
    debugCmdFilePath <- paste(RprofileFolder, "/debugCmd.txt", sep="")
    Rprofile.path    <- paste(RprofileFolder, "/.Rprofile", sep="")

    if (LaunchEditor)
    {
      # Helper to obtain info for json files
      getIntellisenseInfo <- function()
      {
        TargetName   <- ""
        OS           <- Sys.info()[["sysname"]]
        Architecture <- Sys.info()[["machine"]]
        Extra        <- NULL
        ShortPath    <- function(arg) {return (arg)}

        if (OS == "Windows")
        {
          TargetName <- "Win32"
          Mode       <- "gcc"    
          ShortPath  <- function(arg) {gsub("\\\\", "/", utils::shortPathName(arg))}
        }
        else if (OS == "Linux")
        {
          TargetName <- "Linux"
          Mode       <- "gcc"    
        }
        else if (OS == "Darwin")
        {
          TargetName <- "Mac"
          Mode       <- "clang"
          Extra      <- "    \"macFrameworkPath\": [\"/System/Library/Frameworks\"],"
        }
        else
        {
          warning(paste("Unsupported intellisense target:", OS,"\n"))
        }

        if (Architecture =="x86-64")
        {
          Architecture <- "x86_64"
          Mode         <- paste(Mode, "-x64", sep="")
        }
        else if (Architecture =="x86_64")
        {
          Mode         <- paste(Mode, "-x64", sep="")
        }
        else if (Architecture == "x86")
        {
          Mode         <- paste(Mode, "-x86", sep="")
        }
        else
        {
          warning("Unknown intellisense architecture\n")
        }

        return (list(TargetName=TargetName, Architecture=Architecture, Mode=Mode, normPath=ShortPath))
      }

      # create Rprofile folder
      file.attr    <- file.info(RprofileFolder)
      
      if (is.na(file.attr$size))
      {
        dir.create(RprofileFolder)
      }
      else if (!file.attr$isdir)
      {
        warning(paste("Cannot create", RprofileFolder, "folder as .vscode file exists.\n"))
      }

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
        warning("Cannot create .vscode folder as .vscode file exists.\n")
      }

      debug.app          <- "gdb"
      external.console   <- "true"
      R.args             <- "\"--no-save\", \"--no-restore\""
      debug.command.args <- ""
      debug.Cmd.lines    <- c()

      # get needed paths
      if (IsDarwin)
      {
        VisualStudioCode <- "open -a Visual\\ Studio\\ Code.app --args"
        R.path           <- "/Applications/R.app/Contents/MacOS/R"        

        if (!file.exists(R.path))
        {
          warning("Cannot find R.\n")
        }
        
        gdb.path  <- "/Applications/Xcode.app/Contents/Developer/usr/bin/lldb"
        debug.app <- "lldb"

        if (!file.exists(gdb.path))
        {
          warning("Cannot find lldb-mi. Ensure Xcode is installed.\n")
        }

        external.console   <- "false"
        R.args             <- paste("\"", RprofileFolder, "\"", sep="")
        debug.command.args <- paste("--source", debugCmdFilePath)
        debug.Cmd.lines    <- c("breakpoint set -f bsys_abort.cpp -b abort")
      }
      else
      {
        VisualStudioCode <- "code"
        R.path           <- normalizePath(Sys.which("Rgui"), "/", mustWork=FALSE)

        if (nchar(R.path) == 0)
        {
          R.path <- paste(R.home(), "/bin/exec/R", sep="")
        }

        gdb.path <- normalizePath(Sys.which(debug.app), "/", mustWork=FALSE)

        if (nchar(gdb.path) == 0)
        {
          warning(paste("Cannot find path to gdb. Check that", debug.app, "is accessible via the PATH environment variable.\n"))
        }

        debug.command.args <- paste("--init-command", debugCmdFilePath) 
        debug.Cmd.lines    <- c(debug.Cmd.lines, "set breakpoint pending on",
                                "break bsys_abort.cpp:abort")
      }

      # write debuggeer command file
      debugCmdfile <- file(debugCmdFilePath, "wt")
      writeLines(debug.Cmd.lines, debugCmdfile)
      close(debugCmdfile)

      gcc.path <- normalizePath(Sys.which("gcc"), "/", mustWork=FALSE)

      if (nchar(gcc.path) == 0)
      {
        warning("Cannot find path to gcc. Check that gcc is accessible via the PATH environment variable.\n")
      }

      # build intellisense include paths
      R.include             <- R.home("include")
      intellisense.includes <- ""
      intellisense.defines  <- paste(sapply(.Object@Defines, function(str) {paste("\"", str, "\"", sep="")}), collapse=",", sep="")
      intellisense.info     <- getIntellisenseInfo()
      normPath              <- intellisense.info$normPath

      if (grepl("mingw", gcc.path))
      {
        # windows
        # Need the correct compiler for the architecture. Sys.which() just picks up whichever one is in the PATH
        if (Sys.info()["machine"]=="x86-64")
        {
          # mingw64
          gcc.path <- sub("/mingw\\d\\d", "/mingw64", gcc.path)
        }
        else
        {
          # mingw32
          gcc.path <- sub("/mingw\\d\\d", "/mingw32", gcc.path)
        }

        rtools.path           <- sub("/mingw.*", "/", gcc.path)
        gcc.include           <- sub("/bin/gcc.*", "/include", gcc.path)
        intellisense.includes <- paste(intellisense.includes, ",\"", gcc.include, "/**\"", sep="")

        root.include      <- paste(rtools.path, "usr/include", sep="")
        root.user.include <- paste(rtools.path, "usr/local/include", sep="")

        if (file.exists(root.include))
        {
          intellisense.includes <- paste(intellisense.includes, ",\"", normPath(root.include), "/**\"", sep="")
        }

        if (file.exists(root.user.include))
        {
          intellisense.includes <- paste(intellisense.includes, ",\"", normPath(root.user.include), "/**\"", sep="")
        }
      }
      else
      {
        # linux
        gcc.include       <- "/usr/include"
        gcc.local.include <- "/usr/local/include"

        if (file.exists(gcc.include))
        {
          intellisense.includes <- paste(intellisense.includes, ",\"", normPath(gcc.include), "/**\"", sep="")
        }

        if (file.exists(gcc.local.include))
        {
          intellisense.includes <- paste(intellisense.includes, ",\"", normPath(gcc.local.include), "/**\"", sep="")
        }
      }

      intellisense.includes <- paste(intellisense.includes, ",\"", normPath(R.include), "/**\"", sep="")
      intellisense.includes <- paste(intellisense.includes, ",\"", normPath(includePath(.Object)), "**\"", sep="")

      # add other include paths
      for (include in .Object@Includes)
      {
        intellisense.includes <-  paste(intellisense.includes, ",\"", normPath(include), "/**\"", sep="")
      }

      # create debugRprofile.txt environment setup file for gdb debug session
      working.dir <- .Object@WorkingFolder

      Rprofile_lines <- c(
      paste("require(BuildSys)", sep=""),
      paste("setwd(\"", working.dir, "\")", sep=""),
      paste("load(\"", debugSessionPath, "\")", sep=""),
      paste("load(\"", debugProjectPath, "\")", sep=""),
      "vcDebug(BSysDebugProject, FALSE)"
      )

      Rprofile_file <- file(Rprofile.path, "wb")
      writeLines(Rprofile_lines, Rprofile_file)
      close(Rprofile_file)

      StopAtEntry <- "false"
      
      # create launch.json
      if (IsDarwin)
      {
        # Configure for CodeLLDB  
        launch_lines <- c(
        "{",
        "  \"version\": \"0.2.0\",",
        "  \"configurations\": [",
        "    {",
        paste("      \"name\": \"(", debug.app, ") Launch\",", sep=""),
        "      \"type\": \"lldb\",",
        "      \"request\": \"launch\",",
        paste("      \"program\": \"", normPath(R.path), "\",", sep=""),
        paste("      \"args\": [", R.args, "],", sep=""),
        paste("      \"stopOnEntry\": ", StopAtEntry, ",", sep=""), 
        paste("      \"cwd\": \"", normPath(RprofileFolder), "\",", sep=""),
        paste("      \"env\": {\"name\":\"R_HOME\",\"value\":\"",R.home(),"\"},", sep=""),
        paste("      \"initCommands\": [",  paste(sapply(debug.Cmd.lines, function(x) {paste0("\"", x, "\"")}),collapse=","),"]", sep=""),
        "    }",
        "  ]",
        "}")
      }
      else
      {
        # Configure for LLDB-mi  
        launch_lines <- c(
        "{",
        "  \"version\": \"0.2.0\",",
        "  \"configurations\": [",
        "    {",
        paste("      \"name\": \"(", debug.app, ") Launch\",", sep=""),
        "      \"type\": \"cppdbg\",",
        "      \"request\": \"launch\",",
        paste("      \"targetArchitecture\":\"", intellisense.info$Architecture,"\",", sep=""),
        paste("      \"program\": \"", normPath(R.path), "\",", sep=""),
        paste("      \"args\": [", R.args, "],", sep=""),
        paste("      \"stopAtEntry\": ", StopAtEntry, ",", sep=""), 
        paste("      \"cwd\": \"", normPath(RprofileFolder), "\",", sep=""),
        paste("      \"environment\": [{\"name\":\"R_HOME\",\"value\":\"",R.home(),"\"}],", sep=""),
        paste("      \"externalConsole\": ", external.console, ",", sep=""),
        paste("      \"MIMode\": \"", debug.app, "\",", sep=""),
        paste("      \"miDebuggerPath\": \"", normPath(gdb.path), "\",", sep=""),
        paste("      \"miDebuggerArgs\": \"", debug.command.args, "\",", sep=""),     
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
      }

      launch_file <- file(paste(vsCodeFolder, "/launch.json", sep=""), "wb")
      writeLines(launch_lines, launch_file)
      close(launch_file)

      # create c_cpp_properties.json
      c_cpp_properties_lines <- c(
      "{",
      "  \"configurations\": [",
      "  {",
      paste("    \"name\": \"", intellisense.info$TargetName, "\",", sep=""), 
      paste("    \"intelliSenseMode\": \"", intellisense.info$Mode, "\",", sep=""),
      paste("    \"includePath\": [\"${workspaceFolder}\"", intellisense.includes, "],", sep=""),
      intellisense.info$Extra,
      paste("    \"defines\": [", intellisense.defines, "],", sep=""),
      paste("    \"compilerPath\": \"", normPath(gcc.path), "\",", sep=""), 
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
      tr <- try(system(paste(VisualStudioCode, " \"", sourcePath(.Object) ,".\"", sep=""), wait=FALSE), silent=TRUE)

      if (is(tr, "try-error"))
      {
        warning("Cannot find Visual Studio Code. Please ensure it is installed and reachable through the PATH environment variable.\n")
      } 
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

