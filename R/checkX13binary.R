checkX13binary <- function(){
  if (.Platform$OS.type == "windows"){    
    x13.bin <- system.file("bin", "x13ashtml.exe", package="x13binary")
  } else {
    if (!Sys.info()["sysname"] %in% c("Darwin", "Linux")){
      return(message("Unusual platform: ", Sys.info()["sysname"], 
                     "\nFor this platform, there are currently no binaries of X-13ARIMA-SEATS."))
    }
    x13.bin <- system.file("bin", "x13ashtml", package="x13binary")
  }
  if (x13.bin == ""){
    stop("X-13 binary file not found")
  }
  tdir <- tempdir()
  file.copy(system.file("testdata", "Testairline.spc", package="x13binary"), tdir)
  if (.Platform$OS.type == "windows") {
    # shell() gives a more verbose output on windows
    sout <- shell(paste(x13.bin, file.path(tdir, "Testairline")), intern = TRUE)
    if (isTRUE(attr(sout,"status") != 0)){
      stop("When running\n\n  ", x13.bin, 
           "\n\nCommand Prompt returned the following message:\n\n", 
           paste(strwrap(sout, indent = 2, exdent = 2), collapse = "\n"),
           "\n\n")
    }
  } else {
    sout <- system(paste(x13.bin, file.path(tdir, "Testairline")))
    if (sout != 0) {
      stop("Call to X-13 had non zero exit status")
    }
  }
  if (!file.exists(file.path(tdir, "Testairline.html"))){
    stop("X-13 has run but has not produced Testairline.html")
  }
  message("X-13 has run successfully")
}
