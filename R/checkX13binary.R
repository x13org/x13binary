checkX13binary <- function(){
  if (.Platform$OS.type == "windows"){    
    x13.bin <- file.path(path.package("x13binary"), "bin", "x13ashtml.exe")
  } else {
    if (!Sys.info()["sysname"] %in% c("Darwin", "Linux")){
      return(message("Unusual platform: ", Sys.info()["sysname"], 
                     "\nFor this platform, there are currently no binaries of X-13ARIMA-SEATS."))
    }
    x13.bin <- file.path(path.package("x13binary"), "bin", "x13ashtml")
  }
  tdir <- tempdir()
  file.copy(file.path(path.package("x13binary"), "testdata", "Testairline.spc"), tdir)
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
