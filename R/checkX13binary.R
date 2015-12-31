checkX13binary <- function(){
  if (.Platform$OS.type == "windows"){    
    x13.bin <- file.path(path.package("x13binary"), "bin", "x13ashtml.exe")
  } else {
    x13.bin <- file.path(path.package("x13binary"), "bin", "x13ashtml")
  }
  tdir <- tempdir()
  file.copy(file.path(path.package("x13binary"), "testdata", "Testairline.spc"), tdir)
  sout <- system(paste(x13.bin, file.path(tdir, "Testairline")))
  if (sout != 0){
    stop("Call to X-13 hat non zero exit status")
  }
  if (!file.exists(file.path(tdir, "Testairline.html"))){
    stop("X-13 has run but has not produced Testairline.html")
  }
  message("X-13 has run successfully")
}

