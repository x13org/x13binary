test <- function(){
  default.path <- file.path(path.package("x13binary"), "bin")
  if (.Platform$OS.type == "windows"){    
    x13.bin <- file.path(default.path, "x13ashtml.exe")
  } else {
    x13.bin <- file.path(default.path, "x13ashtml")
  }
  system(x13.bin)
}
