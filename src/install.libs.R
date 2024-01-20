# This file prevents R from looking for a shared library
if(grepl("darwin", R.Version()$platform)){
  links <- trimws(readLines(pipe("otool -L ../inst/bin/x13ashtml")))
  input <- unique(regmatches(links, regexpr("/.*/libgfortran.*dylib", links)))
  if(length(input)){
    output <- file.path(R.home('lib'), basename(input))
    if(file.exists(output)){
      system2('install_name_tool', c('-change', input, output, "../inst/bin/x13ashtml"))
    }
  }
}
