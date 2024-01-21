## This file prevents R from looking for a shared library

## We use it here to (on macOS) update the dynamic library information of the just-created binary
if (grepl("darwin", R.Version()$platform)) {
    links <- trimws(readLines(pipe("otool -L ../inst/bin/x13ashtml")))
    for (lib in c("libgfortran", "libquadmath")) {
        libexpr <- paste0("/.*/", lib, ".*dylib")
        input <- unique(regmatches(links, regexpr(libexpr, links)))
        if (length(input)) {
            output <- file.path(R.home('lib'), basename(input))
            if (file.exists(output)) {
                system2('install_name_tool', c('-change', input, output, "../inst/bin/x13ashtml"))
            }
        }
    }
}
