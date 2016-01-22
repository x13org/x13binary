## x13binary [![Build Status](https://travis-ci.org/x13org/x13binary.svg)](https://travis-ci.org/x13org/x13binary) [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN](http://www.r-pkg.org/badges/version/x13binary)](http://cran.rstudio.com/package=x13binary) [![Downloads](http://cranlogs.r-pkg.org/badges/x13binary?color=brightgreen)](http://cran.rstudio.com/package=x13binary)

X-13ARIMA-SEATS Binary for R

### About

This package provides an installer for [R](http://www.r-project.org) to
access prebuilt binaries of [X-13ARIMA-SEATS](https://www.census.gov/srd/www/x13as/) from the sibbling
repository [x13prebuilt](https://github.com/x13org/x13prebuilt). This allows
for fully automated installation of a
[X-13ARIMA-SEATS](https://www.census.gov/srd/www/x13as/) binary simply by
adding `Depends: x13binary` to your R package.

### Installation

As the package [is on CRAN](http://cloud.r-project.org/package=x13binary),
the usual procedure applies:

```r
install.packages("x13binary")
```

### Status

This package as well as the corresponding
[x13prebuilt](https://github.com/x13org/x13prebuilt) repository are
operational for Windows, OS X (Darwin) and Linux (via using statically linked
binaries).

### Author 

Dirk Eddelbuettel and Christoph Sax

### License

GPL (>= 2)
