## x13binary: X-13ARIMA-SEATS Binary for R

[![Build status](https://ci.appveyor.com/api/projects/status/tjwhvfj6l19sq48p?svg=true)](https://ci.appveyor.com/project/christophsax/x13binary) 
[![CI](https://github.com/x13org/x13binary/workflows/ci/badge.svg)](https://github.com/x13org/x13binary/actions?query=workflow%3Aci)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 
[![CRAN](http://www.r-pkg.org/badges/version/x13binary)](https://cran.r-project.org/package=x13binary) 
[![Downloads](http://cranlogs.r-pkg.org/badges/x13binary?color=brightgreen)](https://cran.r-project.org/package=x13binary)

### About

This package provides an installer for [R](https://www.r-project.org) to
access prebuilt binaries of [X-13ARIMA-SEATS](https://www.census.gov/data/software/x13as.X-13ARIMA-SEATS.html) from the sibbling
repository [x13prebuilt](https://github.com/x13org/x13prebuilt). This allows
for fully automated installation of a
[X-13ARIMA-SEATS](https://www.census.gov/data/software/x13as.X-13ARIMA-SEATS.html) binary simply by
adding `Depends: x13binary` to your R package.

### Installation

As the package [is on CRAN](https://cran.r-project.org/package=x13binary),
the usual procedure applies:

```r
install.packages("x13binary")
```

### Status

This package as well as the corresponding
[x13prebuilt](https://github.com/x13org/x13prebuilt) repository are
operational for Windows, OS X (Darwin) and Linux (via using statically linked
binaries).

The current version of [x13binary](https://github.com/x13org/x13binary) uses
**version 1.1, build 57** of of X-13, as can be verified by:

```
seasonal::udg(seasonal::seas(AirPassengers), c("version", "build"))
```


### Author 

Dirk Eddelbuettel and Christoph Sax

### License

GPL (>= 2)
