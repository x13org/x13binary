## x13binary: X-13ARIMA-SEATS Binary for R

[![ci Mac, Ubuntu](https://github.com/x13org/x13binary/actions/workflows/ci.yaml/badge.svg)](https://github.com/x13org/x13binary/actions/workflows/ci.yaml)
[![ci Windows](https://github.com/x13org/x13binary/actions/workflows/ci-win.yaml/badge.svg)](https://github.com/x13org/x13binary/actions/workflows/ci-win.yaml)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 
[![CRAN](https://www.r-pkg.org/badges/version/x13binary)](https://cran.r-project.org/package=x13binary) 
[![Downloads](https://cranlogs.r-pkg.org/badges/x13binary?color=brightgreen)](https://cran.r-project.org/package=x13binary)

### About

This package provides binaries of [X-13ARIMA-SEATS](https://www.census.gov/data/software/x13as.X-13ARIMA-SEATS.html) and makes them available in [R](https://www.r-project.org). It builds them from the Fortran code provided by the US Census Bureau.
This allows for fully automated installation of a
[X-13ARIMA-SEATS](https://www.census.gov/data/software/x13as.X-13ARIMA-SEATS.html) binary simply by
adding `Depends: x13binary` to your R package.

### Installation

As the package [is on CRAN](https://cran.r-project.org/package=x13binary),
the usual procedure applies:

```r
install.packages("x13binary")
```

### Status

The current version of [x13binary](https://github.com/x13org/x13binary) uses
**version 1.1, build 61** of of X-13, as can be verified by:

```
seasonal::udg(seasonal::seas(AirPassengers), c("version", "build"))
```


### License Information and Disclaimer

As stated in the manual of
[X-13ARIMA-SEATS](https://web.archive.org/web/20250412173420/https://www2.census.gov/software/x-13arima-seats/x13as/windows/documentation/docx13as.pdf)
(June 12, 2023):



> This Software was created by U.S. Government employees and therefore is not
> subject to copyright in the United States (17 U.S.C. §105). The United
> States/U.S. Department of Commerce (“Commerce”) reserve all rights to seek and
> obtain copyright protection in countries other than the United States. The
> United States/Commerce hereby grant to User a royalty-free, nonexclusive
> license to use, copy, and create derivative works of the Software outside of
> the United States.

> The Software is provided to the User and those who may take by, through or
> under it, “as is,” without any warranty (whether express or implied) or
> representation whatsoever, including but not limited to any warranty of
> merchantability. The Software is taken hereunder without any right to support
> or to any improvements, extensions, or modifications, except as may be agreed
> to separately, in writing, by Commerce.

> User, on behalf of itself and all others who take by, through or under it,
> hereby and forever waives, releases, and discharges the United States/Commerce
> and all its instrumentalities from any and all liabilities and obligations in
> connection with the use, application, sale or conveyance of the Software. User
> shall indemnify and hold harmless the United States/Commerce and its
> instrumentalities from all claims, liabilities, demands, damages, expenses,
> and losses arising from or in connection with User's use, application, sale or
> conveyance of the Software, including those who take by, through or under User
> whether or not User was directly involved. This provision will survive
> termination of this Agreement and will include any and all claims or
> liabilities arising under intellectual property rights, such as patents,
> copyrights, trademarks, and trade secrets. If User of software is an Executive
> Agency of the United States, this clause is not applicable.

> The construction, validity, performance, and effect of this Agreement for all
> purposes will be governed by Federal law of the United States.

> User agrees to make a good faith effort to use the Software in a way that does
> not cause damage, harm, or embarrassment to the United States/Commerce. The
> United States/Commerce expressly reserve all rights and remedies.


The R Package was created by Dirk Eddelbuettel, Christoph Sax, Kirill Müller and Michael Antonov and is licensed under GPL (>= 2).
