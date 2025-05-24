[![pipeline status](https://gitlab.com/fvafrcu/packager/badges/master/pipeline.svg)](https://gitlab.com/fvafrcu/packager/-/commits/master)    
[![coverage report](https://gitlab.com/fvafrcu/packager/badges/master/coverage.svg)](https://gitlab.com/fvafrcu/packager/-/commits/master)
<!-- 
    [![Build Status](https://travis-ci.org/fvafrcu/packager.svg?branch=master)](https://travis-ci.org/fvafrcu/packager)
    [![Coverage Status](https://codecov.io/github/fvafrcu/packager/coverage.svg?branch=master)](https://codecov.io/github/fvafrcu/packager?branch=master)
-->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/packager)](https://cran.r-project.org/package=packager)
[![RStudio_downloads_monthly](https://cranlogs.r-pkg.org/badges/packager)](https://cran.r-project.org/package=packager)
[![RStudio_downloads_total](https://cranlogs.r-pkg.org/badges/grand-total/packager)](https://cran.r-project.org/package=packager)

<!-- README.md is generated from README.Rmd. Please edit that file -->



# packager
## Introduction
Please read the
[vignette](https://fvafrcu.gitlab.io/packager/doc/An_Introduction_to_packager.html).
<!-- 
[vignette](https://CRAN.R-project.org/package=packager/vignettes/An_Introduction_to_packager.html).
-->

Or, after installation, the help page:

``` r
help("packager-package", package = "packager")
```


```
#> Helps Me Create, Build and Maintain Packages
#> 
#> Description:
#> 
#>      Helper functions for package creation, building and maintenance,
#>      heavily borrowing from 'devtools' 1.13.3.
#> 
#> Details:
#> 
#>      You will find the details in
#>      'vignette("An_Introduction_to_packager", package = "packager")'.
#> 
#> Author(s):
#> 
#>      *Maintainer*: Andreas Dominik Cullmann
#>      <mailto:fvafrcu@mailbox.org>
#> 
#> See Also:
#> 
#>      Useful links:
#> 
#>         • <https://gitlab.com/fvafrcu/packager>
```

## Installation

You can install packager from gitlab via:


``` r
if (! require("remotes")) install.packages("remotes")
remotes::install_gitlab("fvafrcu/packager")
```


