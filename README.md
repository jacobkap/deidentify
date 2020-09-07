
<!-- README.md is generated from README.Rmd. Please edit that file -->

# De-identify private data

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/jacobkap/deidentify.svg?branch=master)](https://travis-ci.com/jacobkap/deidentify)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jacobkap/deidentify?branch=master&svg=true)](https://ci.appveyor.com/project/jacobkap/deidentify)
[![CRAN
status](https://www.r-pkg.org/badges/version/deidentify)](https://CRAN.R-project.org/package=deidentify)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/jacobkap/deidentify/branch/master/graph/badge.svg)](https://codecov.io/gh/jacobkap/deidentify?branch=master)
[![](http://cranlogs.r-pkg.org/badges/grand-total/deidentify?color=blue)](https://cran.r-project.org/package=deidentify)
<!-- badges: end -->

The goal of `deidentify` is to automate some of the work of
de-identifying private data for releasing that data to the public. This
involves determining how many people are in each unique group (the “case
score”) which determines how easy it is to figure out who someone is.
Basically, if someone meets a criteria that is rare in your data
(e.g. combination of some columns such as a White female, who lives in
X city, and is aged 16) then it could be easier to figure out who it is.
The `deidentify_data()` function helps to de-identify data by
aggregating values that are rare, encrypting strings (though the
`seed_cipher()` function in the `caesar` package), and aggregating dates
in larger groupings (e.g. months, years)

## Installation

You can install the released version of `deidentify` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("deidentify")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jacobkap/deidentify")
```

## Example

``` r
library(deidentify)
## basic example code
```
