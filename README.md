
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deidentify <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/jacobkap/deidentify.svg?branch=master)](https://travis-ci.org/jacobkap/deidentify)
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

# De-identify private data

The goal of `deidentify` is to automate some of the work of
de-identifying private data for releasing that data to the public. This
involves determining how many people are in each unique group (the “k-score”) which determines how easy it is to figure out who someone is.
Basically, if someone meets a criteria that is rare in your data
(e.g. combination of some columns such as a White female, who lives in
X city, and is aged 16) then it could be easier to figure out who it is.
The `deidentify_data()` function helps to de-identify data by
aggregating values that are rare, encrypting strings, and aggregating
dates in larger groupings (e.g. months, years).

## Installation

Currently, `deidentify` is only available on
[GitHub](https://github.com/). To install the dev version then, you can
use the following

``` r
# install.packages("devtools")
devtools::install_github("jacobkap/deidentify")
```

## Why de-identify data?

For people working with private data (such as data about crime victims,
or data that includes people’s social security numbers or home
addresses) that they would like to release publicly, there are a number
of steps they must take first to ensure people’s privacy. While some of
these steps must be done manually (such as deleting a column with social
security numbers which, even if encrypted, should never be made public),
there are a number of steps that can be automated - which is the purpose
of this package.

Beyond deleting identifying info (e.g. people’s names), the main way to
de-identify private data is to reduce the number of unique individuals
in the data. Think about a dataset showing crime victims where each row
is a crime victim and each column is info about the victim (race, age,
gender, crime type, date of the crime). If you are looking at a common
crime like theft, then there are probably many people who share all of
these characteristics in the data. For example, say there are 50 people
who were victims of theft of New Years Day 2020, are White women, and
are age 25. It’s hard to pick out any individual from the data (and see
other columns which may have information you don’t know). Now, if only a
single person in your dataset has that mix of characteristics, then you
can probably pick out who it is, making their private data no longer
private. So to make this data ready for release publicly, you need to
ensure that any combinations of variables lead to few unique people in
each group.

### Balancing research ability with privacy concerns

When de-identifying private data, you need to consider the balance
between having too many bins in each column (e.g. having age be how old
you are in years) to too few bins (e.g. group age into units of 30 years
each such as 0-30, 31-60, 61+). Essentially, this is the balance between
having enormous groups with a lot of people in them, which is extremely
hard to de-identify but is not very useful, or having small groups with
few people in each, which is easier to de-identify but much more useful.
As researchers, we built some tools in this package to allow you to set
the balance you want and the package will tell you what decisions to
make (literally what parameters in the `deidentify_data()` function to
set) to get data de-identified in a way that meets this balance.

While the above should give you a starting sense on how to use
`deidentify`, there are more features which can be used. These are
covered more in depth in the vignettes for the package.
