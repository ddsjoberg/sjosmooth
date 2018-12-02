
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/ddsjoberg/sjosmooth.svg?branch=master)](https://travis-ci.org/ddsjoberg/sjosmooth)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/sjosmooth?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/sjosmooth)
[![Coverage
status](https://codecov.io/gh/ddsjoberg/sjosmooth/branch/master/graph/badge.svg)](https://codecov.io/github/ddsjoberg/sjosmooth?branch=master)

# sjosmooth <img src="man/figures/logo.png" align="right" height=140/>

The goal of sjosmooth is to provide kernel smoothed estimates for time
to event data.

## Installation

You can install the released version of sjosmooth from GitHub with:

``` r
remotes::install_github("ddsjoberg/sjosmooth")
```

## Functions

The **sjosmooth** package has two primary functions. The first
calculates kernel-weighted regression models (`sm_regression`). The user
specifies a regression model and a variable for weighting, and
`sm_regression` will estimate a weighted regression model for each
unique value of the specified variable.

The second function (`sm_predict`) calculates kernel-weighted
predictions from regression models (i.e. outcomes that can be calculated
from the `predict` function).
