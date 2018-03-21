
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sjosmooth <img src="man/figures/logo-small.png" align="right" />

<!-- 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sjosmooth)](https://cran.r-project.org/package=sjosmooth)

[![Coverage Status](https://img.shields.io/codecov/c/github/ddsjoberg/sjosmooth/master.svg)](https://codecov.io/github/ddsjoberg/sjosmooth?branch=master)
-->

[![Build
Status](https://travis-ci.org/ddsjoberg/sjosmooth.svg?branch=master)](https://travis-ci.org/ddsjoberg/sjosmooth)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/sjosmooth?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/sjosmooth)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ddsjoberg/sjosmooth/master.svg)](https://codecov.io/github/ddsjoberg/sjosmooth?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sjosmooth)](https://cran.r-project.org/package=sjosmooth)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/master)

The `sjosmooth` package (pronounced sō smüt͟h) was built to perform
kernel smoothing on any type of regression model, but the focus is on
censored time-to-event or survival data. The package provides kernel
smoothed estimates of survival probabilities at specified times, as well
as other outcomes from survival regression models. Time to event
analysis is often referred to as survival analysis in medicine,
reliability analysis in engineering, duration analysis or duration
modelling in economics, and event history analysis in sociology.

The `sjosmooth` syntax is simple and modeled after existing functions.
If you are famililar the syntax for Cox regression (`survival::coxph`)
and prediction (`survival::predict.coxph`), then you’re already familiar
with `sjosmooth` syntax.

## Installation

You can install sjosmooth from github with:

``` r
# install.packages("devtools")
devtools::install_github("ddsjoberg/sjosmooth")
```

## Example

We know that age is associated with survival after cancer diagnosis.
Using data from patients with advanced lung cancer from the North
Central Cancer Treatment Group, we’ll estimate one year survival
probability by age at diagnosis.

``` r
# load survival and sjosmooth packages
library(survival)
library(sjosmooth)

# save local copy of lung cancer data and order data
cancer.df = lung[order(lung$age), ]

# estimating survival 365 days after diagnosis
cancer.df$yr1surv = sm.coxph(formula = Surv(time, status) ~ age, data = cancer.df, 
                             type = "survival", time = 365, bandwidth = 0.9)

# plotting estimated 1 year survival probability by age
plot(cancer.df$age, cancer.df$yr1surv, type = "l")
```

<img src="man/figures/README-example-1.png" width="50%" />
