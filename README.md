
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/ddsjoberg/sjosmooth.svg?branch=master)](https://travis-ci.org/ddsjoberg/sjosmooth)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/sjosmooth?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/sjosmooth)
[![Coverage
status](https://codecov.io/gh/ddsjoberg/sjosmooth/branch/master/graph/badge.svg)](https://codecov.io/github/ddsjoberg/sjosmooth?branch=master)

# sjosmooth

The goal of sjosmooth is to provide kernel smoothed estimates for time
to event data.

## Installation

You can install the released version of sjosmooth from GitHub with:

``` r
remotes::install_github("ddsjoberg/sjosmooth")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sjosmooth)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)

sm_regression(
 data = mtcars,
 method = "lm",
 formula = mpg ~ am,
 weighting_var = "hp",
 lambda = 2
) %>% 
  mutate(
     # tidying model
        model_tidy = map(
          model_obj,
          ~broom::tidy(.x, conf.int = TRUE) %>%
            dplyr::filter_(~term == "am")
        )
  ) %>%
  select(newdata, model_tidy) %>% 
  unnest(newdata) %>% 
  unnest(model_tidy) %>% 
  ggplot(aes(x = hp, y = estimate)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4) +
  geom_hline(
    yintercept = lm(mpg ~ am, mtcars) %>% coef() %>% purrr::pluck(2),
    linetype = "dashed"
    ) +
  labs(
    y = "Slope Coefficient for 'am' regressed on 'mpg'"
  )
```

<img src="man/figures/README-example-1.png" width="100%" />
