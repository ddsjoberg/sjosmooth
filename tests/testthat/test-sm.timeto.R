context("test-sm.coxph.R")

test_that("test 1: input tibble, univariate", {
  sm.timeto(formula = Surv(time.true) ~ x, data = sjosmooth.tbl, scale = F, details = T)
})

test_that("test 2: input tibble, bivariate", {
  sm.timeto(formula = Surv(time.true) ~ x + y, data = sjosmooth.tbl)
})

test_that("test 3: input data.frame, univariate", {
  sm.timeto(formula = Surv(time.true) ~ x, data = sjosmooth.df)
})

test_that("test 4: input data.frame, bivariate", {
  sm.timeto(formula = Surv(time.true) ~ x + y, data = sjosmooth.df)
})

test_that("test 5: input tibble, univariate, character formula", {
  sm.timeto(formula = "Surv(time.true) ~ x", data = sjosmooth.tbl)
})
