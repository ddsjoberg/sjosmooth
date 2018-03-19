context("test-sm.coxph.R")

newdata = tibble::tibble(x = seq(0.1, 0.9, length.out = 3),
                         time.true = 1)


test_that("Simple case runs without error, testing survival, failure, and expected type", {
  expect_error(
    purrr::map(c("survival", "failure", "expected"),
               ~ sm.regress(formula = Surv(time.true) ~ x,
                           data = sjosmooth.tbl,
                           newdata = newdata,
                           type = .x)
               )
    , NA
  )
})


test_that("Simple case runs without error, testing epanechnikov, tricube, and gaussian kernels ", {
  expect_error(
    purrr::map(
      c("epanechnikov", "tricube", "gaussian", "flat"),
      ~ sm.regress(formula = Surv(time.true) ~ x,
                  data = sjosmooth.tbl,
                  newdata = newdata,
                  kernel = .x)
    ),
    NA
  )
})

test_that("Simple case runs without error on scaled data", {
  expect_error(
    sm.regress(formula = Surv(time.true) ~ x,
              data = sjosmooth.tbl,
              newdata = newdata,
              verbose = TRUE)
    , NA
  )
})

test_that("Simple case runs without error, with verbose output", {
  expect_error(
    sm.regress(formula = Surv(time.true) ~ x,
              data = sjosmooth.tbl,
              newdata = newdata,
              verbose = TRUE)
    , NA
  )
})



test_that("Lambda cannot be negative", {
  expect_error(
    sm.regress(formula = Surv(time.true) ~ x,
              data = sjosmooth.tbl,
              newdata = newdata,
              lambda = -1)
  )
})

test_that("input a variable not in dataset", {
  expect_error(
    sm.regress(formula = Surv(notime) ~ x,
              data = sjosmooth.tbl,
              newdata = newdata.negtime)
  )
})

test_that("All dist.methods function properly", {
  expect_error(
    purrr::map(c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
               ~ sm.regress(formula = Surv(time.true) ~ x,
                           data = sjosmooth.tbl,
                           newdata = newdata,
                           dist.method = .x))
    , NA
  )
})



# test_that("Median survival smoothing", {
#   expect_error(
#     sm.regress(formula = Surv(time.true) ~ x,
#               data = sjosmooth.tbl,
#               newdata = newdata,
#               kernel = "knn", knn = 100)
#     , NA
#   )
# })
