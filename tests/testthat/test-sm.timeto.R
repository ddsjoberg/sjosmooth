context("test-sm.coxph.R")

newdata = tibble::tibble(x = seq(0, 1, by = 0.5),
                         time.true = 1)

test_that("Simple case runs without error", {
  expect_error(
    sm.timeto(formula = Surv(time.true) ~ x,
              data = sjosmooth.tbl,
              newdata = newdata),
    NA
    )
})

test_that("Lambda cannot be negative", {
  expect_error(
    sm.timeto(formula = Surv(time.true) ~ x,
              data = sjosmooth.tbl,
              newdata = newdata,
              lambda = -1)
  )
})
