context("test-sm_regression")

test_that("sm_regression creates output without error/warning: glm", {
  glm_ex <-
    sm_regression(
      data = mtcars,
      method = "glm",
      formula = am ~ mpg,
      weighting_var = "mpg",
      method.args = list(family = binomial(link = "logit"))
    )

  expect_error(glm_ex, NA)
  expect_warning(glm_ex, NA)
})

test_that("sm_regression creates output without error/warning: coxph", {
  library(survival)
  coxph_ex <-
    sm_regression(
      data = lung,
      method = "coxph",
      formula = Surv(time, status) ~ age,
      weighting_var = "meal.cal"
    )

  expect_error(coxph_ex, NA)
  expect_warning(coxph_ex, NA)
})

test_that("sm_regression with each kernel", {
  coxph_kernel <-
    purrr::map(
      c("epanechnikov", "tricube", "gaussian", "flat"),
      ~sm_regression(
        data = lung,
        method = "coxph",
        formula = Surv(time, status) ~ age,
        weighting_var = "meal.cal",
        kernel = .x
      )
    )

  expect_error(coxph_kernel, NA)
  expect_warning(coxph_kernel, NA)
})
