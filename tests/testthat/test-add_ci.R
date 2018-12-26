context("test-add_ci")

test_that("add_ci.sm_regression creates output without error/warning: lm", {
  lm_ex <-
    sm_regression(
      data = mtcars,
      method = "lm",
      formula = mpg ~ am ,
      weighting_var = "hp",
      lambda = 1,
      newdata = data.frame(hp = c(150, 200))
    ) %>%
    add_ci(n = 2)

  expect_error(lm_ex, NA)
  expect_warning(lm_ex, NA)
})
