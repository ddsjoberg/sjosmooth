context("test-add_coef")

test_that("add_coef creates output without error/warning: lm", {
  lm_ex <-
    sm_regression(
      data = mtcars,
      method = "lm",
      formula = mpg ~ am ,
      weighting_var = "hp",
      lambda = 1,
      newdata = data.frame(hp = c(150, 200))
    ) %>%
    add_coef()

  expect_error(lm_ex, NA)
  expect_warning(lm_ex, NA)
})
