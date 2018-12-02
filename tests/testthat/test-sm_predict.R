context("test-sm_predict")

test_that("sm_predict creates output without error/warning: glm", {
  glm_ex <-
    sm_predict(
      data = mtcars,
      method = "glm",
      formula = am ~ mpg,
      method.args = list(family = binomial(link = "logit")),
      type = "response"
    )

  expect_error(glm_ex, NA)
  expect_warning(glm_ex, NA)
})

test_that("sm_predict creates output without error/warning: coxph", {
  library(survival)
  coxph_ex <-
    sm_predict(
      data = lung,
      method = "coxph",
      formula = Surv(time, status) ~ age,
      type = "survival"
    )

  expect_error(coxph_ex, NA)
  expect_warning(coxph_ex, NA)
})
