context("test-utils-sm_predict_raw")

test_that("utils-sm_predict_raw creates output without error/warning: glm", {
  glm_ex <-
    sm_predict_raw(
      method = "glm",
      object = glm(am ~ mpg, mtcars, family = binomial(link = "logit")),
      newdata = mtcars %>% dplyr::as_data_frame() %>% dplyr::filter(dplyr::row_number() == 1),
      type = "response"
    )

  expect_error(glm_ex, NA)
  expect_warning(glm_ex, NA)
})

test_that("utils-sm_predict_raw creates output without error/warning: coxph", {
  library(survival)
  glm_ex <-
    sm_predict_raw(
      method = "coxph",
      object = coxph(Surv(time, status) ~ age, lung),
      newdata = lung %>% dplyr::as_data_frame() %>% dplyr::filter(dplyr::row_number() == 1),
      type = "failure"
    )

  expect_error(glm_ex, NA)
  expect_warning(glm_ex, NA)
})


test_that("utils-sm_predict_raw no error when NULL object passed", {
  glm_null <-
    sm_predict_raw(
      method = "glm",
      object = NULL,
      newdata = mtcars %>% dplyr::as_data_frame() %>% dplyr::filter(dplyr::row_number() == 1),
      type = "response"
    )

  expect_error(glm_null, NA)
  expect_warning(glm_null, NA)
})
