context("test-utils-calculate_weights")

test_that("utils-calculate_weights creates output without error/warning: glm", {
  glm_ex <-
    purrr::map(
      c("epanechnikov", "tricube", "gaussian", "flat"),
      ~calculate_weights(
        dist = calculate_dist(
          data = mtcars,
          point = dplyr::data_frame(mpg = 20),
          dist_method = "euclidean"
        ),
        lambda = 1,
        kernel = .x,
        weighting_var = "mpg"
      )
    )

  expect_error(glm_ex, NA)
  expect_warning(glm_ex, NA)
})

