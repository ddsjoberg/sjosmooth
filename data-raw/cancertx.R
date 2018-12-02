# Results from a simulated recur data
library(survival)

set.seed(490345)
n <- 10000
cancertx <-
  dplyr::data_frame(
    age = rnorm(n, 30, 10),
    marker = rnorm(n, 10, 2),
    xb = (1 / 50) * age - 0.2 * marker,
    xb_marker = (1 / 50) * 30 - 0.2 * marker,
    xb_age = (1 / 50) * age - 0.2 * 10,
    time = -log(runif(n, 0, 1)) * exp(-xb),
    # getting the true survival probabilites at time 1 using x
    survt1 = exp(-exp(xb)),
    survt1_marker = exp(-exp(xb_marker)),
    survt1_age = exp(-exp(xb_age))
  )

cancertx <- dplyr::select(cancertx, -xb)

# checking model estimates
survival::coxph(survival::Surv(time) ~ age + marker, data = cancertx)
survival::coxph(survival::Surv(time) ~ marker, data = cancertx)
survival::coxph(survival::Surv(time) ~ age, data = cancertx)

save(cancertx, file = "examples/cancertx.rda")

# saving out example results
library(survival)
sm_regression_ex1 <-
  sjosmooth::sm_regression(
    data = cancertx,
    method = "coxph",
    formula = Surv(time) ~ age,
    weighting_var = "marker",
    newdata = dplyr::data_frame(marker = seq(5, 15, 0.5))
  )
save(sm_regression_ex1, file = "examples/sm_regression_ex1.rda")

sm_predict_ex1 <-
  sjosmooth::sm_predict(
    data = cancertx,
    method = "coxph",
    formula = Surv(time) ~ marker,
    newdata = dplyr::data_frame(marker = seq(5, 15, 0.5), time = 1),
    type = "survival",
    verbose = TRUE
  )
save(sm_predict_ex1, file = "examples/sm_predict_ex1.rda")


sm_predict_ex2 <-
  sjosmooth::sm_predict(
    data = cancertx,
    method = "coxph",
    formula = Surv(time) ~ marker + age,
    newdata =
      list(
        marker = seq(7, 13, 1),
        age = seq(20, 40, 2)
      ) %>%
        purrr::cross_df() %>%
        dplyr::mutate(time = 1),
    type = "survival",
    verbose = TRUE
  )
save(sm_predict_ex2, file = "examples/sm_predict_ex2.rda")
