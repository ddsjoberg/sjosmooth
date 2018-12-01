# Results from a simulated recur data

set.seed(490345)
n <- 10000
cancertx <-
  dplyr::data_frame(
    age = rnorm(n, 30, 10),
    marker = rnorm(n, 10, 2),
    xb = (1/50) * age - 0.2 * marker,
    time = -log(runif(n, 0, 1)) * exp(-xb),
    # getting the true survival probabilites at time 1 using x
    survt1 = exp(-exp(xb))
  )

cancertx <- dplyr::select(cancertx, -xb)

# checking model estimates
survival::coxph(survival::Surv(time) ~ age + marker, data = cancertx)
survival::coxph(survival::Surv(time) ~ marker, data = cancertx)
survival::coxph(survival::Surv(time) ~ age, data = cancertx)

save(cancertx, file = "examples/cancertx.rda")

# saving out example results
library(survival)
sm_regression_ex1 =
  sjosmooth::sm_regression(
    data = cancertx,
    method = "coxph",
    formula = Surv(time) ~ age,
    weighting_var = "marker",
    newdata = dplyr::data_frame(marker = seq(7, 12, 0.5))
  )
save(sm_regression_ex1, file = "examples/sm_regression_ex1.rda")
