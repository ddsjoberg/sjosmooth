# Results from a simulated recur data

set.seed(8976)
n <- 2000
cancertx <-
  dplyr::data_frame(
    age = rnorm(n, 50, 10),
    marker = rnorm(n, 10, 2),
    xb = -2 + (1/50) * age + 0.2 * marker,
    time = -log(runif(n, 0, 1)) * exp(-xb),
    # getting the true survival probabilites at time 1 using x
    survt1 = exp(-exp(xb))
  ) %>%
  dplyr::select(-xb)

# checking model estimates
coxph(Surv(time) ~ age + marker, data = cancertx)
coxph(Surv(time) ~ marker, data = cancertx)
coxph(Surv(time) ~ age, data = cancertx)

usethis::use_data(cancertx, overwrite = TRUE)

