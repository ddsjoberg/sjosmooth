#' Wrapper for `predict()` with custom coding for some types of predictions.
#'
#' @param method type of regression model
#' @param object the results of a regression fit
#' @param newdata New data at which to do predictions
#' @param type the type of predicted value
#' @keywords internal

sm_predict <- function(method, object, newdata, type) {
  # if model object is NULL, return NULL
  if (is.null(object)) return(NA_real_)

  if (method == "coxph" & type == "survival") {
    stats::predict(object = object, newdata = newdata, type = "expected") %>%
    {exp(-.)} %>%
      return()
  }
  else {
    stats::predict(object = object, newdata = newdata, type = type) %>%
      return()
  }
}

#' @keywords internal
#' @rdname sm_predict
# safe version
sm_predict_safely <-
  sm_predict %>%
  purrr::quietly() %>%
  purrr::safely()
