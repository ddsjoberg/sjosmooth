#' Wrapper for `predict()` with custom coding for some types of predictions.
#'
#' @param method type of regression model
#' @param object the results of a regression fit
#' @param newdata New data at which to do predictions
#' @param type the type of predicted value
#' @param conf.level The confidence level to use for the confidence interval.
#' Must be strictly greater than 0 and less than 1. Default is 0.95.
#' @keywords internal

sm_predict_raw <- function(method, object, newdata, type, conf.level = 0.95) {
  # if model object is NULL, return empty data frame
  if (is.null(object))
    return(
      dplyr::data_frame(
        .fitted = NA_real_,
        .se.fit = NA_real_,
        .fitted.ll = NA_real_,
        .fitted.ul = NA_real_
      )
    )

  # for these outcomes, first must calculate expected, then transform
  type2 = type
  if (method == "coxph" & type %in% c("survival", "failure")) {
    type = "expected"
  }


  # first calculate predictions for all model types
  prediction <-
    stats::predict(
      object = object, newdata = newdata, type = type, se.fit = TRUE
    )

  # checking the returned object is what I expect
  if (!setequal(names(prediction)[1:2], c("fit", "se.fit"))) {
    stop('Expecting an object with names c("fit", "se.fit") from predict() function.')
  }

  # converting list to tibble, and calculating CI
  prediction <-
    prediction[c("fit", "se.fit")] %>%
    dplyr::as_data_frame() %>%
    purrr::set_names(c(".fitted", ".se.fit")) %>%
    dplyr::mutate_(
      .fitted.ll = ~.fitted + abs(qnorm((1 - conf.level)/2)) * .se.fit,
      .fitted.ul = ~.fitted - abs(qnorm((1 - conf.level)/2)) * .se.fit
    )

  # transforming coxph survival and failure
  if (method == "coxph" & type2 %in% c("survival", "failure")) {
    prediction <-
      prediction %>%
      dplyr::mutate_at(
        .vars = c(".fitted", ".fitted.ll", ".fitted.ul"),
        .funs = dplyr::funs(exp(-.))
      )

    # if type failure, then 1 minus survival prob
    if (type == "failure") {
      prediction <-
        prediction %>%
        dplyr::mutate_at(
          .vars = c(".fitted", ".fitted.ll", ".fitted.ul"),
          .funs = dplyr::funs(1 - .)
        )
      }
  }


  # returning results
  prediction
}

#' @keywords internal
#' @rdname sm_predict_raw
# safe version
# returns a list with two elements list(result, error).  THe first is the result, and the
# second the error message (if there is one). The first result is a list of the
# results list(result, warnings, messages)
# final output looks like this
# list(result = list(result, warnings, messages), error)
sm_predict_raw_safely <-
  sm_predict_raw %>%
  purrr::quietly() %>%
  purrr::safely()
