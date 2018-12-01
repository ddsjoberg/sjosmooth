#' Kernel-weighted regression estimation
#'
#' Provides smoothed estimates from a variety of models, but was built to work
#' primarily with time to event endpoints.
#'
#' @param data data frame
#' @param method function to use
#' @param formula formula
#' @param type type of statistic to smooth (e.g. survival, median survival, etc.)
#' @param newdata new data frame.  Default is `data`. Only requires covarites from
#' the RHS of `~` and the time compenent from the outcome for some survival estimators.
#' @param method.args List of additional arguments passed on to the
#' modelling function defined by `method`
#' @param lambda The radius of the kernel for tri-cubic, Epanechnikov, and flat kernels.
#' The standard deviation for the Gaussian kernel
#' @param verbose Return full set of results. Default is `FALSE`
#' @export

sm_predict <- function(data, method, formula, type, newdata = data,
                       method.args = NULL, lambda = 1,
                       verbose = FALSE) {

  # WEIGHTED REGRESSION MODELS -------------------------------------------------
  wt_models <-
    sm_regression(
      data = data, method = method, formula = formula,
      weighting_var = formula %>% stats::terms() %>% labels(),
      newdata = newdata, method.args = method.args, lambda = lambda
    )

  # PREDICTIONS ----------------------------------------------------------------
  wt_models <-
    wt_models %>%
    dplyr::mutate_(
      predict_safely = ~purrr::map2(
        model_obj, newdata,
        ~sm_predict_raw_safely(
          method = method, object = .x, newdata = .y,
          type = type, conf.level = 0.95
        )
      ),
      # extracting objects, warnings, errors from safely object
      predict_error = ~purrr::map(predict_safely, ~.x[["error"]]),
      predict_warning = ~purrr::map(predict_safely, ~.x[["result"]][["warnings"]]),
      predict_message = ~purrr::map(predict_safely, ~.x[["result"]][["messages"]]),
      # extracting result and storing in vector
      predict_result = ~purrr::map(predict_safely, ~.x[["result"]][["result"]])
    )

  # RETURN ---------------------------------------------------------------------
  # getting names of variables in newname for merging
  names_newdata <- names(wt_models$newdata[1][[1]])

  # extracting newdata points and predictions from results,
  # and merging with full newdata object
  sm_predict <-
    newdata %>%
    dplyr::left_join(
      wt_models %>%
        dplyr::select(c("newdata", "predict_result")) %>%
        tidyr::unnest_(c("newdata", "predict_result")),
      by = names_newdata
    )

  # adding sm_predict
  attr(sm_predict$.fitted, "type") <- type
  if (verbose == TRUE) {
    attr(sm_predict, "wt_models") <- wt_models
  }

  sm_predict
}
