#' Kernel-weighted predictions from regression models
#'
#' Calculates kernel-weighted predictions from regression models
#' (i.e. outcomes that can be calculated from the predict function).
#'
#' @param data data frame
#' @param method function to use
#' @param formula formula
#' @param type type of statistic to smooth (e.g. survival, median survival, etc.)
#' @param newdata new data frame.  Default is `data`. Only requires covariates from
#' the RHS of `~` and the time component from the outcome for some survival estimators.
#' @param method.args List of additional arguments passed on to the
#' modelling function defined by `method`
#' @param lambda The radius of the kernel for tri-cubic, Epanechnikov, and flat kernels.
#' The standard deviation for the Gaussian kernel
#' @param kernel Specifies the kernel to be used: `epanechnikov`, `tricube`,
#' `gaussian`, and `flat` are accepted. Default is `epanechnikov`
#' @param dist.method Specifies the distance measure to be used in the kernel.
#' Default is `euclidean`. Distance measures accepted by
#' @param verbose Return full set of results as an attribute. Default is `FALSE`
#' @export
#' @examples
#' sm_predict(
#'   mtcars,
#'   method = "glm",
#'   formula = am ~ mpg,
#'   method.args = list(family = binomial(link = "logit")),
#'   type = "response"
#' )

sm_predict <- function(data, method, formula, type, newdata = data,
                       method.args = NULL, lambda = 1, kernel = "epanechnikov",
                       dist.method = "euclidean", verbose = FALSE) {

  # WEIGHTED REGRESSION MODELS -------------------------------------------------
  wt_models <-
    sm_regression(
      data = data, method = method, formula = formula,
      weighting_var = all.vars(formula[-2]), # vars on RHS of ~
      newdata = newdata, method.args = method.args,
      kernel = kernel, dist.method = dist.method, lambda = lambda,
      verbose = TRUE
    ) %>%
    attr(which = "wt_models") # returning full set of results

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

  # adding sm_predict attributes
  attr(sm_predict$.fitted, "type") <- type
  if (verbose == TRUE) {
    attr(sm_predict, "wt_models") <- wt_models
  }

  sm_predict
}
