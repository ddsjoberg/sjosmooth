#' Kernel-weighted regression models
#'
#' The user specifies a regression model and a variable for weighting,
#' and `sm_regression`` will estimate a weighted regression model for each
#' unique value of the specified variable.
#'
#' @param data data frame
#' @param method function to use
#' @param formula formula
#' @param weighting_var columns name(s) of variables used to calculate weights
#' @param newdata new data frame.  Default is `data`.
#' @param method.args List of additional arguments passed on to the
#' modelling function defined by `method`
#' @param lambda The radius of the kernel for tri-cubic, Epanechnikov, and flat kernels.
#' The standard deviation for the Gaussian kernel
#' @param kernel Specifies the kernel to be used: `epanechnikov`, `tricube`,
#' `gaussian`, and `flat` are accepted. Default is `epanechnikov`
#' @param dist.method Specifies the distance measure to be used in the kernel.
#' Default is `euclidean`. Distance measures accepted by
#' \code{stats::\link[stats]{dist}} is acceptable.
#' @param verbose Return full set of results as an attribute. Default is `FALSE`
#' @export
#' @examples
#' sm_regression(
#'   data = mtcars,
#'   method = "glm",
#'   formula = am ~ mpg,
#'   weighting_var = "mpg",
#'   method.args = list(family = binomial(link = "logit"))
#' )

sm_regression <- function(data, method, formula, weighting_var, newdata = data,
                          method.args = NULL, lambda = 1, kernel = "epanechnikov",
                          dist.method = "euclidean", verbose = FALSE) {

  # will return call, and all object passed to in fmt_table1 call
  # the object func_inputs is a list of every object passed to the function
  sm_regression_inputs <- as.list(environment())

  # all variables
  all_vars <- c(all.vars(formula), weighting_var) %>% unique()

  # converting to tibble, and only keeping required vars
  newdata_keepvars <- intersect(all_vars, names(newdata))
  newdata <-
    newdata[newdata_keepvars] %>%
    dplyr::as_data_frame() %>%
    stats::na.omit()

  # saving subsets of the data
  data <-
    data[all_vars] %>%
    dplyr::as_data_frame() %>%
    stats::na.omit()

  # scaling third interaction term
  data_scaled <- scale(data)
  scaled_mean <- attr(data_scaled, "scaled:center")
  scaled_sd <- attr(data_scaled, "scaled:scale")
  data_scaled <- data_scaled %>% dplyr::as_data_frame()

  # Creating tibble of results (starting with prepping new data, then building models)
  results_full <-
    # converting newdata into list where each row is a list element
    dplyr::data_frame(
      newdata = apply(
        newdata %>% dplyr::distinct(), 1,
        function(x) t(x) %>% dplyr::as_data_frame() %>% purrr::set_names(newdata_keepvars)
      )
    ) %>%
      dplyr::mutate_(
        # adding scaled newdata points
        newdata_scaled = ~purrr::map(
          newdata,
          ~((.x[weighting_var] - scaled_mean[weighting_var]) / scaled_sd[weighting_var]) %>% dplyr::as_data_frame()
        ),
        # calculating distance vector between point and full data (weighting variables only)
        distance = ~purrr::map(
          newdata_scaled,
          ~calculate_dist(data = data_scaled[weighting_var], point = .x, dist_method = dist.method)
        ),
        # calculating kernel weights
        weight = ~purrr::map(
          distance,
          ~calculate_weights(
            dist = .x, lambda = lambda, kernel = kernel,
            weighting_var = weighting_var
          )
        ),
        # building model, safely
        model_safely = ~purrr::map(
          weight,
          ~do.call_safely(
            what = method,
            args = c(
              method.args,
              list(
                data = data %>% dplyr::filter(.x > 0),
                formula = formula,
                weights = .x[.x > 0]
              )
            )
          )
        ),
        # extracting objects, warnings, errors from model_safely object
        model_error = ~purrr::map(model_safely, ~.x[["error"]]),
        model_warning = ~purrr::map(model_safely, ~.x[["result"]][["warnings"]]),
        model_message = ~purrr::map(model_safely, ~.x[["result"]][["messages"]]),
        .model = ~purrr::map(model_safely, ~.x[["result"]][["result"]])
      )

  # printing errors/warnings/messages from model builds
  if (purrr::map_lgl(results_full$model_message, ~ length(.x) > 0) %>% any()) {
    message_print(results_full, "model_message", paste0("Message in ", method, ":"))
  }
  if (purrr::map_lgl(results_full$model_warning, ~ length(.x) > 0) %>% any()) {
    message_print(results_full, "model_warning", paste0("Warning in ", method, ":"))
  }
  if (purrr::map_lgl(results_full$model_error, ~ !is.null(.x)) %>% any()) {
    message_print(results_full, "model_error", paste0("Error in ", method, ":"))
  }


  # only keeping newdata and model
  results <-
    results_full %>%
    dplyr::select(c("newdata", ".model")) %>%
    tidyr::unnest_("newdata") %>%
    # moving model_obj to the end of data frame
    dplyr::select(-c(".model"), (".model")) %>%
    dplyr::filter_(~purrr::map_lgl(.model, ~!is.null(.x)))

  # returning additional information
  if (verbose == TRUE) {
    attr(results, "full_results") <- results_full
  }
  attr(results, "sm_regression_inputs") <- sm_regression_inputs

  # assigning class
  class(results) <- c("sm_regression", class(results))

  results
}

# sm_regression(
#   data = lung,
#   method = "coxph",
#   formula = Surv(time, status) ~ age,
#   weighting_var = "wt.loss",
#   lambda = 0.5
# )
#
# sm_regression(
#   data = mtcars,
#   method = "lm",
#   formula = mpg ~ am ,
#   weighting_var = "am",
#   lambda = 1
# )
