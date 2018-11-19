#' Kernel-weighted regression modelling
#'
#' General function that fits weighted regerssion models, where the weights
#' are calculated from a ancillary variable(s) or from variable(s) found in the
#' regression model. Using `data`, `method`, and `formula` the regression model is
#' estimated; the estimates are weighted by the variable(s) listed in `weighting_var`.
#'
#' @param data data frame
#' @param method (function) to use
#' @param formula formula
#' @param weighting_var columns name(s) of variables used to calculate weights
#' @param newdata new data frame.  Default is `data`.  Only requires third
#' term from `interaction`
#' @param method.args List of additional arguments passed on to the
#' modelling function defined by `method`
#' @param lambda The radius of the kernel for tri-cubic, Epanechnikov, and flat kernels.
#' The standard deviation for the Gaussian kernel
#' @export
#' @examples
#' sm_wt_regression(
#'   data = mtcars,
#'   method = "lm",
#'   formula = mpg ~ am ,
#'   weighting_var = "hp",
#'   lambda = 2
#' )

sm_wt_regression <- function(data, method, formula, weighting_var, newdata = data,
                                method.args = NULL, lambda = 1) {

  # all variables
  all_vars <- c(all.vars(formula), weighting_var)
  covar <- all.vars(formula)[[2]]

  # converting to tibble
  newdata <-
    newdata[weighting_var] %>%
    dplyr::as_data_frame()

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

  # prepping new data, by scaling interaction term and keeping unique value
  results <-
    # putting points in data frame
    dplyr::data_frame(
      point = apply(
        newdata %>% dplyr::distinct(), 1,
        function(x) dplyr::as_data_frame(x) %>% purrr::set_names(weighting_var)
      )
    ) %>%
      dplyr::mutate_(
        # adding scaled points
        point_scaled = ~purrr::map(
          point,
          ~((.x - scaled_mean[weighting_var]) / scaled_sd[weighting_var]) %>% dplyr::as_data_frame()
        ),
        # calculating distance vector between point and full data
        distance = ~purrr::map(
          point_scaled,
          ~calculate_dist(data = data_scaled[weighting_var], point = .x, dist_method = "euclidean")
        ),
        # calculating kernel weights
        weight = ~purrr::map(
          distance,
          ~calculate_weights(
            dist = .x, lambda = lambda, kernel = "epanechnikov",
            weighting_var = weighting_var
          )
        ),
        # building model
        model_obj = ~purrr::map(
          weight,
          ~do.call(
            method,
            c(method.args,
              list(data = data %>% dplyr::filter(.x > 0),
                   formula = formula,
                   weights = .x[.x > 0])
            )
          )
        )
      )

  return(results)
}

# t =
# sm_beta_interaction(
#   data = mtcars,
#   method = "lm",
#   formula = mpg ~ am ,
#   interaction = "hp"
# )
