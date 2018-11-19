#' Illustrate interactions by illustrating changes in beta coefficient(s) over
#' a continuous variable
#'
#' This function illustrates how the relationship between an independent
#' variable and a dependent variable is modified over the values of a third
#' variable.  Using `data`, `method`, and `formula` the regression model is
#' estimated; the estimates are weighted by the third `interaction`
#' variable.
#'
#' @param data data frame
#' @param method (function) to use
#' @param formula formula
#' @param interaction third variable interaction term
#' @param newdata new data frame.  Default is `data`.  Only requires third
#' term from `interaction`
#' @param method.args List of additional arguments passed on to the
#' modelling function defined by `method`
#' @param lambda The radius of the kernel for tri-cubic, Epanechnikov, and flat kernels.
#' The standard deviation for the Gaussian kernel
#' @param exponentiate Default is `FALSE`.  Exponentiate beta coefficients
#' @export
#' @examples
#' sm_beta_interaction(
#'   data = mtcars,
#'   method = "lm",
#'   formula = mpg ~ am ,
#'   interaction = "hp"
#' )

sm_beta_interaction <- function(data, method, formula, interaction, newdata = data,
                                method.args = NULL, lambda = 1, exponentiate = FALSE) {

  # all variables
  all_vars <- c(all.vars(formula), interaction)
  covar <- all.vars(formula)[[2]]

  # converting to tibble
  newdata <-
    newdata[interaction] %>%
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
        function(x) dplyr::as_data_frame(x) %>% purrr::set_names(interaction)
      )
    ) %>%
      dplyr::mutate_(
        # adding scaled points
        point_scaled = ~purrr::map(
          point,
          ~((.x - scaled_mean[interaction]) / scaled_sd[interaction]) %>% dplyr::as_data_frame()
        ),
        # calculating distance vector between point and full data
        distance = ~purrr::map(
          point_scaled,
          ~calculate_dist(data = data_scaled[interaction], point = .x, dist_method = "euclidean")
        ),
        # calculating kernel weights
        weight = ~purrr::map(
          distance,
          ~calculate_weights(
            dist = .x, lambda = lambda, kernel = "epanechnikov",
            interaction = interaction
          )
        ),
        # building model
        model_obj = ~purrr::map(
          weight,
          ~do.call(
            method,
            c(
              method.args,
              list(
                data = data %>% dplyr::filter(.x > 0),
                formula = formula,
                weights = .x[.x > 0]
              )
            )
          )
        ),
        # tidying model
        model_tidy = ~purrr::map(
          model_obj,
          ~broom::tidy(.x, conf.int = TRUE, exponentiate = exponentiate) %>%
            dplyr::filter_(~term == covar)
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
