#' Grab coefficient from models
#'
#' This function is meant to be run after `sm_regression` when the regression model
#' is univariate.  From each of the weighted models, the coefficient will be
#' saved as a column in the output data frame.
#'
#' @param x `sm_regression` object
#' @seealso \code{\link{sm_regression}}
#' @export
add_coef <- function(x) {
  # checking class of input
  if (!"sm_regression" %in% class(x)) {
    stop("x must be of class sm_regression")
  }

  # getting single variable name from RHS of formula
  pred_var <-
    attr(x, "sm_regression_inputs") %>%
    purrr::pluck("formula") %>%
    stats::terms() %>%
    labels()

  # printing error if model has more than one independent variable
  if (length(pred_var) > 1) {
    stop("This function is meant to work with models with a single covariate.")
  }

  # adding coefficient to output data frame
  x <-
    x %>%
    dplyr::mutate_(
      .coef = ~purrr::map_dbl(
        .model,
        ~dplyr::case_when(
          is.null(.x) ~ NA_real_,
          TRUE ~ coef(.x) %>% purrr::pluck(pred_var)
        ))
    )

  return(x)
}


