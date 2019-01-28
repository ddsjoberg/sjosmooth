#' Grab coefficient from models
#'
#' This function is meant to be run after `sm_regression` when the regression model
#' is univariate.  From each of the weighted models, the coefficient will be
#' saved as a column in the output data frame.
#'
#' @param x `sm_regression` object
#' @seealso \code{\link{sm_regression}}
#' @export
#' @examples
#' sm_regression(
#'   data = mtcars,
#'   method = "lm",
#'   formula = mpg ~ am ,
#'   weighting_var = "hp",
#'   newdata = data.frame(hp = c(150, 200))
#' ) %>%
#' add_coef()
add_coef <- function(x) {
  # checking class of input
  if (!"sm_regression" %in% class(x)) {
    stop("x must be of class sm_regression")
  }

  # saving attributes (expect those associated with tibble properties)
  attr <- attributes(x)
  attr <- attr[names(attr) %>% setdiff(c("row.names", "class", "names"))]

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

  attributes(x) <- c(attributes(x), attr)
  class(x) <- c("sm_regression", class(x))
  return(x)
}


