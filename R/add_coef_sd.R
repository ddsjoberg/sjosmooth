#' Estimate the SD for model coefficients
#'
#' This function is meant to be run after `sm_regression() %>% add_ci()`
#' when the regression model is univariate.  From each of the weighted models,
#' the coefficient standard deviation will be estimated and
#' saved as a column in the output data frame.
#'
#' @param x `sm_regression` object
#' is `FALSE`
#' @seealso \code{\link{sm_regression}}
#' @export
#' @importFrom purrr %||%
#' @examples
#' sm_regression(
#'   data = mtcars,
#'   method = "lm",
#'   formula = mpg ~ am ,
#'   weighting_var = "hp",
#'   newdata = data.frame(hp = c(150, 200))
#' ) %>%
#' add_ci(n = 10) %>%
#' add_coef_sd()
add_coef_sd <- function(x) {
  # checking class of input
  if (!"sm_regression" %in% class(x)) {
    stop("x must be of class sm_regression")
  }

  # saving attributes (expect those associated with tibble properties)
  attr <- attributes(x)
  attr <- attr[names(attr) %>% setdiff(c("row.names", "class", "names"))]

  # checking the bootstrap datasets exist in data frame
  if (!".model.boot" %in% names(x)) {
    stop("Column '.model.boot' not found in data frame. You must run add_ci() before add_coef_sd().")
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

  x <-
    x %>%
    dplyr::mutate_(
      # extracting coefficient from each model to a list
      .coef.list =
        ~purrr::map(
          .model.boot,
          ~purrr::map_dbl(
            .x,
            ~ .x %>%
            stats::coefficients() %>%
            purrr::pluck(pred_var) %||% NA_real_
          )
        ),
      # calculating the SD of the coefs
      .coef.sd =
        ~purrr::map_dbl(
          .coef.list,
          stats::sd,
          na.rm =TRUE
        )
    )

  attributes(x) <- c(attributes(x), attr)
  class(x) <- c("sm_regression", class(x))
  return(x)
}


