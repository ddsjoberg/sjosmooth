#' Add bootstrapped confidence intervals
#'
#' This function is meant to be run after `sm_predict` or `sm_regression` and
#' will calculate bootstrapped confidence intervals for `sm_predict` objects
#' and add the bootstraped model estimates for `sm_regression` objects.
#' @param x `sm_predict` or `sm_regression` object
#' @param ... further arguments passed to or from other methods.
#' @seealso \code{\link{add_ci.sm_regression}}
#' @export
add_ci <- function(x, ...) UseMethod("add_ci")

#' Add bootstrapped model estimates
#'
#' This function is meant to be run after `sm_regression` and
#' will calculate bootstraped model estimates.
#' @param x `sm_regression` object
#' @param n number of bootstrap models to run. Default is 200.
#' @param seed a single value, interpreted as an integer. See \code{\link{set.seed}}
#' @param ... further arguments passed to or from other methods.
#' @export
#' @examples
#' sm_regression(
#'   data = mtcars,
#'   method = "lm",
#'   formula = mpg ~ am ,
#'   weighting_var = "hp",
#'   lambda = 1,
#'   newdata = data.frame(hp = c(150, 200))
#' ) %>%
#' add_ci(n = 50)
#'
#' # example plotting slope coefficient with confidence intervals
#' \dontrun{
#' library(ggplot2)
#' mtcars %>%
#'   # calculate locally-weighted regression models
#'   sm_regression(
#'     method = "lm",
#'     formula = mpg ~ am ,
#'     weighting_var = "hp",
#'     newdata = data.frame(hp = seq(125, 175, by = 5))
#'   ) %>%
#'   # add models from bootstrap resamples
#'   add_ci(n = 200, seed = 23948) %>%
#'   # calculating confidence interval for beta coefficient
#'   dplyr::mutate(
#'     # extracting central estimate of beta
#'     .coef = purrr::map_dbl(
#'       .model,
#'       ~ .x %>% coef() %>% purrr::pluck("am")
#'     ),
#'     # extracting each estimate of beta from bootstrapped models
#'     .coef.boot = purrr::map(
#'       .model.boot,
#'       ~purrr::map_dbl(
#'         .x,
#'         ~ .x %>% coef() %>% purrr::pluck("am")
#'       )
#'     ),
#'     # calculating the SD of the beta distribution
#'     .coef.sd = purrr::map_dbl(
#'       .coef.boot,
#'       sd,
#'       na.rm = TRUE
#'     ),
#'     # calculating the confidence interval for beta coef
#'     .coef.ll = .coef - qnorm(0.975) * .coef.sd,
#'     .coef.ul = .coef + qnorm(0.975) * .coef.sd,
#'   ) %>%
#'   ggplot(aes(x = hp, y = .coef)) +
#'     geom_line() +
#'     geom_ribbon(
#'       aes(ymin = .coef.ll, ymax = .coef.ul),
#'       alpha = 0.4
#'     ) +
#'     labs(
#'       y = "Slope coefficient for 'am' when regressed on 'mpg'"
#'     )
#' }

add_ci.sm_regression <- function(x, n = 200, seed = NULL, ...) {

  # setting seed if provided
  if (!is.null(seed)) set.seed(seed)

  # creating resampled datasets
  data_boot <- purrr::map(
    seq(1, n),
    ~attr(x, "sm_regression_inputs") %>%
      purrr::pluck("data") %>%
      {
        dplyr::slice(., sample.int(nrow(.), replace = TRUE))
      }
    )

  # copy of sm_regression inputs without data
  inputs_no_data <- attr(x, "sm_regression_inputs")
  newdata = inputs_no_data[["newdata"]]
  inputs_no_data[["verbose"]] <- TRUE
  inputs_no_data[["data"]] <- NULL
  inputs_no_data[["newdata"]] <- NULL

  # running sm_regression on the bootstrapped datasets
  suppressMessages(
    result_all <-
      purrr::map_dfr(
        1:length(data_boot),
        ~do.call(
          sm_regression,
          c(list(data = data_boot[[.x]], newdata = newdata), inputs_no_data)
        ) %>%
          attr("full_results") %>%
          dplyr::select(c("newdata", ".model")) %>%
          dplyr::mutate_(
            ..id.. = ~1:dplyr::n()
          )
      )
  )

  result_model_nested <-
    result_all %>%
    dplyr::select(c("..id..", ".model")) %>%
    tidyr::nest(".model") %>%
    dplyr::mutate_(
      .model.boot = ~purrr::map(
        data,
        ~ .x %>% dplyr::pull(.model)
        )
    ) %>%
    dplyr::select(c("..id..", ".model.boot"))


  result <-
    result_all %>%
    dplyr::select(c("..id..", "newdata")) %>%
    dplyr::group_by_("..id..") %>%
    dplyr::filter_(~dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      result_model_nested,
      by = "..id.."
    ) %>%
    dplyr::select(c("newdata", ".model.boot")) %>%
    tidyr::unnest_("newdata") %>%
    dplyr::group_by_(names(newdata)) %>%
    dplyr::filter_(~dplyr::row_number() == 1) %>%
    dplyr::ungroup()

  # merging in bootstapped results with central estimate.
  x %>%
    dplyr::left_join(
      result,
      by = names(newdata)
    )

}


