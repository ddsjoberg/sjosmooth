#' n-dimensional kernel-smoothed survival probabilites.
#'
#' The purpose of this function is to proivde kernel-smoothed n-year survival
#' estimates for n-dimensial data. For example, using two predictors, x and y,
#' predict time to death. The program estimates a hyperplane that estimates
#' risk of death at n-years for every x, y combination.
#'
#' @param formula The formula call used by \code{survival::coxph(formula = Surv(time, dead) ~ x + y)}.
#' Do not include transformed variables in the formula (e.g. \code{Surv(time, dead) ~ log(x) + y})--the specified
#' \code{lambda} will no longer be interpretable.
#' @param data A data.frame or tibble with named columns with the columns specifed in formula.
#' @param pred.time The time at which the function will estimate the survival probabilites.
#' @param lambda The radius of the kernel for tri-cubic and Epanechnikov kernels.
#' The standard deviation for the Gaussian kernel.
#' @param grid A data.frame or tibble with named columns that are the covaraites specified in formula.
#' Survival probabilities will only be estimated at these points if specified.  Otherwise, survival probabilities
#' will be estimated at each unique combination of the observed covaraites.
#' @return A data.frame or tibble with the covariates and the estiamted survival probability.
#'
#' @export
sm.coxph = function(formula, data, pred.time = 1, lambda = 1, grid = NULL){
  # defining the pipe operator for use throughout function
  `%>%` <- magrittr::`%>%`

  #converting formula to string (if not already)
  if(class(formula) == "formula") formula = deparse(formula)


  # extracting variable names from formula
  all.vars = all.vars(as.formula(formula))
  covars = all.vars(as.formula(formula)[[3]])
  outcome = all.vars(as.formula(formula)[[2]])

  # only keeping variables in model
  data = data[all.vars]

  # getting means/sd for scaling
  data.scale = scale(data[covars])
    means = attributes(data.scale)$`scaled:center`
    sds = attributes(data.scale)$`scaled:scale`
    # adding the outcome variable (all analyses will be done on the scaled data)
    data.scale = dplyr::bind_cols(data[outcome], tibble::as.tibble(data.scale))

  # which points to calculate the weighted estimates for
  # default is observed obbservations
  if(is.null(grid) == TRUE){
    results =
      data.scale %>%
      # keeping the ID and covariates
      dplyr::select_(.dots = covars) %>%
      #keeping unique vars combinations only
      dplyr::distinct(.)
  }
  # if grid of points was provided in function call
  if(is.null(grid) == FALSE){
    results =
      grid %>%
      # keeping covariates (just in case user provided additional data)
      dplyr::select_(.dots = covars) %>%
      #keeping unique vars combinations only
      dplyr::distinct(.)

    #scaling data to mean 0 and sd 1
    results = (results - means)/sds
  }

  # computing weighted results
  results =
    results %>%
    dplyr::mutate(.id. = 1:nrow(.),
           !!outcome[1] := pred.time) %>%
    dplyr::group_by(.id.) %>%
    dplyr::do(
      #storing prediction set as tbl0, and original data as tbl
      tbl0 = .[c(covars, outcome[1])] ,
      tbl = data.scale
      ) %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(
       # calculating Euclidean distance between observation point (in tbl0), and actual data (in tbl)
       dist = purrr::map2(tbl0, tbl,
                       ~ as.matrix(dist(dplyr::bind_rows(.x, .y)))[-1,1]),
      # calculating weights, and adding to tbl
      tbl = purrr::map2(tbl, dist,
                 ~ dplyr::bind_cols(.x, tibble::tibble(dist = .y,
                                        t = dist / lambda,
                                        K = ifelse(t <= 1, (1 - t^3)^3, 0)))),
      pred = exp(-purrr::map2_dbl(tbl, tbl0,
                      ~ predict(survival::coxph(formula = as.formula(formula), data = .x),
                                newdata = .y,
                                type = "expected")))
     ) %>%
    dplyr::select(tbl0, pred) %>%
    tidyr::unnest(tbl0) %>%
    dplyr::select_(.dots = c(covars,"pred"))

  #scaling covariates data back to original scale
  results[covars] = results[covars]*sds + means

  return(results)
}


