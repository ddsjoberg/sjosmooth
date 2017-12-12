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
#' @param grid A data.frame or tibble with named columns that are the covaraites specified in formula.
#' Survival probabilities will only be estimated at these points if specified.  Otherwise, survival probabilities
#' will be estimated at each unique combination of the observed covaraites.
#' @param type Specifes the type of statistic that will be calculated.  Default is \code{survival}.
#' @param lambda The radius of the kernel for tri-cubic and Epanechnikov kernels.
#' The standard deviation for the Gaussian kernel.
#' @param kernel Specifies the kernel to be used: \code{epanechnikov}, \code{tricube}, \code{gaussian}, and \code{knn} are accepted.
#' Default is \code{epanechnikov}.  See vignettes for differences in methods.
#' @param dist.method Specifies the distance measure to be used in the kernel.  Default is \code{euclidean}.
#' Any distance measure accepted by \code{stats::dist} is acceptable.
#' @param scale Default is TRUE. If TRUE, the covariates are scaled to mean 0 and standard deviation 1 prior to kernel smoothing.
#' If includeing 2 or more covariates, scale should be set to \code{TRUE}.
#' @param details Default is FALSE. If TRUE, the survival::coxph object will be returned along the the predictions.
#' @return A data.frame or tibble with the covariates and the estiamted survival probability.
#' @importFrom survival coxph
#' @importFrom survival Surv
#' @export
sm.coxph = function(formula, data, pred.time = 1, grid = NULL,
                    type = "survival", lambda = 1, kernel = "epanechnikov",
                    dist.method = "euclidean", knn = NULL, scale = FALSE, details = FALSE){

  #converting formula to string (if not already)
  if(class(formula) == "formula") formula = deparse(formula)

  # extracting variable names from formula
  all.vars = all.vars(stats::as.formula(formula))
  covars = all.vars(stats::as.formula(formula)[[3]])
  outcome = all.vars(stats::as.formula(formula)[[2]])

  # only keeping variables in model
  data = data[all.vars]

  # Scaling covariates to mean 0 and SD 1 if requested
  if (scale == TRUE){
    data.scaled = scale(data[covars])
      #saving out mean and SD vectors
      means = attributes(data.scaled)$`scaled:center`
      sds = attributes(data.scaled)$`scaled:scale`

      # adding the outcome variable (all analyses will be done on the scaled data)
      data = cbind(data[outcome], data.scaled)
  }

  # which points to calculate the weighted estimates for
    if(is.null(grid) == TRUE){ # default is observed observations
      # keeping unique combinations of covars
      grid = unique(data[covars])
    } else if(is.null(grid) == FALSE){ # if grid of points was provided in function call
      # keeping unique combinations of covars
      grid = unique(grid[covars])

      #scaling data to mean 0 and sd 1
      if (scale == TRUE) grid = (grid - means)/sds
    }

  # computing weighted results
  results =
    tibble::tibble(
      # extracting each row from grid and saving as it's own data.frame
      tbl0 = apply(grid, 1, function(x) tibble::as.tibble(t(x))),
      # saving a copy of the data for each row in the grid (will calculate different weights for each of these sets)
      tbl  = rep(list(data), nrow(grid))
    ) %>%
    dplyr::mutate(
      #calculating the kernel
      tbl = sjosmooth.kernel(tbl0, tbl, kernel, dist.method, lambda, covars),

      # building cox models, returning NA if error
      coxph = purrr::map(tbl, ~ tryCatch(survival::coxph(formula = stats::as.formula(formula),
                                                         data = .x,
                                                         weights = K),
                                         error=function(e) NA)),

      # calculating predictions on estimating point (tbl0)
      pred = prediction.type(type, coxph, tbl0, pred.time, outcome)
     ) %>%
    dplyr::select(tbl0, pred, coxph, tbl) %>%
    tidyr::unnest(tbl0)
  print("Print tbl (with weights calculated)")
  print(results$tbl[1][[1]])
  print("Print cox model")
  print(results$coxph[1][[1]])

    #dropping cox model unless details == TRUE
    if (details == F) {
      results = results %>%
        dplyr::select_(.dots = c(covars,"pred"))
    } else if (details == T) {
      results = results %>%
        dplyr::select_(.dots = c(covars,"pred", "tbl", "coxph"))
    }

  #scaling covariates data back to original scale
  if (scale == TRUE) results[covars] = results[covars]*sds + means

  return(results)
}

# this simple function adds a variable to a tibble or data frame
# most useful for adding columns to tibbles within a list when the variable name is a string
# for example,
# tbl0 = purrr::map(tbl0, ~ add.var(data = .x, varname = outcome[1], varvalue = pred.time))
add.var = function(data , varname, varvalue){
  data[[varname]] <- with(data, varvalue)
  return(data)
}


# this function calculates the kernels
sjosmooth.kernel = function(tbl0, tbl, kernel, dist.method, lambda, covars, knn) {
  # calculating distance between estimation point (in tbl0), and actual data (in tbl)
  dist = purrr::map2(tbl0, tbl,
                     ~ as.matrix(dist(dplyr::bind_rows(.x, .y)[covars], method = dist.method))[-1,1])

  # calculating weights, and adding to tbl
  if (kernel == "epanechnikov") {
    K = purrr::map(dist, ~ ifelse((.x / lambda) <= 1, 0.75*(1 - (.x / lambda)^2), 0))
  } else if (kernel == "tricube") {
    K = purrr::map(dist, ~ ifelse((.x / lambda) <= 1, (1 - (.x / lambda)^3)^3, 0))
  } else if (kernel == "gaussian") {
    K = purrr::map(dist, ~ sqrt(2*pi*lamda^2)^-length(covars) * exp(-0.5/lambda^2 * .x^2))
  } else if (kernel == "knn") {
    ### PUT ERROR if is.null(knn)==TRUE !
    K = purrr::map(dist, ~ ifelse(rank(.x, ties.method = "min") <= knn, 1, 0))
  } else {
    ##  PUT ERROR for not selecting appropriate kernel
  }

  #attaching K to the tbl, deleting observations with zero weight
  tbl = purrr::map2(tbl, K, ~ dplyr::bind_cols(.x, tibble::tibble(K = .y)) %>% dplyr::filter(K>0) )

  return(tbl)
}

#this function calculates various types of predictions
prediction.type = function(type, coxph, tbl0, pred.time, outcome){

  # calculating survival probabilites
  if (type == "survival"){
    # adding pred.time to tbl0 object
    tbl0 = purrr::map(tbl0, ~ add.var(data = .x, varname = outcome[1], varvalue = pred.time))

    # calculating the survival probability
    pred = exp(-purrr::map2_dbl(coxph, tbl0,
                                ~ tryCatch(stats::predict(object = .x,
                                                          newdata = .y,
                                                          type = "expected"),
                                           error=function(e) NA)))
  }

  return(pred)
}
