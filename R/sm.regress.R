#' Univariate and multivariable kernel smoothing.
#'
#' The purpose of this function is to provide kernel-smoothed
#' estimates for n-dimensional data. For example, using two predictors, x and y,
#' to predict an outcome. The program provides an estimated hyperplane of the outcome by x and y.
#'
#' @param formula The formula call for the model. For exampe, \code{survival::coxph(formula = Surv(time, dead) ~ x + y)}.
#' Do not include transformed variables in the formula (e.g. \code{Surv(time, dead) ~ log(x) + y})--the specified
#' \code{lambda} will no longer be interpretable.
#' @param data A data.frame or tibble with named columns.
#' @param newdata A data.frame or tibble with named columns.
#' Predictions will only be estimated at these points, if specified.
#' @param type Specifies the type of statistic that will be calculated.  For example, from a Cox model, you may request \code{survival} or \code{expected}.
#' @param model.FUN Specifies the type of model that will be used for the estimation  Default is \code{coxph}.
#' @param bandwidth The proportion of data to be included in each kernel-smoothed estimate.  Univariate models only.
#' @param lambda The radius of the kernel for tri-cubic, Epanechnikov, and flat kernels.
#' The standard deviation for the Gaussian kernel.
#' @param kernel Specifies the kernel to be used: \code{epanechnikov}, \code{tricube}, \code{gaussian}, and \code{flat} are accepted.
#' Default is \code{epanechnikov}.  See vignettes for differences in methods.
#' @param dist.method Specifies the distance measure to be used in the kernel.  Default is \code{euclidean}.
#' Any distance measure accepted by \code{stats::dist} is acceptable.
#' @param quantile If \code{type = "quantile"}, specify the quantile to be estimates with a number between 0 and 1.
#' @param verbose Default is \code{FALSE}.  If \code{TRUE}, additional results will be returned as attributes, and more detailed errors will be printed.
#' @param ... Additional arguments to be passed to model.FUN function call.
#' @return A vector with the estimated (i.e. kernel-smoothed) outcomes.
#' @importFrom stats complete.cases
#' @importFrom survival Surv
#' @importFrom survival coxph
#' @export
sm.regress <- function(formula, data, newdata, type, model.FUN,
                       bandwidth = 0.8,
                       lambda = NULL,
                       kernel = c("epanechnikov", "tricube", "gaussian", "flat"),
                       dist.method = "euclidean",
                       quantile = NULL, # only used for survival regression to get survival quantiles
                       verbose = FALSE,
                       ...){

  # checking function inputs
  kernel <- match.arg(kernel)
  if (!is.null(lambda)) {
    if (lambda <= 0) stop("lambda must be positive")
  }


  #converting formula to string (if not already)
  if (class(formula) == "formula") formula <- deparse(formula)

  # extracting variable names from formula
  all.vars <- all.vars(stats::as.formula(formula))
  covars <- all.vars(stats::as.formula(formula)[[3]])
  outcome <- all.vars(stats::as.formula(formula)[[2]])

  # bandwidth cannot be used with more than one unique covariate
  if (is.null(lambda) && length(covars) > 1)
    stop("Must specify lambda with 2 or more covariates.")

  # if lambda is sepcified, making bandwidth NULL
  if (!is.null(lambda)) bandwidth = NULL

  # only keeping variables in model and complete cases
  data <- data[stats::complete.cases(data), all.vars]

  # Scaling covariates to mean 0 and SD 1
  data.scaled <- scale(data[covars])
  # adding the outcome variable (all analyses will be done on the scaled data)
  data[covars] <- data.scaled

  # scaling newdata to mean 0 and sd 1
  newdata <- newdata[all.vars]
  newdata[covars] <- dplyr::bind_cols(
    sapply(1:length(covars),
           function(a) (newdata[covars][, a] - attr(data.scaled, "scaled:center")[a]) /
             attr(data.scaled, "scaled:scale")[a])
  )

  # getting unique observations to save computation time (and complete cases)
  newdata.uniq <- unique(newdata[stats::complete.cases(newdata), ])

  # extracting each row from newdata and saving as it's own data frame and returning each row as a list entry
  tbl <- apply(newdata.uniq, 1, function(x) data.frame(t(x)))

  # calculating a list of lambdas IF bandwidth is supplied
  if (is.null(lambda) && !is.null(bandwidth)) {
    # number of obs that are included on each side on x0
    bandwidth.k = (nrow(data) * bandwidth - 0.5) / 2

    lambda.list <- purrr::map(
      tbl,
      ~ sjosmooth.bwidthlambda(.x, data, covars, bandwidth.k)
    )
  } else {
    # otherwise repeating the specified lambda in list
    lambda.list <- rep(list(lambda), length(tbl))
  }

  #calculating the kernel weights
  K <- purrr::map2(
    tbl, lambda.list,
    ~ sjosmooth.kernel(.x, data, kernel, dist.method, .y, covars)
  )

  # building models, returning NA if error
  model.obj <- purrr::map2(
    K, tbl, #this function doesn't need tbl.  only used to print more info for verbose errors#
    ~ sjosmooth.model(model.FUN, formula, data, K = .x, verbose, tbl = .y)
  )

  # calculating predictions on estimating point (tbl)
  preds <- purrr::map2_dbl(
    tbl, model.obj,
    ~ sjosmooth.prediction(type, model.obj = .y, tbl = .x, outcome, quantile, verbose)
  )

  # converting the list of tbls to a single data frame
  tbl <- dplyr::bind_rows(tbl)

  # extra results to be returned if requested
  if (verbose == TRUE)
    verbose.results = dplyr::bind_cols(
      tibble::tibble(
        K = K,
        model.obj = model.obj,
        preds = preds
      ),
      tbl
    )

  # binding predictions and covariates, merging back in with original data
  tbl <- add.var(tbl, type, preds)
  tbl <- dplyr::left_join(newdata, tbl, by = names(newdata))

  # extracting kernel smoothed vector from tibble
  result <- as.vector(tbl[type][[1]])

  # adding prediction type and model type to attributes
  attributes(result)$`type` <- type
  attributes(result)$`model` <- substitute(model.FUN) # substitute converts to string

  # adding additional output if verbose == TRUE
  if (verbose == TRUE) {
    attributes(result)$`verbose.results` <- verbose.results
    attributes(result)$`scaled:center` <- attributes(data.scaled)$`scaled:center`
    attributes(result)$`scaled:scale` <- attributes(data.scaled)$`scaled:scale`
  }

  # Returning kernel smoothed estimate
  return(result)
}

