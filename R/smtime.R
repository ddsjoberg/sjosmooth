#' Univariate and multivariable kernel smoothing with time to event outcomes.
#'
#' The purpose of this function is to provide kernel-smoothed n-year survival
#' estimates for n-dimensional data. For example, using two predictors, x and y,
#' predict time to death. The program estimates a hyperplane that estimates
#' risk of death at n-years for every x, y combination.
#'
#' @param formula The formula call used by \code{survival::coxph(formula = Surv(time, dead) ~ x + y)}.
#' Do not include transformed variables in the formula (e.g. \code{Surv(time, dead) ~ log(x) + y})--the specified
#' \code{lambda} will no longer be interpretable.
#' @param data A data.frame or tibble with named columns with the columns specified in formula.
#' @param newdata A data.frame or tibble with named columns that are the covariates specified in formula.
#' Survival probabilities will only be estimated at these points if specified.  Otherwise, survival probabilities
#' will be estimated at each unique combination of the observed covariates.
#' @param type Specifies the type of statistic that will be calculated.  Default is \code{survival}.
#' @param model Specifies the type of model that will be used for the estimation  Default is \code{coxph}.
#' @param bandwidth The proportion of data to be included in each kernel-smoothed estimate.  Univariate models only.
#' @param lambda The radius of the kernel for tri-cubic and Epanechnikov kernels.
#' The standard deviation for the Gaussian kernel.
#' @param kernel Specifies the kernel to be used: \code{epanechnikov}, \code{tricube}, \code{gaussian}, and \code{knn} are accepted.
#' Default is \code{epanechnikov}.  See vignettes for differences in methods.
#' @param dist.method Specifies the distance measure to be used in the kernel.  Default is \code{euclidean}.
#' Any distance measure accepted by \code{stats::dist} is acceptable.
#' @param scale If TRUE, the covariates are scaled to mean 0 and standard deviation 1 prior to kernel smoothing.
#' For one covariate the default is scale = FALSE, and for 2 of more covariates the default is scale = TRUE.
#' If including 2 or more covariates, scale should be set to \code{TRUE}.
#' @param time Optional argument for specifying one timepoint at which predictions will be evaluated (e.g. \code{type = c("survival", "expected", "failure")}).
#' @param knn Only used with \code{kernel == "knn"}.  Positive integer that specifies how many observations (k-nearest neighbors)
#' to include for each point estimate.
#' @param quantile If \code{type = "quantile"}, specify the quantile to be estimates with a number between 0 and 1.
#' @param verbose Default is \code{FALSE}.  If \code{TRUE}, additional results will be return as attributes, and more detailed errors will be printed.
#' @return A vector with the estimated survival probability.
#' @importFrom survival Surv
#' @export
smtime <- function(formula, data, newdata = NULL,
                   bandwidth = 0.8,
                   lambda = NULL,
                   type = c("survival", "failure", "expected",
                            "median", "quantile"),
                   kernel = c("epanechnikov", "tricube", "gaussian", "knn"),
                   dist.method = "euclidean",
                   model = "coxph",
                   scale = FALSE,
                   time = NULL,
                   knn = NULL,
                   quantile = NULL,
                   verbose = FALSE){

  # checking function inputs
  type <-        match.arg(type)
  kernel <-      match.arg(kernel)
  model <-       match.arg(model)
  if (lambda <= 0) stop("lambda must be positive")
  if (kernel == "knn" & is.null(knn))
    stop('knn must be specified when kernal == "knn"')


  #converting formula to string (if not already)
  if (class(formula) == "formula") formula <- deparse(formula)

  # extracting variable names from formula
  all.vars <- all.vars(stats::as.formula(formula))
  covars <- all.vars(stats::as.formula(formula)[[3]])
  outcome <- all.vars(stats::as.formula(formula)[[2]])

  # printing message if scale = F and there are 2 or more covariates
  if (scale == FALSE & length(covars) > 1)
    message("scale == FALSE with 2 or more covariates is not recommended")

  # bandwidth cannot be used with more than one unique covariate
  if (is.null(lambda) && length(covars) > 1)
    stop("Must specify lambda with 2 or more covariates.")

  # if lambda is sepcified, making bandwidth NULL
  if (!is.null(lambda)) bandwidth = NULL

  # only keeping variables in model
  data <- data[all.vars]

  # Scaling covariates to mean 0 and SD 1 if requested
  if (scale == TRUE){
    data.scaled <- scale(data[covars])
    # adding the outcome variable (all analyses will be done on the scaled data)
    data[covars] <- data.scaled
  }

  # list of outcomes that need a specified outcome (e.g. time)
  # list of vars to keep in dataset for prediction
  need.time <- c("survival", "failure", "expected")
  if (type %in% need.time) newdata.vars <- c(outcome, covars)
  else newdata.vars <- covars

  # which points to calculate the weighted estimates for
  # default is observed observations
  if (is.null(newdata) == TRUE) {
    # keeping unique combinations of covars,
    # keeping newdata0 to be a copy of the original data
    newdata <- data[newdata.vars]

  } else if (is.null(newdata) == FALSE){
    # if newdata of points was provided in function call
    # keeping unique combinations of covars
    newdata <- newdata[newdata.vars]

    #scaling data to mean 0 and sd 1
    if (scale == TRUE)
      newdata[covars] <-
        (newdata[covars] - attributes(data.scaled)$`scaled:center`) /
         attributes(data.scaled)$`scaled:scale`
  }

  # adding/replacing a timepoint if specified
  if (is.null(time) == F && need.time == T) newdata <- add.var(newdata, outcome[1], time)

  # adding the censoring indicator if not already in dataset
  # (predict function wants this input, even though not needed)
  if (length(outcome) > 1 && need.time == T) {
    if (!(outcome[2] %in% names(newdata))) newdata <- add.var(newdata, outcome[2], 0)
  }

  # getting unique observations to save computation time (and complete)
  newdata.uniq <- unique(newdata[complete.cases(newdata), ])

  # extracting each row from newdata and saving as it's own tibble
  tbl <- apply(newdata.uniq, 1, function(x) data.frame(t(x)))

  # calculating a list of lambdas IF bandwidth is supplied
  if (is.null(lambda) && !is.null(bandwidth)) {
    #####################  CONTINUE UPDATING HERE
    # number of obs that are included on each side on x0
    bandwidth.k = (nrow(data) * bandwidth - 0.5) / 2

  }

  #calculating the kernel weights
  K <- purrr::map(
    tbl,
    ~ sjosmooth.kernel(.x, data, kernel, dist.method, lambda, covars, knn)
  )

  # building models, returning NA if error
  model.obj <- purrr::map(
    K,
    ~ sjosmooth.model(model, formula, data, K = .x, verbose)
  )

  # calculating predictions on estimating point (tbl)
  preds <- purrr::map2_dbl(
    tbl,
    model.obj,
    ~ sjosmooth.prediction(type, model.obj = .y, tbl = .x, outcome, quantile, verbose)
  )

  # extra results to be returned if requested
  if (verbose == TRUE)
    verbose.results = tibble::tibble(
      tbl = tbl,
      K = K,
      model.obj = model.obj,
      preds = preds
    )

  # converting the list of tbls to a single data frame
  tbl <- dplyr::bind_rows(tbl)

  # binding predictions and covariates, merging back in with original data
  tbl <- add.var(tbl, type, preds)
  tbl <- dplyr::left_join(newdata, tbl, by = newdata.vars)

  # extracting kernel smoothed vector from tibble
  result <- as.vector(tbl[type][[1]])

  # adding prediction type to attributes
  attributes(result)$`type` <- type
  # adding additional output if verbose == TRUE
  if (verbose == TRUE)
    attributes(result)$`verbose.results` <- verbose.results

  # Returning kernel smoothed estimate
  return(result)
}

