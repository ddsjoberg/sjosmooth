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
#' @param time Specifies the timepoint where kernel smoothing will be performed.  Applies to \code{type = c("survival", "failure", "expected")}
#' If time is not specified, then the time from data (or newdata) will be used.
#' @param type Specifies the type of statistic that will be calculated.  Default is \code{survival}.
#' @param bandwidth The proportion of data to be included in each kernel-smoothed estimate.  Univariate models only.
#' @param lambda The radius of the kernel for tri-cubic, Epanechnikov, and flat kernels.
#' The standard deviation for the Gaussian kernel.
#' @param kernel Specifies the kernel to be used: \code{epanechnikov}, \code{tricube}, \code{gaussian}, and \code{flat} are accepted.
#' Default is \code{epanechnikov}.  See vignettes for differences in methods.
#' @param dist.method Specifies the distance measure to be used in the kernel.  Default is \code{euclidean}.
#' Any distance measure accepted by \code{stats::dist} is acceptable.
#' @param quantile If \code{type = "quantile"}, specify the quantile to be estimates with a number between 0 and 1.
#' @param ... Additional arguments to be passed to survival::coxph function call.
#' @return A vector with the estimated (i.e. kernel-smoothed) outcomes.
#' @export
sm.coxph <- function(formula, data,
                     newdata = NULL,
                     time = NULL,
                     bandwidth = 0.8,
                     lambda = NULL,
                     type = c("survival", "failure", "expected", "median", "quantile", "risk", "lp"),
                     kernel = c("epanechnikov", "tricube", "gaussian", "flat"),
                     dist.method = "euclidean",
                     quantile = NULL,
                     ...){

  # checking function inputs
  type <- match.arg(type)
  kernel <- match.arg(kernel)

    #converting formula to string (if not already)
  if (class(formula) == "formula") formula <- deparse(formula)

  # extracting variable names from formula
  all.vars <- all.vars(stats::as.formula(formula))
  covars <- all.vars(stats::as.formula(formula)[[3]])
  outcome <- all.vars(stats::as.formula(formula)[[2]])

  # if newdata not specified, then use input datatse
  if (is.null(newdata)) newdata <- data[,all.vars]

  # if time is given over-write time in newdata with time provided
  if (!is.null(time)) newdata[outcome[1]] <- time

  # ensuring newdata has correct form
  # these predictions need a time component
  need.outcome <- type %in% c("survival", "failure", "expected")

  # newdata must contain the same covariates as data
  if (!setequal(intersect(covars, names(newdata)), covars))
    stop("newdata must include all independent variables from regression model.")

  # if newdata is missing censoring variable, then adding one in
  # the predict.coxph requires it, but it is not used in calculating predictions
  if (need.outcome == T && length(outcome) == 2) {
    if (!(outcome[2] %in% names(newdata))) newdata[outcome[2]] <- data[1,outcome[2]]
  }


  ##  now running sm.regress to get the locally weighted estimates
  sm.regress(formula, data, newdata, type, model.FUN = coxph,
             bandwidth,
             lambda,
             kernel,
             dist.method,
             quantile,
             verbose = FALSE,
             ...)
}

