#' n-dimensional kernel-smoothed survival probabilities
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
#' @param lambda The radius of the kernel for tri-cubic and Epanechnikov kernels.
#' The standard deviation for the Gaussian kernel.
#' @param kernel Specifies the kernel to be used: \code{epanechnikov}, \code{tricube}, \code{gaussian}, and \code{knn} are accepted.
#' Default is \code{epanechnikov}.  See vignettes for differences in methods.
#' @param dist.method Specifies the distance measure to be used in the kernel.  Default is \code{euclidean}.
#' Any distance measure accepted by \code{stats::dist} is acceptable.
#' @param scale If TRUE, the covariates are scaled to mean 0 and standard deviation 1 prior to kernel smoothing.
#' For one covariate the default is scale = FALSE, and for 2 of more covariates the default is scale = TRUE.
#' If including 2 or more covariates, scale should be set to \code{TRUE}.
#' @param knn Only used with \code{kernel == "knn"}.  Positive integer that specifies how many observations (k-nearest neighbors)
#' to include for each point estimate.
#' @param verbose Default is \code{FALSE}.  If \code{TRUE}, additional results will be return as attributes, and more detailed errors will be printed.
#' @return A vector with the estimated survival probability.
#' @importFrom survival Surv
#' @export
sm.timeto = function(formula, data, lambda = 1, newdata = NULL,
                     type = c("survival", "failure", "expected", "median", "quantile"),
                     kernel = c("epanechnikov", "tricube", "gaussian", "knn"),
                     dist.method = "euclidean",
                     model = "coxph", scale = FALSE,
                     knn = NULL, quantile = NULL, verbose = FALSE){
  # checking function inputs
  type =        match.arg(type)
  kernel =      match.arg(kernel)
  model =       match.arg(model)
  if (lambda <= 0) stop("lambda must be positive")
  if (kernel == "knn" & is.null(knn)) stop('knn must be specified when kernal == "knn"')


  #converting formula to string (if not already)
  if(class(formula) == "formula") formula = deparse(formula)

  # extracting variable names from formula
  all.vars = all.vars(stats::as.formula(formula))
  covars = all.vars(stats::as.formula(formula)[[3]])
  outcome = all.vars(stats::as.formula(formula)[[2]])

  # printing message if scale = F and there are 2 or more covariates
  if (scale == FALSE & length(covars)>1) message("scale == FALSE with 2 or more covariates is not recommended")

  # only keeping variables in model
  data = data[all.vars]

  # Scaling covariates to mean 0 and SD 1 if requested
  if (scale == TRUE){
    data.scaled = scale(data[covars])
    # adding the outcome variable (all analyses will be done on the scaled data)
    data[covars] = data.scaled
  }

  # list of outcomes that need a specified time
  # list of vars to keep in dataset for prediction
  need.time = c("survival", "failure", "expected")
  if (type %in% need.time) newdata.vars = c(outcome[1], covars)
  else newdata.vars = covars

  # which points to calculate the weighted estimates for
  if(is.null(newdata) == TRUE){ # default is observed observations
    # keeping unique combinations of covars, keeping newdata0 to be a copy of the original data
    newdata = data[newdata.vars]
    newdata.uniq = unique(newdata)
  } else if(is.null(newdata) == FALSE){ # if newdata of points was provided in function call
    # keeping unique combinations of covars
    newdata = newdata[newdata.vars]
    #scaling data to mean 0 and sd 1
    if (scale == TRUE) newdata[covars] = (newdata[covars] - attributes(data.scaled)$`scaled:center`) /
                                                          attributes(data.scaled)$`scaled:scale`

    newdata.uniq = unique(newdata)
  }

  # extracting each row from newdata and saving as it's own tibble
  tbl = apply(newdata.uniq, 1, function(x) data.frame(t(x)))

  #calculating the kernel weights
  K = purrr::map(tbl, ~ sjosmooth.kernel(.x, data, kernel, dist.method, lambda, covars, knn))

  # building models, returning NA if error
  model.obj = purrr::map(K, ~ sjosmooth.model(model, formula, data, .x, verbose))

  # calculating predictions on estimating point (tbl)
  preds = purrr::map2_dbl(tbl, model.obj, ~ sjosmooth.prediction(type, .y, .x, outcome, quantile, verbose))

  # extra results to be returned if requested
  if (verbose == TRUE) verbose.results = tibble::tibble(tbl = tbl,
                                                        K = K,
                                                        model.obj = model.obj,
                                                        preds)

  # converting the list of tbls to a single data frame
  tbl = dplyr::bind_rows(tbl)

  # binding predictions and covariates, merging back in with original data
  tbl = add.var(tbl, type, preds)
  tbl = dplyr::left_join(newdata, tbl, by = newdata.vars)

  # extracting kernerl smoothed vector from tibble
  result = as.vector(tbl[type][[1]])

  # adding prediction type to attributes
  attributes(result)$`type` = type
  # adding additional output if verbose == TRUE
  if (verbose == TRUE) attributes(result)$`verbose.results` = verbose.results

  # Returning kernel smoothed estimate
  return(result)
}


# this function calculates the kernel weights
sjosmooth.kernel = function(tbl, data, kernel, dist.method, lambda, covars, knn) {
  # calculating distance between estimation point (in tbl), and actual data
  dist = as.matrix(dist(dplyr::bind_rows(tbl[covars], data[covars]), method = dist.method))[-1,1]

  # calculating weights, and adding to tbl
  if (kernel == "epanechnikov") {
    K = ifelse((dist / lambda) <= 1, 0.75*(1 - (dist / lambda)^2), 0)
  } else if (kernel == "tricube") {
    K = ifelse((dist / lambda) <= 1, (1 - (dist / lambda)^3)^3, 0)
  } else if (kernel == "gaussian") {
    K = sqrt(2*pi*lambda^2)^-length(covars) * exp(-0.5/lambda^2 * dist^2)
  } else if (kernel == "knn") {
    if (kernel == "knn" & is.null(knn)) stop('knn must be specified when kernal == "knn"')
    K = ifelse(rank(dist, ties.method = "min") <= knn, 1, 0)
  } else {
    ##  PUT ERROR for not selecting appropriate kernel
    stop(paste("kernel ==", kernel, "not an accepted input."))
  }

  return(K)
}



# this function builds the model
sjosmooth.model = function(model, formula, data, K, verbose){

  if (model == "coxph"){
    # building cox models, returning NA if error
    model.obj = tryCatch(survival::coxph(formula = stats::as.formula(formula),
                                         data =  data[which(K>0),],
                                         weights =  K[which(K>0) ]),
                         error=function(e){
                           #printing error and returning NA
                           message("Error in survival::coxph : verbose = TRUE to see errors")
                           if (verbose == TRUE) print(e)
                           return(NA)
                         })
  } else {
    # return error for not selecting appropriate model
    stop(paste("model ==", model, "not an accepted input."))
  }

  return(model.obj)
}


#this function calculates various types of predictions
sjosmooth.prediction = function(type, model.obj, tbl, outcome, quantile, verbose){

  # calculating "survival", "failure", "expected"
  if (type %in% c("survival", "failure", "expected")){
    # calculating expected
    pred = tryCatch(stats::predict(object = model.obj,
                                   newdata = tbl,
                                   type = "expected"),
                    error=function(e){
                      #printing error and returning NA
                      message("Error in predict : NAs introduced, verbose = TRUE to see errors")
                      if (verbose == TRUE) print(e)
                      return(NA)
                    })
    # calculating the survival probability
    if (type %in% c("survival", "failure")) pred = exp(- pred)
    # calculating the failure probability
    if (type %in% c("failure")) pred = 1 - pred

  } else if (type %in% c("median", "quantile")) {
    # calculating the survival quantile
    if (type == "median") quantile = 0.5

    pred = tryCatch(stats::quantile(survival::survfit(model.obj,
                                                      newdata = tbl),
                                    probs = quantile)$quantile,
                    error=function(e){
                      #printing error and returning NA
                      message("Error in survfit/quantile : NAs introduced, verbose = TRUE to see errors")
                      if (verbose == TRUE) print(e)
                      return(NA)
                    })
  } else {
    ### return error for not selecting an appropriate outcome
    stop(paste("type ==", type, "not an accepted input."))
  }

  return(pred)
}


# this simple function adds a variable to a tibble or data frame
add.var = function(data , varname, varvalue){
  data[[varname]] <- with(data, varvalue)
  return(data)
}
