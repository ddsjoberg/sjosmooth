#  HELPER FUNCTION (rank distance)  ---------------------------------------------------------------
# this function takes a vector input
# and return it's rank-distance from 0
# eg
# -100, -0.5, 0, 342, 299992 would result in
# -2, -1, 0, 1, 2
# the function is used to define lambda when a badnwidth is supplied
rankunidist = function(x) {

  # getting ranks for pos and neg separately
  x.pos = x[x>0]
  x.neg = x[x<0]
  rank.pos = rank(x.pos, ties.method = "min")
  rank.neg = -rank(-x.neg, ties.method = "min")

  # joining ranks back into original data
  x = dplyr::left_join(
    data.frame(x = x),
    data.frame(x = x.pos, rank.pos),
    by = "x"
  )
  x = dplyr::left_join(
    x,
    data.frame(x = x.neg, rank.neg),
    by = "x"
  )

  #returning ranked results
  dplyr::coalesce(x$rank.pos, x$rank.neg, 0L)
}

#  HELPER FUNCTION (bandwidth lambda)  ---------------------------------------------------------------
# function to calculate lambda based on badwidth
sjosmooth.bwidthlambda <- function(tbl, data, covars, bandwidth.k) {
  dist = data[covars] - as.numeric(tbl[covars])
  rank.dist = rankunidist(dist)

  # returning lambda
  max(abs(dist[abs(rank.dist) <= bandwidth.k]))
}  ###  THIS NEEDS TO BE CHECKED!


#  HELPER FUNCTION (calculate kernel weights)  ------------------------------------------------------
# this function calculates the kernel weights
sjosmooth.kernel <-
  function(tbl, data, kernel, dist.method, lambda, covars, knn) {

    # calculating distance between estimation point (in tbl), and actual data
    dist <- as.matrix(
      dist(
        dplyr::bind_rows(tbl[covars], data[covars]),
        method = dist.method
      )
    )[-1, 1]

    # calculating weights
    if (kernel == "epanechnikov") {
      K <- ifelse( ( dist / lambda ) <= 1, 0.75 * (1 - ( dist / lambda ) ^ 2 ), 0)
    } else if (kernel == "tricube") {
      K <- ifelse( ( dist / lambda ) <= 1, ( 1 - ( dist / lambda ) ^ 3) ^ 3, 0)
    } else if (kernel == "gaussian") {
      K <- sqrt(2 * pi * lambda ^ 2) ^ -length(covars) *
        exp(-0.5 / lambda ^ 2 * dist ^ 2)
    } else if (kernel == "knn") {
      # printing error if knn not specified
      if (kernel == "knn" & is.null(knn))
        stop('knn must be specified when kernal == "knn"')
      K <- ifelse(rank(dist, ties.method = "min") <= knn, 1, 0)
    } else {
      ##  PUT ERROR for not selecting appropriate kernel
      stop(paste("kernel ==", kernel, "not an accepted input."))
    }

    return(K)
  }



#  HELPER FUNCTION (build model)  ---------------------------------------------------------------
# this function builds the model
sjosmooth.model <- function(model, formula, data, K, verbose){

  if (model == "coxph"){
    # building cox models, returning NA if error
    model.obj <- tryCatch(
      survival::coxph(formula = stats::as.formula(formula),
                      data =  data[which(K > 0), ],
                      weights =  K[which(K > 0)  ]),
      error = function(e){
        #printing error and returning NA
        message("Error in survival::coxph")
        if (verbose == TRUE) print(e)
        return(NA)
      }
    )
  } else {
    # return error for not selecting appropriate model
    stop(paste("model ==", model, "not an accepted input."))
  }

  return(model.obj)
}


#  HELPER FUNCTION (predictions)  ---------------------------------------------------------------
#this function calculates various types of predictions
sjosmooth.prediction <-
  function(type, model.obj, tbl, outcome, quantile, verbose){

    # calculating "survival", "failure", "expected"
    if (type %in% c("survival", "failure", "expected")){
      # calculating expected
      pred <- tryCatch(
        stats::predict(object = model.obj,
                       newdata = tbl,
                       type = "expected"),
        error = function(e){
          #printing error and returning NA
          message("Error in predict : NAs introduced")
          if (verbose == TRUE) print(e)
          return(NA)
        }
      )
      # calculating the survival probability
      if (type %in% c("survival", "failure")) pred <- exp(-pred)
      # calculating the failure probability
      if (type %in% c("failure")) pred <- 1 - pred

    } else if (type %in% c("median", "quantile")) {
      # calculating the survival quantile
      if (type == "median") quantile <- 0.5

      pred <- tryCatch(
        stats::quantile(survival::survfit(model.obj,
                                          newdata = tbl),
                        probs = quantile)$quantile,
        error = function(e){
          #printing error and returning NA
          message("Error in survfit/quantile : NAs introduced")
          if (verbose == TRUE) print(e)
          return(NA)
        }
      )
    } else {
      ### return error for not selecting an appropriate outcome
      stop(paste("type ==", type, "not an accepted input."))
    }

    return(pred)
  }

#  HELPER FUNCTION (add vars)  ---------------------------------------------------------------
# this simple function adds a variable to a tibble or data frame
add.var <- function(data, varname, varvalue){
  data[[varname]] <- with(data, varvalue)
  return(data)
}
