#  HELPER FUNCTION (rank distance)  ---------------------------------------------------------------
# this function takes a vector input
# and return it's rank-distance from 0
# eg
# -100, -0.5, 0, 342, 299992 would result in
# -2, -1, 0, 1, 2
# the function is used to define lambda when a bandwidth is supplied
rankunidist = function(a) {

  #  putting all results in dataframe
  x.df = data.frame(x = a)

  # getting ranks for pos and neg separately
  x.df$rank = 0
  x.df$rank[x.df$x>0] =  rank( x.df$x[x.df$x>0], ties.method = "min")
  x.df$rank[x.df$x<0] = -rank(-x.df$x[x.df$x<0], ties.method = "min")

  #returning ranked results
  return(x.df$rank)
}

#  HELPER FUNCTION (bandwidth lambda)  ---------------------------------------------------------------
# function to calculate lambda based on bandwidth
sjosmooth.bwidthlambda <- function(tbl, data, covars, bandwidth.k) {
  dist = as.vector(data[[covars]] - as.numeric(tbl[covars]))

  rank.dist = rankunidist(dist)

  # returning lambda
  max(abs(dist[abs(rank.dist) <= bandwidth.k]))*1.0001
}  ###  THIS NEEDS TO BE CHECKED!



#  HELPER FUNCTION (calculate kernel weights)  ------------------------------------------------------
# this function calculates the kernel weights
sjosmooth.kernel <-
  function(tbl, data, kernel, dist.method, lambda, covars) {

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
    } else if (kernel == "flat") {
      K <- ifelse(( dist / lambda ) <= 1, 1, 0)
    } else {
      ##  PUT ERROR for not selecting appropriate kernel
      stop(paste("kernel =", kernel, "not an accepted input."))
    }

    return(K)
  }



#  HELPER FUNCTION (build model)  ---------------------------------------------------------------
# this function builds the model
sjosmooth.model <- function(model.FUN, formula, data, K, verbose, tbl){

    # if user requests point with no observations within the kernel radius, return a NULL model object
    if (sum(K) == 0) {
      message("Kernel weights sum to 0. Kernel-weighted model cannot be estimated for 1 observation--NA introduced.")
      return(NULL)
    }
    # building regression models, returning NA if error
    model.obj <- tryCatch(
      model.FUN(formula = stats::as.formula(formula),
                      data =  data[which(K > 0), ],
                      weights =  K[which(K > 0)  ]),
      error = function(e){
        #printing error and returning NA
        message(paste("Error in", substitute(model.FUN), "call"))
        if (verbose == TRUE) {
          print(tbl)
          print(e)
        }
        return(NA)
      }
    )

  return(model.obj)
}


#  HELPER FUNCTION (predictions)  ---------------------------------------------------------------
# this function calculates various types of predictions
sjosmooth.prediction <-
  function(type, model.obj, tbl, outcome, quantile, verbose){

    # if NULL model object is entered, returning NA.
    # This happens when a prediction point has all zero weights and the model cannot be estimated.
    if (is.null(model.obj)) return(NA)

    # calculating "survival", "failure", "expected"
    if (type %in% c("survival", "failure", "expected")){
      # calculating expected
      pred <- tryCatch(
        stats::predict(object = model.obj,
                       newdata = tbl,
                       type = "expected"),
        error = function(e){
          #printing error and returning NA
          message("Error in predict : NA introduced")
          if (verbose == TRUE) {
            print(tbl)
            print(e)
          }
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
          message("Error in survfit/quantile : NA introduced")
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
