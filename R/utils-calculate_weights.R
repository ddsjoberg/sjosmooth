#' Provided a point, a data frame, and lambda, this function calculates the
#' weights each observation in the data frame provides the calculations
#'
#' @param data data frame
#' @param point center point around which weights are calculated in data frame
#' @param kernel type of kernel to use for weight calculation
#' @param lambda lambda parameter in kernel.
#' Often the circumference of points with positive weights
#' @param covars character vector of column names of the covariates
#' @param dist_method the distance measure to be used.
#' This must be one of "euclidean", "maximum", "manhattan", "canberra",
#' "binary" or "minkowski". Any unambiguous substring can be given.
#' @keywords internal

caclulate_weights <-
  function(data, point, kernel, dist_method, lambda, covars) {

    # calculating distance between estimation point, and actual data
    dist <- as.matrix(
      dist(
        dplyr::bind_rows(point[covars], data[covars]),
        method = dist_method
      )
    )[-1, 1]

    # calculating weights
    if (kernel == "epanechnikov") {
      return(
        ifelse(
          (dist / lambda) <= 1,
          0.75 * (1 - (dist / lambda)^2),
          0
        )
      )
    }
    if (kernel == "tricube") {
      return(
        ifelse(
          (dist / lambda) <= 1,
          (1 - (dist / lambda)^3)^3,
          0
        )
      )
    }
    if (kernel == "gaussian") {
      return(
        sqrt(2 * pi * lambda^2)^-length(covars) * exp(-0.5 / lambda^2 * dist^2)
      )
    }
    if (kernel == "flat") {
      return(
        ifelse((dist / lambda) <= 1, 1, 0)
      )
    }

    ##  PUT ERROR for not selecting appropriate kernel
    stop(paste("kernel =", kernel, "not an accepted input."))
  }
