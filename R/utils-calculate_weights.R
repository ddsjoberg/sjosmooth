#' Provided a point, a data frame, and lambda, this function calculates the
#' weights each observation in the data frame provides the calculations
#'
#' @param dist distance vector
#' @param lambda The radius of the kernel for tri-cubic, Epanechnikov, and flat kernels.
#' The standard deviation for the Gaussian kernel.
#' @param kernel type of kernel to use for weight calculation
#' @keywords internal
#' @export

calculate_weights <- function(dist, lambda, kernel, weighting_var) {

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
      sqrt(2 * pi * lambda^2)^-length(weighting_var) * exp(-0.5 / lambda^2 * dist^2)
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
