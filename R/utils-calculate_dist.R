#' Calculate the distance vector between a single point and each observation
#' in the data frame
#'
#' @param data data frame
#' @param point center point around which weights are calculated in data frame
#' @param dist_method the distance measure to be used.
#' This must be one of "euclidean", "maximum", "manhattan", "canberra",
#' "binary" or "minkowski". Any unambiguous substring can be given.
#' @keywords internal

calculate_dist <- function(data, point, dist_method) {

  # calculating distance between estimation point, and actual data
  as.matrix(
    stats::dist(
      dplyr::bind_rows(point, data),
      method = dist_method
    )
  )[-1, 1]
}
