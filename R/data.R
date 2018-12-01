#' Simulated survival outcome after treatment
#'
#' A dataset containing the baseline characteristics of 1000 patients, and
#' years survived after treatment.  Set also contains the 'true' probability
#' of survival at 1 year.  Data simulated via Cox PH model, with baseline
#' hazard 1, and linear predictor `xb = -2 + (1/60) * age + 0.2 * marker`,
#' where `age` and `marker` are independent.
#'
#' @format A data frame with one row per patient
#' \describe{
#'     \item{time}{Years to Death}
#'     \item{marker}{Marker Level, ng/mL}
#'     \item{age}{Age}
#'     \item{survt1}{True survival probability at 1 year}
#' }
"cancertx"
