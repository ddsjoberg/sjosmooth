set.seed(5641324)

# simulating 2 covariates to predict time to event
seq.n = 15
sjosmooth.tbl = tibble::tibble(x = seq(from = 0, to = 1, length.out = seq.n),
                               y = seq(from = 0, to = 1, length.out = seq.n))
sjosmooth.tbl = tidyr::complete(sjosmooth.tbl, x, y)

# simulating the time to event outcome assuming a correct Cox model
sjosmooth.tbl = dplyr::mutate(sjosmooth.tbl,
                              xb = x + y,
                              time.true = -log(runif(seq.n^2, 0, 1)) * exp(-xb),
                              time.cens = runif(seq.n^2, 0, max(time.true)),
                              time.obs = pmin(time.true, time.cens),
                              event = as.numeric(time.true<= time.cens))

# creating a data.frame version of the data
sjosmooth.df = data.frame(sjosmooth.tbl)

#appending datasets to package
usethis::use_data(sjosmooth.tbl, sjosmooth.df, overwrite = TRUE)

#' A simulated tibble used to motivate examples in the \code{sjosmooth} package
#'
#' @format A tibble with 7 variables:
#' \describe{
#'   \item{x}{Covariate x}
#'   \item{y}{Covariate y}
#'   \item{xb}{Linear portion of Cox regression model (i.e. x + y)}
#'   \item{time.true}{The true time to death}
#'   \item{time.cens}{Censoring time}
#'   \item{time.obs}{Observed time to death/censoring time}
#'   \item{event}{Death observed: yes/no (1/0)}
#' }
"sjosmooth.tbl"

#appending datasets to package
usethis::use_data(sjosmooth.tbl, sjosmooth.df, overwrite = TRUE)

#' Same as sjosmooth.tbl, but as a data frame.  Primarily used for testing.
#' A simulated data frame used to motivate examples in the \code{sjosmooth} package
#'
#' @format A data frame with 7 variables:
#' \describe{
#'   \item{x}{Covariate x}
#'   \item{y}{Covariate y}
#'   \item{xb}{Linear portion of Cox regression model (i.e. x + y)}
#'   \item{time.true}{The true time to death}
#'   \item{time.cens}{Censoring time}
#'   \item{time.obs}{Observed time to death/censoring time}
#'   \item{event}{Death observed: yes/no (1/0)}
#' }
"sjosmooth.df"
