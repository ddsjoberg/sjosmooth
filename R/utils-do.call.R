#' @keywords internal

# run do.call safely.
# returns a list with two elements list(result, error).  THe first is the result, and the
# second the error message (if there is one). The first result is a list of the
# results list(result, warnings, messages)
# final output looks like this
# list(result = list(result, warnings, messages), error)
do.call_safely <-
  do.call %>%
  purrr::quietly() %>%
  purrr::safely()
