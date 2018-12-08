#' Prints errors/warnings/messages
#'
#' Prints errors/warnings/messages from the regression model function and from the
#' predict function.  The function will print each unique message along with the
#' data associated with the message.
#'
#' @param data data frame
#' @param message_col string with column name of message to print
#' @param preamble character string to print before the message
#' (e.g. 'Error from coxph:')

message_print <- function(data, message_col, preamble) {

  x <- data[c("newdata", message_col)]
  x[["..message_chr.."]] <- purrr::map_chr(
    x[[message_col]],
    ~ifelse(
      is.null(.x) | (length(.x) == 0 & !is.null(.x)),
      NA_character_,
      .x %>% as.character()
    )
  )

  message_unique <-
    x$..message_chr.. %>%
    stats::na.omit() %>%
    unique()


  # if no message end function
  if(length(message_unique) == 0) stop()

  # printing messages and data
  for (m in message_unique) {
    message(paste(preamble, m))

    print(
      x %>%
        dplyr::filter(~..message_chr.. == m) %>%
        dplyr::select("newdata") %>%
        tidyr::unnest_("newdata")
    )

  }
}


# t = sm_regression(
#   data = mtcars,
#   method = "glm",
#   formula = am ~ mpg,
#   weighting_var = "mpg",
#   method.args = list(family = binomial(link = "logit")),
#   newdata = dplyr::data_frame(mpg = c(15, 30:40))
# )
#
# message_print(t, "model_error", "Error in glm:")
# message_print(t, "model_warning", "Warning in glm:")
# message_print(t, "model_message", "Message in glm:")
