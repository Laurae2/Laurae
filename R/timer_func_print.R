#' Get Function Time in Milliseconds (with printing)
#'
#' This function returns the time needed for a function to run in milliseconds, and prints it.
#' 
#' @param expr Type: any expression. An expression to evalutate (the expression you want to benchmark).
#' @param seconds Type: logical. Whether you want milliseconds (default) or seconds. Defaults to \code{FALSE}.
#' @param pre_msg Type: character. The message preceeding the time print. Defaults to \code{"The function ran in "}.
#' @param msg_format Type: character. The formatting applied to the evaluation time, used by \code{sprintf}. Defaults to \code{"\%05.03f"}.
#' @param post_msg Type: character. The message succeeding the time print. Defaults to \code{ifelse(seconds == TRUE, " seconds.", " milliseconds."}.
#' @param linebreak Type: logical. Whether to add a line break after the printed message. Defaults to \code{TRUE}.
#' 
#' @return The time needed for a function to run in milliseconds (or seconds).
#' 
#' @examples
#' library(R.utils)
#' timer_func_print({for (i in 1:100) {rnorm(1)}}, seconds = TRUE)
#' 
#' @export

timer_func_print <- function(expr, seconds = FALSE, pre_msg = "The function ran in ", msg_format = "%05.03f", post_msg = ifelse(seconds == TRUE, " seconds.", " milliseconds."), linebreak = TRUE) {
  current_time <- Laurae::timer()
  expr
  end_time <- Laurae::timer()
  total_time <- unname(end_time - current_time) / (seconds * 999 + 1)
  cat(pre_msg, sprintf(msg_format, total_time), post_msg, rep("\n", linebreak), sep = "")
  return(total_time)
}
