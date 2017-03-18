#' Get Function Time in Milliseconds
#'
#' This function returns the time needed for a function to run in milliseconds.
#' 
#' @param expr Type: any expression. An expression to evalutate (the expression you want to benchmark).
#' 
#' @return The time needed for a function to run in milliseconds.
#' 
#' @examples
#' library(R.utils)
#' timer_func({for (i in 1:100) {cat(i, " ")}})
#' 
#' @export

timer_func <- function(expr) {
  current_time <- Laurae::timer()
  expr
  end_time <- Laurae::timer()
  return(end_time - current_time)
}
