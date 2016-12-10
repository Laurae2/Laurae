#' Get current Time in Milliseconds
#'
#' This function returns the current time in milliseconds.
#' 
#' @param None
#' 
#' @return The current time at the call in milliseconds.
#' 
#' @examples
#' library(R.utils)
#' timer()
#' 
#' @export

timer <- function() {
  return(System$currentTimeMillis())
}