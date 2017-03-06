#' Fast mean computation
#' 
#' This function allows to perform \code{sum(x) / length(x)} in one shot, without performing any background check on the data. Beware of integer overflow!
#' 
#' @param x Type: unknown. The variable to perform \code{sum(x)/length(x)} on.
#' 
#' @return The mean.
#' 
#' @examples
#' mean2(c(1:1000000))
#' 
#' @export

mean2 <- function(x) {
  return(sum(x) / length(x))
}
