#' MASS' Rule of Thumb Bandwidth Estimation
#'
#' A well-supported rule-of-thumb for choosing the bandwidth of a Gaussian kernel density estimator.
#' 
#' @param x Type: numeric vector. A data vector.
#' 
#' @return A bandwidth on a scale suitable for the \code{width} argument of \code{density}.
#' 
#' @examples
#' \dontrun{
#' #?MASS::bandwidth.nrd
#' bandwidth_rot <- function (x) {
#'   r <- quantile(x, c(0.25, 0.75))
#'   h <- (r[2L] - r[1L])/1.34
#'   4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
#' }
#' }
#' 
#' @export

bandwidth_rot <- function (x) {
  r <- quantile(x, c(0.25, 0.75))
  h <- (r[2L] - r[1L])/1.34
  4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
}