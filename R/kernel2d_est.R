#' MASS' Two-Dimensional Kernel Density Estimation
#'
#' Two-dimensional kernel density estimation with an axis-aligned bivariate normal kernel, evaluated on a square grid. Originally from \code{MASS} package, this function was copied to avoid CRAN notes.
#' 
#' Return a list of three components:
#' 
#' \describe{
#'   \item{x}{The x coordinates of the grid points, vector of length n}
#'   \item{y}{The y coordinates of the grid points, vector of length n}
#'   \item{z}{An n[1] by n[2] matrix of the estimated density: rows correspond to the value of x, columns to the value of y.}
#' }
#' 
#' @param x Type: numeric vector. x coordinate of data.
#' @param y Type: numeric vector. y coordinate of data.
#' @param h Type: numeric vector. Vector of bandwidths for x and y directions. Defaults to normal reference bandwidth. A scalar value will be taken to apply to both directions.
#' @param n Type: numeric vector. Number of grid points in each direction. Can be scalar or a length-2 integer vector.
#' @param lims Type: numeric vector. The limits of the rectangle covered by the grid as \code{c(xl, xu, yl, yu)}.
#' 
#' @return A list of three elements.
#' 
#' @examples
#' \dontrun{
#' #?MASS::kde2d
#' }
#' 
#' @export

kernel2d_est <- function (x, y, h, n = 25, lims = c(range(x), range(y))) {
  nx <- length(x)
  if (length(y) != nx)
    stop("data vectors must be the same length")
  if (any(!is.finite(x)) || any(!is.finite(y))) 
    stop("missing or infinite values in the data are not allowed")
  if (any(!is.finite(lims))) 
    stop("only finite values are allowed in 'lims'")
  n <- rep(n, length.out = 2L)
  gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
  gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
  h <- if (missing(h)) 
    c(bandwidth_rot(x), bandwidth_rot(y))
  else rep(h, length.out = 2L)
  if (any(h <= 0)) 
    stop("bandwidths must be strictly positive")
  h <- h / 4
  ax <- outer(gx, x, "-") / h[1L]
  ay <- outer(gy, y, "-") / h[2L]
  z <- tcrossprod(matrix(dnorm(ax), , nx), matrix(dnorm(ay), , nx)) / (nx * h[1L] * h[2L])
  list(x = gx, y = gy, z = z)
}