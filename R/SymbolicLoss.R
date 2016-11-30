#' Symbolic Gradient/Hessian Loss computation
#'
#' This function computes the 1st and 2nd symbolic derivatives of the loss function (gradient/hessian) provided.
#' 
#' This function cannot handle any type of input. It cannot handle sums or loops in the function code. It handles the following, in the alphabetic order:
#' 
#' \describe{
#'   \item{\*}{Multiplication}
#'   \item{/}{Division}
#'   \item{^}{Power}
#'   \item{abs}{Absolute value function}
#'   \item{acos}{Arcosine function}
#'   \item{acosh}{Hyperbolic Arcosine function}
#'   \item{asin}{Arsine function}
#'   \item{asinh}{Hyperbolic Arcsine function}
#'   \item{atan}{Arctangent function}
#'   \item{atan2}{Arctangent angle function between the x-axis and the vector from the origin (x,y), atan=y/x if x>0 and y>0}
#'   \item{atanh}{Hyperbolic Arctangent function}
#'   \item{besselI}{Modified Bessel function of the first kind}
#'   \item{besselJ}{Bessel function of the first kind}
#'   \item{besselK}{Modified Bessel function of the second kind}
#'   \item{besselY}{Sphereical Bessel function}
#'   \item{beta}{Beta function (Eulerian integral of the first kind)}
#'   \item{cos}{Cosine function}
#'   \item{cosh}{Hyperbolic cosine function}
#'   \item{cospi}{Cosine function with argument multiplicand pi}
#'   \item{dbinom}{Density binomial function}
#'   \item{digamma}{First derivative of the logarithm of the gamma function}
#'   \item{dnorm}{Density normal function}
#'   \item{exp}{Exponential function}
#'   \item{expm1}{Exponential function minus 1}
#'   \item{gamma}{Gamma function (Mellin transform of the negative exponential function)}
#'   \item{lbeta}{Natural logarithm of the beta function}
#'   \item{lgamma}{Natural logarithm of the absolute value of the gamma function}
#'   \item{log}{Natural (e) logarithm function}
#'   \item{log10}{Common (10) logarithm function}
#'   \item{log1p}{Natural (e) logarithm function with 1 added to the argument}
#'   \item{log2}{Binary (2) logarithm function}
#'   \item{logb}{Logarithm function of base b (base)}
#'   \item{pnorm}{Normal distribution function}
#'   \item{psigamma}{Polygamma function (degree specified by deriv)}
#'   \item{rep.int}{Replicate "times" elements of vectors and lists}
#'   \item{rep_len}{Replicate "length.out" elements of vectors and lists}
#'   \item{sign}{Sign function}
#'   \item{sin}{Sine function}
#'   \item{sinh}{Hyperbolic sine function}
#'   \item{sinpi}{Sine function with argument multiplicand pi}
#'   \item{sqrt}{Square root function}
#'   \item{tan}{Tangent function}
#'   \item{tanh}{Hyperbolic tangent function}
#'   \item{tanpi}{Tangent function with argument multiplicand pi}
#'   \item{trigamma}{Second derivative of the logarithm of the gamma function}
#' }
#' 
#' @param fc The loss function to derivate twice. Gradient and hessian are computed and returned into a list to the user.
#' @param fc_ref The loss function for reference to compare when using \code{plotting = TRUE}. Defaults to \code{NULL}.
#' @param verbose Whether the functions should be printed to the console while being returned. Defaults to \code{TRUE}.
#' @param plotting Whether the functions should be plotted for debugging purposes. Defaults to \code{TRUE}.
#' @param xmin The x-axis minimum when plotting data when \code{plotting = TRUE}. Defaults to \code{-10}.
#' @param xmax The x-axis maximum when plotting data when \code{plotting = TRUE}. Defaults to \code{10}.
#' @param xpoint How many poitns to plot when \code{plotting = TRUE}. Defaults to \code{20}.
#' @param ... Arguments to pass to \code{fc}.
#' 
#' @return A list with \code{grad} as the gradient of the loss function and \code{hess} as the hessian of the loss function.
#' 
#' @examples
#' # Median Fair loss (just the Fair loss...)
#' library(Deriv) # loads the required library
#' fc <- function(x, c=2, t=0.5)
#' {(c^2) * ((abs(x) / c) - log(1 + (abs(x) / c))) * ifelse(x > 0, 2 * t, 2-2*t)}
#' fc_ref <- function(x) {x^2} # Quadratic loss, aka Mean Squared Error
#' SymbolicLoss(fc = fc,
#'              fc_ref = fc_ref,
#'              verbose = TRUE,
#'              plotting = TRUE)
#' 
#' @export

SymbolicLoss <- function(fc, fc_ref = NULL, verbose = TRUE, plotting = TRUE, xmin = -10, xmax = 10, xpoint = 20, ...) {
  
  mini_func_grad <- Deriv(fc, "x")
  mini_func_hess <- Deriv(fc, "x", nderiv = 2)
  
  if (plotting) {
    
    has_ref <- !is.null(fc_ref)
    if (has_ref) {
      #   ref_name <- substitute(fc_ref)
      mini_func_grad_ref <- Deriv(fc_ref, "x")
      mini_func_hess_ref <- Deriv(fc_ref, "x", nderiv = 2)
    }
    
    x_plot <- c(xmin, xmin + ((xmax - xmin) / xpoint) * 1:xpoint)
    
    dev.off()
    par(mfrow=c(3, 1 + has_ref))
    
    plot(x = x_plot, y = fc(x_plot, ...), type = "o", main = "Loss function", xlab = "Pred - Label", ylab = "Loss")
    if (has_ref) {
      plot(x = x_plot, y = fc_ref(x_plot), type = "o", main = "Reference loss function", xlab = "Pred - Label", ylab = "Loss")
    }
    
    if (length(try(print(plot(x = x_plot, y = mini_func_grad(x_plot, ...), type = "o", main = "Computed Gradient: Success", xlab = "Pred - Label", ylab = "Loss")))) == 1) {print(plot(x = 0, y = 0, main = "Computed Gradient: Constant/Error", xlab = "Pred - Label", ylab = "Loss"))}
    if (has_ref) {
      if (length(try(print(plot(x = x_plot, y = mini_func_grad_ref(x_plot), type = "o", main = "Reference Gradient: Success", xlab = "Pred - Label", ylab = "Loss")))) == 1) {print(plot(x = 0, y = 0, main = "Reference Gradient: Constant/Error", xlab = "Pred - Label", ylab = "Loss"))}
    }
    
    if (length(try(print(plot(x = x_plot, y = mini_func_hess(x_plot, ...), type = "o", main = "Computed Hessian: Success", xlab = "Pred - Label", ylab = "Loss")))) == 1) {print(plot(x = 0, y = 0, main = "Computed Hessian: Constant/Error", xlab = "Pred - Label", ylab = "Loss"))}
    if (has_ref) {
      if (length(try(print(plot(x = x_plot, y = mini_func_hess_ref(x_plot), type = "o", main = "Reference Hessian: Success", xlab = "Pred - Label", ylab = "Loss")))) == 1) {print(plot(x = 0, y = 0, main = "Reference Hessian: Constant/Error", xlab = "Pred - Label", ylab = "Loss"))}
    }
    
  }
  
  if (verbose) {
    print(mini_func_grad)
    print(mini_func_hess)
  }
  
  return(list(grad = mini_func_grad, hess = mini_func_hess))
  
}
