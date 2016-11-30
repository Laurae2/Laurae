### Mean Squared Error

#' Mean Squared Error (symbolic function)
#' 
#' This function computes the Mean Squared Error loss (MSE) per value provided \code{x} (preds - labels).
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{x^2}
#' 
#' Gradient Formula : \eqn{2 * x}
#' 
#' Hessian Formula : \eqn{2}
#' 
#' @param x The \code{predictions}.
#' @param y The \code{labels}.
#' 
#' @return The Squared Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_MSE_math)
#' }
#' 
#' @export
loss_MSE_math <- function(x, y) {
  (x - y)^2
}

#' Mean Squared Error (computation function)
#' 
#' This function computes the Mean Squared Error loss (MSE) per value provided \code{preds} and \code{labels}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{x^2}
#' 
#' Gradient Formula : \eqn{2 * x}
#' 
#' Hessian Formula : \eqn{2}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The Squared Error per value.
#' 
#' @export
loss_MSE <- function(y_pred, y_true) {
  x <- y_pred - y_true
  x * x
}

#' Mean Squared Error (gradient function)
#' 
#' This function computes the Mean Squared Error loss (MSE) gradient per value provided \code{preds} and \code{labels}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{x^2}
#' 
#' Gradient Formula : \eqn{2 * x}
#' 
#' Hessian Formula : \eqn{2}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The gradient of the Squared Error per value.
#' 
#' @export
loss_MSE_grad <- function(y_pred, y_true) {
  x <- y_pred - y_true
  grad <- 2 * x
  return(grad)
}

#' Mean Squared Error (hessian function)
#' 
#' This function computes the Mean Squared Error loss (MSE) hessian per value provided \code{preds} and \code{labels}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{x^2}
#' 
#' Gradient Formula : \eqn{2 * x}
#' 
#' Hessian Formula : \eqn{2}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The hessian of the Squared Error per value.
#' 
#' @export
loss_MSE_hess <- function(y_pred, y_true) {
  hess <- rep(2, length(y_pred))
  return(hess)
}


#' Mean Squared Error (xgboost function)
#' 
#' This function computes for xgboost's \code{obj} function the Mean Squared Error loss (MSE) gradient and hessian per value provided \code{preds} and \code{dtrain}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{x^2}
#' 
#' Gradient Formula : \eqn{2 * x}
#' 
#' Hessian Formula : \eqn{2}
#' 
#' @param preds The \code{predictions}.
#' @param dtrain The xgboost model.
#' 
#' @return The gradient and the hessian of the Squared Error per value in a list.
#' 
#' @export
loss_MSE_xgb <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  x <- preds - labels
  grad <- 2 * x
  hess <- 2
  return(list(grad = grad, hess = hess))
}



### Mean Absolute Error

#' Mean Absolute Error (symbolic function)
#' 
#' This function computes the Mean Absolute Error loss (MAE) per value provided \code{x} (preds - labels).
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x)}
#' 
#' Gradient Formula : \eqn{sign(x)}
#' 
#' Hessian Formula : \eqn{0}
#' 
#' @param x The \code{predictions}.
#' @param y The \code{labels}.
#' 
#' @return The Squared Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_MAE_math)
#' }
#' 
#' @export
loss_MAE_math <- function(x, y) {
  abs(x - y)
}

#' Mean Absolute Error (computation function)
#' 
#' This function computes the Mean Absolute Error loss (MAE) per value provided \code{preds} and \code{labels}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x)}
#' 
#' Gradient Formula : \eqn{sign(x)}
#' 
#' Hessian Formula : \eqn{0}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The Squared Error per value.
#' 
#' @export
loss_MAE <- function(y_pred, y_true) {
  x <- y_pred - y_true
  abs(x)
}

#' Mean Absolute Error (gradient function)
#' 
#' This function computes the Mean Absolute Error loss (MAE) gradient per value provided \code{preds} and \code{labels}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x)}
#' 
#' Gradient Formula : \eqn{sign(x)}
#' 
#' Hessian Formula : \eqn{0}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The gradient of the Absolute Error per value.
#' 
#' @export
loss_MAE_grad <- function(y_pred, y_true) {
  x <- y_pred - y_true
  grad <- sign(x)
  return(grad)
}

#' Mean Absolute Error (hessian function)
#' 
#' This function computes the Mean Absolute Error loss (MAE) hessian per value provided \code{preds} and \code{labels}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x)}
#' 
#' Gradient Formula : \eqn{sign(x)}
#' 
#' Hessian Formula : \eqn{0}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The hessian of the Absolute Error per value.
#' 
#' @export
loss_MAE_hess <- function(y_pred, y_true) {
  hess <- rep(0, length(y_pred))
  return(hess)
}

#' Mean Absolute Error (xgboost function)
#' 
#' This function computes for xgboost's \code{obj} function the Mean Absolute Error loss (MAE) gradient and hessian per value provided \code{preds} and \code{dtrain}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x)}
#' 
#' Gradient Formula : \eqn{sign(x)}
#' 
#' Hessian Formula : \eqn{0}
#' 
#' @param preds The \code{predictions}.
#' @param dtrain The xgboost model.
#' 
#' @return The gradient and the hessian of the Absolute Error per value in a list.
#' 
#' @export
loss_MAE_xgb <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  x <- preds - labels
  grad <- sign(x)
  hess <- rep(0, length(grad))
  return(list(grad = grad, hess = hess))
}



### Mean Cubic Error

#' Mean Cubic Error (math function)
#' 
#' This function computes the Mean Cubic Error loss (MCE) per value provided \code{x} (preds - labels).
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x^3)}
#' 
#' Gradient Formula : \eqn{3 * (x * abs(x))}
#' 
#' Hessian Formula : \eqn{(6 * x * x) / abs(x)}
#' 
#' @param x The \code{predictions}.
#' @param y The \code{labels}.
#' 
#' @return The Cubic Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_MCE_math)
#' }
#' 
#' @export
loss_MCE_math <- function(x, y) {
  abs((x - y)^3)
}

#' Mean Cubic Error (computation function)
#' 
#' This function computes the Mean Cubic Error loss (MCE) per value provided \code{preds} and \code{labels}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x^3)}
#' 
#' Gradient Formula : \eqn{3 * (x * abs(x))}
#' 
#' Hessian Formula : \eqn{(6 * x * x) / abs(x)}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The Cubic Error per value.
#' 
#' @export
loss_MCE <- function(y_pred, y_true) {
  x <- y_pred - y_true
  abs(x * x * x)
}

#' Mean Cubic Error (gradient function)
#' 
#' This function computes the Mean Cubic Error loss (MCE) gradient per value provided \code{preds} and \code{labels}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x^3)}
#' 
#' Gradient Formula : \eqn{3 * (x * abs(x))}
#' 
#' Hessian Formula : \eqn{(6 * x * x) / abs(x)}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The gradient of the Cubic Error per value.
#' 
#' @export
loss_MCE_grad <- function(y_pred, y_true) {
  x <- y_pred - y_true
  grad <- 3 * (x * abs(x))
  return(grad)
}

#' Mean Cubic Error (hessian function)
#' 
#' This function computes the Mean Cubic Error loss (MCE) hessian per value provided \code{preds} and \code{labels}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x^3)}
#' 
#' Gradient Formula : \eqn{3 * (x * abs(x))}
#' 
#' Hessian Formula : \eqn{(6 * x * x) / abs(x)}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The hessian of the Cubic Error per value.
#' 
#' @export
loss_MCE_hess <- function(y_pred, y_true) {
  x <- y_pred - y_true
  hess <- (6 * x * x) / abs(x)
  return(hess)
}

#' Mean Cubic Error (xgboost function)
#' 
#' This function computes for xgboost's \code{obj} function the Mean Cubic Error loss (MCE) gradient and hessian per value provided \code{preds} and \code{dtrain}.
#' 
#' Supposing: \eqn{x = preds - labels}
#' 
#' Loss Formula : \eqn{abs(x^3)}
#' 
#' Gradient Formula : \eqn{3 * (x * abs(x))}
#' 
#' Hessian Formula : \eqn{(6 * x * x) / abs(x)}
#' 
#' @param preds The \code{predictions}.
#' @param dtrain The xgboost model.
#' 
#' @return The gradient and the hessian of the Cubic Error per value in a list.
#' 
#' @export
loss_MCE_xgb <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  x <- preds - labels
  grad <- 3 * (x * abs(x))
  hess <- (6 * x * x) / abs(x)
  return(list(grad = grad, hess = hess))
}



### Loglikelihood Error

#' Loglikelihood Error (math function)
#' 
#' This function computes the Loglikelihood Error loss (logloss) per value provided \code{x, y} (preds, labels) probabilities.
#' 
#' Loss Formula : \eqn{- (y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))}
#' 
#' Gradient Formula : \eqn{(1 - y_true)/(1 - y_pred) - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{(1 - y_true)/((1 - y_pred) * (1 - y_pred)) + y_true/(y_pred * y_pred)}
#' 
#' @param x The \code{predictions}.
#' @param y The \code{labels}.
#' 
#' @return The Loglikelihood Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_LL_math, xmin = 0, xmax = 1, y = rep(0, 21))
#' }
#' 
#' @export
loss_LL_math <- function(x, y) {
  - (y * log(x) + (1 - y) * log(1 - x))
}

#' Loglikelihood Error (computation function)
#' 
#' This function computes the Loglikelihood Error loss (logloss) per value provided \code{preds} and \code{labels} probabilities.
#' 
#' Loss Formula : \eqn{- (y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))}
#' 
#' Gradient Formula : \eqn{(1 - y_true)/(1 - y_pred) - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{(1 - y_true)/((1 - y_pred) * (1 - y_pred)) + y_true/(y_pred * y_pred)}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The Loglikelihood Error per value.
#' 
#' @export
loss_LL <- function(y_pred, y_true) {
  - (y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

#' Loglikelihood Error (gradient function)
#' 
#' This function computes the Loglikelihood loss (logloss) gradient per value provided \code{preds} and \code{labels} probabilities.
#' 
#' Loss Formula : \eqn{- (y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))}
#' 
#' Gradient Formula : \eqn{(1 - y_true)/(1 - y_pred) - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{(1 - y_true)/((1 - y_pred) * (1 - y_pred)) + y_true/(y_pred * y_pred)}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The gradient of the Loglikelihood Error per value.
#' 
#' @export
loss_LL_grad <- function(y_pred, y_true) {
  grad <- (1 - y_true)/(1 - y_pred) - y_true/y_pred
  return(grad)
}

#' Loglikelihood Error (hessian function)
#' 
#' This function computes the Loglikelihood Error loss (logloss) hessian per value provided \code{preds} and \code{labels} probabilities.
#' 
#' Loss Formula : \eqn{- (y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))}
#' 
#' Gradient Formula : \eqn{(1 - y_true)/(1 - y_pred) - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{(1 - y_true)/((1 - y_pred) * (1 - y_pred)) + y_true/(y_pred * y_pred)}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The hessian of the Loglikelihood Error per value.
#' 
#' @export
loss_LL_hess <- function(y_pred, y_true) {
  hess <- (1 - y_true)/((1 - y_pred) * (1 - y_pred)) + y_true/(y_pred * y_pred)
  return(hess)
}

#' Loglikelihood Error (xgboost function)
#' 
#' This function computes for xgboost's \code{obj} function the Loglikelihood Error loss (logloss) gradient and hessian per value provided \code{preds} and \code{dtrain}.
#' 
#' Loss Formula : \eqn{- (y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))}
#' 
#' Gradient Formula : \eqn{(1 - y_true)/(1 - y_pred) - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{(1 - y_true)/((1 - y_pred) * (1 - y_pred)) + y_true/(y_pred * y_pred)}
#' 
#' @param preds The \code{predictions}.
#' @param dtrain The xgboost model.
#' 
#' @return The gradient and the hessian of the Loglikelihood Error per value in a list.
#' 
#' @export
loss_LL_xgb <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  x <- 1/(1 + exp(-preds)) #likelihood transformation first in xgboost
  grad <- x - labels
  hess <- x * (1 - x)
  return(list(grad = grad, hess = hess))
}



### Laurae's Poisson Error

#' Laurae's Poisson Error (math function)
#' 
#' This function computes the Laurae's Poisson Error loss per value provided \code{x, y} (preds, labels) counts.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. The negative values are cancelled out to make the loss function positive, with \code{loss = 0} when \code{y_true = y_pred}. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_pred - y_true * log(y_pred))}
#' 
#' Gradient Formula : \eqn{1 - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{y_true/(y_pred * y_pred)}
#' 
#' @param x The \code{predictions}.
#' @param y The \code{labels}.
#' 
#' @return The Laurae's Poisson Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_poisson_math, xmin = 1, xmax = 50, y = rep(10, 21))
#' }
#' 
#' @export
loss_Poisson_math <- function(x, y) {
  (x - y * log(x)) + (y * log(y) - y)
}

#' Laurae's Poisson Error (computation function)
#' 
#' This function computes the Laurae's Poisson Error loss per value provided \code{preds} and \code{labels} count.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. The negative values are cancelled out to make the loss function positive, with \code{loss = 0} when \code{y_true = y_pred}. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_pred - y_true * log(y_pred))}
#' 
#' Gradient Formula : \eqn{1 - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{y_true/(y_pred * y_pred)}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The Laurae's Poisson Error per value.
#' 
#' @export
loss_Poisson <- function(y_pred, y_true) {
  -(y_pred - y_true * log(y_pred)) + (y_true * log(y_true) - y_true)
}

#' Laurae's Poisson Error (gradient function)
#' 
#' This function computes the Laurae's Poisson loss gradient per value provided \code{preds} and \code{labels} count.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. The negative values are cancelled out to make the loss function positive, with \code{loss = 0} when \code{y_true = y_pred}. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_pred - y_true * log(y_pred))}
#' 
#' Gradient Formula : \eqn{1 - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{y_true/(y_pred * y_pred)}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The gradient of the Laurae's Poisson Error per value.
#' 
#' @export
loss_Poisson_grad <- function(y_pred, y_true) {
  grad <- 1 - y_true/y_pred
  return(grad)
}

#' Laurae's Poisson Error (hessian function)
#' 
#' This function computes the Laurae's Poisson Error loss hessian per value provided \code{preds} and \code{labels} count.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. The negative values are cancelled out to make the loss function positive, with \code{loss = 0} when \code{y_true = y_pred}. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_pred - y_true * log(y_pred))}
#' 
#' Gradient Formula : \eqn{1 - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{y_true/(y_pred * y_pred)}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The hessian of the Laurae's Poisson Error per value.
#' 
#' @export
loss_Poisson_hess <- function(y_pred, y_true) {
  hess <- y_true/(y_pred * y_pred)
  return(hess)
}

#' Laurae's Poisson Error (xgboost function)
#' 
#' This function computes for xgboost's \code{obj} function the Laurae's Poisson Error loss gradient and hessian per value provided \code{preds} and \code{dtrain}. Negative and null values are set to \code{1e-15}.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. The negative values are cancelled out to make the loss function positive, with \code{loss = 0} when \code{y_true = y_pred}. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_pred - y_true * log(y_pred))}
#' 
#' Gradient Formula : \eqn{1 - y_true/y_pred}
#' 
#' Hessian Formula : \eqn{y_true/(y_pred * y_pred)}
#' 
#' @param preds The \code{predictions}.
#' @param dtrain The xgboost model.
#' 
#' @return The gradient and the hessian of the Laurae's Poisson Error per value in a list.
#' 
#' @export
loss_Poisson_xgb <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds[preds <= 0] <- 1e-15
  x <- labels/preds #poisson transformation
  grad <- 1 - x
  hess <- x/preds
  return(list(grad = grad, hess = hess))
}



### Laurae's Kullback-Leibler Error

#' Laurae's Kullback-Leibler Error (math function)
#' 
#' This function computes the Laurae's Kullback-Leibler Error loss per value provided \code{x, y} (preds, labels) values.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. Compared to Laurae's Poisson loss function, Laurae's Kullback-Leibler loss has much higher loss. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_true - y_pred) * log(y_true / y_pred)}
#' 
#' Gradient Formula : \eqn{-((y_true - y_pred)/y_pred + log(y_true) - log(y_pred))}
#' 
#' Hessian Formula : \eqn{((y_true - y_pred)/y_pred + 2)/y_pred}
#' 
#' @param x The \code{predictions}.
#' @param y The \code{label}.
#' 
#' @return The Laurae's Kullback-Leibler Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_LKL_math, fc_ref = loss_MSE_math, xmin = 1, xmax = 100, y = rep(30, 21))
#' SymbolicLoss(fc = loss_LKL_math, fc_ref = loss_Poisson_math, xmin = 1, xmax = 100, y = rep(30, 21))
#' }
#' 
#' @export
loss_LKL_math <- function(x, y) {
  (y - x) * log(y / x)
}

#' Laurae's Kullback-Leibler Error (computation function)
#' 
#' This function computes the Laurae's Kullback-Leibler Error loss per value provided \code{preds} and \code{labels} values.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. Compared to Laurae's Poisson loss function, Laurae's Kullback-Leibler loss has much higher loss. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_true - y_pred) * log(y_true / y_pred)}
#' 
#' Gradient Formula : \eqn{-((y_true - y_pred)/y_pred + log(y_true) - log(y_pred))}
#' 
#' Hessian Formula : \eqn{((y_true - y_pred)/y_pred + 2)/y_pred}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The Laurae's Kullback-Leibler Error per value.
#' 
#' @export
loss_LKL <- function(y_pred, y_true) {
  (y_true - y_pred) * log(y_true / y_pred)
}

#' Laurae's Kullback-Leibler Error (gradient function)
#' 
#' This function computes the Laurae's Kullback-Leibler loss gradient per value provided \code{preds} and \code{labels} values.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. Compared to Laurae's Poisson loss function, Laurae's Kullback-Leibler loss has much higher loss. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_true - y_pred) * log(y_true / y_pred)}
#' 
#' Gradient Formula : \eqn{-((y_true - y_pred)/y_pred + log(y_true) - log(y_pred))}
#' 
#' Hessian Formula : \eqn{((y_true - y_pred)/y_pred + 2)/y_pred}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The gradient of the Laurae's Kullback-Leibler Error per value.
#' 
#' @export
loss_LKL_grad <- function(y_pred, y_true) {
  grad <- -((y_true - y_pred)/y_pred + log(y_true) - log(y_pred))
  return(grad)
}

#' Laurae's Kullback-Leibler Error (hessian function)
#' 
#' This function computes the Laurae's Kullback-Leibler Error loss hessian per value provided \code{preds} and \code{labels} values.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. Compared to Laurae's Poisson loss function, Laurae's Kullback-Leibler loss has much higher loss. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_true - y_pred) * log(y_true / y_pred)}
#' 
#' Gradient Formula : \eqn{-((y_true - y_pred)/y_pred + log(y_true) - log(y_pred))}
#' 
#' Hessian Formula : \eqn{((y_true - y_pred)/y_pred + 2)/y_pred}
#' 
#' @param y_pred The \code{predictions}.
#' @param y_true The \code{labels}.
#' 
#' @return The hessian of the Laurae's Kullback-Leibler Error per value.
#' 
#' @export
loss_LKL_hess <- function(y_pred, y_true) {
  hess <- ((y_true - y_pred)/y_pred + 2)/y_pred
  return(hess)
}

#' Laurae's Kullback-Leibler Error (xgboost function)
#' 
#' This function computes for xgboost's \code{obj} function the Laurae's Kullback-Leibler Error loss gradient and hessian per value provided \code{preds} and \code{dtrain}.
#' 
#' This loss function is strictly positive, therefore defined in \code{\]0, +Inf\[}. It penalizes lower values more heavily, and as such is a good fit for typical problems requiring fine tuning when undercommitting on the predictions. Compared to Laurae's Poisson loss function, Laurae's Kullback-Leibler loss has much higher loss. Negative and null values are set to \code{1e-15}. This loss function is experimental.
#' 
#' Loss Formula : \eqn{(y_true - y_pred) * log(y_true / y_pred)}
#' 
#' Gradient Formula : \eqn{-((y_true - y_pred)/y_pred + log(y_true) - log(y_pred))}
#' 
#' Hessian Formula : \eqn{((y_true - y_pred)/y_pred + 2)/y_pred}
#' 
#' @param preds The \code{predictions}.
#' @param dtrain The xgboost model.
#' 
#' @return The gradient and the hessian of the Laurae's Kullback-Leibler Error per value in a list.
#' 
#' @export
loss_LKL_xgb <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds[preds <= 0] <- 1e-15
  x <- (labels - preds)/preds #quantile transformation
  grad <- -(x + log(labels) - log(preds))
  hess <- (x + 2)/preds
  return(list(grad = grad, hess = hess))
}





