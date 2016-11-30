### Mean Squared Error

#' Mean Squared Error (symbolic function)
#' 
#' This function computes the Mean Squared Error loss (MSE) per value provided \code{x} (preds - labels).
#' 
#' @param x The difference between the \code{label} and the \code{prediction}.
#' 
#' @return The Squared Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_MSE_math, fc_ref)
#' }
#' 
#' @export
loss_MSE_math <- function(x) {
  x^2
}

#' Mean Squared Error (computation function)
#' 
#' This function computes the Mean Squared Error loss (MSE) per value provided \code{preds} and \code{labels}.
#' 
#' @param y_pred The predictions.
#' @param y_true The labels.
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
#' @param y_pred The predictions.
#' @param y_true The labels.
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
#' @param y_pred The predictions.
#' @param y_true The labels.
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
#' @param preds The predictions.
#' @param dtrain The labels.
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
#' @param x The difference between the \code{label} and the \code{prediction}.
#' 
#' @return The Squared Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_MAE_math, fc_ref)
#' }
#' 
#' @export
loss_MAE_math <- function(x) {
  abs(x)
}

#' Mean Absolute Error (computation function)
#' 
#' This function computes the Mean Absolute Error loss (MAE) per value provided \code{preds} and \code{labels}.
#' 
#' @param y_pred The predictions.
#' @param y_true The labels.
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
#' @param y_pred The predictions.
#' @param y_true The labels.
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
#' @param y_pred The predictions.
#' @param y_true The labels.
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
#' @param preds The predictions.
#' @param dtrain The labels.
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
#' @param x The difference between the \code{label} and the \code{prediction}.
#' 
#' @return The Cubic Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_MCE_math, fc_ref)
#' }
#' 
#' @export
loss_MCE_math <- function(x) {
  abs(x^3)
}

#' Mean Cubic Error (computation function)
#' 
#' This function computes the Mean Cubic Error loss (MCE) per value provided \code{preds} and \code{labels}.
#' 
#' @param y_pred The predictions.
#' @param y_true The labels.
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
#' @param y_pred The predictions.
#' @param y_true The labels.
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
#' @param y_pred The predictions.
#' @param y_true The labels.
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
#' @param preds The predictions.
#' @param dtrain The labels.
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
#' @param x The \code{prediction}.
#' @param y The \code{label}.
#' 
#' @return The Loglikelihood Error per value.
#' 
#' @examples
#' \dontrun{
#' SymbolicLoss(fc = loss_logloss_math, fc_ref, xmin = 0, xmax = 1, y = rep(0, 21))
#' }
#' 
#' @export
loss_logloss_math <- function(x, y) {
  - (y * log(x) + (1 - y) * log(1 - x))
}

#' Loglikelihood Error (computation function)
#' 
#' This function computes the Loglikelihood Error loss (logloss) per value provided \code{preds} and \code{labels} probabilities.
#' 
#' @param y_pred The predictions.
#' @param y_true The labels.
#' 
#' @return The Loglikelihood Error per value.
#' 
#' @export
loss_logloss <- function(y_pred, y_true) {
  - (y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

#' Loglikelihood Error (gradient function)
#' 
#' This function computes the Loglikelihood loss (logloss) gradient per value provided \code{preds} and \code{labels} probabilities.
#' 
#' @param y_pred The predictions.
#' @param y_true The labels.
#' 
#' @return The gradient of the Loglikelihood Error per value.
#' 
#' @export
loss_logloss_grad <- function(y_pred, y_true) {
  grad <- (1 - y_true)/(1 - y_pred) - y_true/y_pred
  return(grad)
}

#' Loglikelihood Error (hessian function)
#' 
#' This function computes the MLoglikelihood Error loss (logloss) hessian per value provided \code{preds} and \code{labels} probabilities.
#' 
#' @param y_pred The predictions.
#' @param y_true The labels.
#' 
#' @return The hessian of the Loglikelihood Error per value.
#' 
#' @export
loss_logloss_hess <- function(y_pred, y_true) {
  hess <- (1 - y_true)/((1 - y_pred) * (1 - y_pred)) + y_true/(y_pred * y_pred)
  return(hess)
}

#' Loglikelihood Error (xgboost function)
#' 
#' This function computes for xgboost's \code{obj} function the Loglikelihood Error loss (logloss) gradient and hessian per value provided \code{preds} and \code{dtrain}.
#' 
#' @param preds The predictions.
#' @param dtrain The labels.
#' 
#' @return The gradient and the hessian of the Loglikelihood Error per value in a list.
#' 
#' @export
loss_logloss_xgb <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  x <- 1/(1 + exp(-preds)) #likelihood transformation first in xgboost
  grad <- x - labels
  hess <- x * (1 - x)
  return(list(grad = grad, hess = hess))
}



