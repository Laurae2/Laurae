#' Fast Logarithmic Loss (logloss) computation
#' 
#' This function computes the Logarithmic Lost fast.
#'  
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param eps Type: numeric. The shrinkage between the 0 and 1's bounds.
#' 
#' @return The Logarithmic Loss.
#' 
#' @export

LogLoss <- function(preds, labels, eps = 1e-15) {
  predicted <- pmin(pmax(preds, eps), 1 - eps)
  -1/length(labels) * (sum(labels * log(preds) + (1 - labels) * log(1 - preds)))
}