#' Probability Precision (Positive Predictive Value)
#' 
#' This function allows to use a custom thresholding method to compute the Precision (Positive Predictive Value).
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param thresh Type: numeric. The cutoff (threshold) probability which is deemed positive when higher or equal, or negative when lower. Defaults to \code{0.5} (anything higher or equal to 0.5 is positive, anything lower is negative).
#' 
#' @return The Precision (Positive Predictive Value) at the provided threshold.
#' 
#' @export

prob.max_precision <- function(preds, labels, thresh = 0.5) {
  
  positives <- as.logical(labels)
  tp <- as.numeric(sum(preds[positives] >= thresh))
  fp <- as.numeric(sum(preds[!positives] >= thresh))
  
  prec <- tp / (tp + fp)
  prec <- ifelse(!is.finite(prec), -1, prec)
  
  return(prec)
  
}