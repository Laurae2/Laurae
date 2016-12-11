#' Probability Specificity (True Negative Rate)
#' 
#' This function allows to use a custom thresholding method to compute the Specificity (True Negative Rate).
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param thresh Type: numeric. The cutoff (threshold) probability which is deemed positive when higher or equal, or negative when lower. Defaults to \code{0.5} (anything higher or equal to 0.5 is positive, anything lower is negative).
#' 
#' @return The Specificity (True Negative Rate) at the provided threshold.
#' 
#' @export

prob.max_specificity <- function(preds, labels, thresh = 0.5) {
  
  positives <- as.logical(labels)
  counter <- sum(positives)
  fp <- as.numeric(sum(preds[!positives] >= thresh))
  tn <- as.numeric(length(labels) - counter - fp)
  
  spec <- tn / (tn + fp)
  spec <- ifelse(!is.finite(spec), -1, spec)
  
  return(spec)
  
}