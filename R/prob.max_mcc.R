#' Probability Matthews Correlation Coefficient
#' 
#' This function allows to use a custom thresholding method to compute the Matthews Correlation Coefficient.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param thresh Type: numeric. The cutoff (threshold) probability which is deemed positive when higher or equal, or negative when lower. Defaults to \code{0.5} (anything higher or equal to 0.5 is positive, anything lower is negative).
#' 
#' @return The Matthews Correlation Coefficient at the provided threshold.
#' 
#' @export

prob.max_mcc <- function(preds, labels, thresh = 0.5) {
  
  positives <- as.logical(labels)
  counter <- sum(positives)
  tp <- as.numeric(sum(preds[positives] >= thresh))
  fp <- as.numeric(sum(preds[!positives] >= thresh))
  tn <- as.numeric(length(labels) - counter - fp)
  fn <- as.numeric(counter - tp)
  
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc <- ifelse(!is.finite(mcc), -1, mcc)
  
  return(mcc)
  
}