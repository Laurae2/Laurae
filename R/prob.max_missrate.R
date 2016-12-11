#' Probability Miss-Rate (False Negative Rate)
#' 
#' This function allows to use a custom thresholding method to compute the Miss-Rate (False Negative Rate).
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param thresh Type: numeric. The cutoff (threshold) probability which is deemed positive when higher or equal, or negative when lower. Defaults to \code{0.5} (anything higher or equal to 0.5 is positive, anything lower is negative).
#' 
#' @return The Miss-Rate (False Negative Rate) at the provided threshold.
#' 
#' @export

prob.max_missrate <- function(preds, labels, thresh = 0.5) {
  
  positives <- as.logical(labels)
  counter <- sum(positives)
  tp <- as.numeric(sum(preds[positives] >= thresh))
  fn <- as.numeric(counter - tp)
  
  miss <- fn / (tp + fn)
  miss <- ifelse(!is.finite(miss), -1, miss)
  
  return(miss)
  
}