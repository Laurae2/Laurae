#' Probability Sensitivity (True Positive Rate)
#' 
#' This function allows to use a custom thresholding method to compute the Sensitivity (True Positive Rate).
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param thresh Type: numeric. The cutoff (threshold) probability which is deemed positive when higher or equal, or negative when lower. Defaults to \code{0.5} (anything higher or equal to 0.5 is positive, anything lower is negative).
#' 
#' @return The Sensitivity (True Positive Rate) at the provided threshold.
#' 
#' @export

prob.max_sensitivity <- function(preds, labels, thresh = 0.5) {
  
  positives <- as.logical(labels)
  counter <- sum(positives)
  tp <- as.numeric(sum(preds[positives] >= thresh))
  fn <- as.numeric(counter - tp)
  
  sens <- tp / (tp + fn)
  sens <- ifelse(!is.finite(sens), -1, sens)
  
  return(sens)
  
}