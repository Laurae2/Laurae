#' Probability F1 Score (Precision with Sensitivity harmonic mean)
#' 
#' This function allows to use a custom thresholding method to compute the F1 Score (Precision with Sensitivity harmonic mean).
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param thresh Type: numeric. The cutoff (threshold) probability which is deemed positive when higher or equal, or negative when lower. Defaults to \code{0.5} (anything higher or equal to 0.5 is positive, anything lower is negative).
#' 
#' @return The F1 Score (Precision with Sensitivity harmonic mean) at the provided threshold.
#' 
#' @export

prob.max_f1 <- function(preds, labels, thresh = 0.5) {
  
  positives <- as.logical(labels)
  counter <- sum(positives)
  tp <- as.numeric(sum(preds[positives] >= thresh))
  fp <- as.numeric(sum(preds[!positives] >= thresh))
  fn <- as.numeric(counter - tp)
  
  f1s <- 2 * tp / (2 * tp + fp + fn)
  f1s <- ifelse(!is.finite(f1s), -1, f1s)
  
  return(f1s)
  
}