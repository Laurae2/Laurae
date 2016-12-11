#' Probability Kappa statistic
#' 
#' This function allows to use a custom thresholding method to compute the Kappa statistic.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param thresh Type: numeric. The cutoff (threshold) probability which is deemed positive when higher or equal, or negative when lower. Defaults to \code{0.5} (anything higher or equal to 0.5 is positive, anything lower is negative).
#' 
#' @return The Kappa statistic at the provided threshold.
#' 
#' @export

prob.max_kappa <- function(preds, labels, thresh = 0.5) {
  
  positives <- as.logical(labels)
  counter <- sum(positives)
  tp <- as.numeric(sum(preds[positives] >= thresh))
  fp <- as.numeric(sum(preds[!positives] >= thresh))
  tn <- as.numeric(length(labels) - counter - fp)
  fn <- as.numeric(counter - tp)
  
  kappa1 <- (tp + tn) / length(labels)
  kappa2 <- (((tp + fn) * (tp + fp)) + ((fp + tn) * (fn + tn))) / (length(labels) * length(labels))
  kappa <- (kappa1 - kappa2) / (1 - kappa2)
  kappa <- ifelse(!is.finite(kappa), -1, kappa)
  
  return(kappa)
  
}