#' Probability binary accuracy
#' 
#' This function allows to use a custom thresholding method to compute the binary accuracy.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param thresh Type: numeric. The cutoff (threshold) probability which is deemed positive when higher or equal, or negative when lower. Defaults to \code{0.5} (anything higher or equal to 0.5 is positive, anything lower is negative).
#' 
#' @return The accuracy at the provided threshold.
#' 
#' @export

prob.max_acc <- function(preds, labels, thresh = 0.5) {
  
  acc <- (sum(labels[preds >= thresh] == 1) + sum(labels[preds < thresh] == 0)) / length(labels)
  
  return(acc)
  
}