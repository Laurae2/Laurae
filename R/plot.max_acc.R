#' Maximum binary accuracy plotting
#' 
#' This function allows to use a custom thresholding method to maximize the binary accuracy. A data.table of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param plots Type: numeric. Whether to plot the data immediately or not.
#' @param ... Other arguments to pass to \code{plot}.
#' 
#' @return A data.table containing the probabilities and their accuracy.
#' 
#' @export

plotting.max_acc <- function(preds, labels, plots = TRUE, ...) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  lens <- length(labels)
  nump <- sum(labels)
  
  DT[, tn_v := cumsum(y_true == 0)]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, acc := (tn_v + tp_v) / lens]
  DT <- DT[is.finite(acc)]
  if (plots) {
    plot(x = DT[["y_prob"]], y = DT[["acc"]], ...)
  }
  
  return(DT[, c("y_prob", "acc")])
  
}