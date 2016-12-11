#' Maximum Precision (Positive Predictive Value) plotting
#' 
#' This function allows to use a custom thresholding method to maximize the Precision (Positive Predictive Value). A data.table of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param plots Type: numeric. Whether to plot the data immediately or not.
#' @param ... Other arguments to pass to \code{plot}.
#' 
#' @return A data.table containing the probabilities and their Precision (Positive Predictive Value).
#' 
#' @export

plotting.max_precision <- function(preds, labels, plots = TRUE, ...) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(y_true)
  
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, tp_v := nump - fp_v]
  DT <- DT[cleaner, ]
  DT[, prec := tp_v / (tp_v + fp_v)]
  DT <- DT[is.finite(prec)]
  if (plots) {
    plot(x = DT[["y_prob"]], y = DT[["prec"]], ...)
  }
  
  return(DT[, c("y_prob", "prec")])
  
}