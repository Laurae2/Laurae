#' Maximum Sensitivity (True Positive Rate) plotting
#' 
#' This function allows to use a custom thresholding method to maximize the Sensitivity (True Positive Rate). A data.table of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param plots Type: numeric. Whether to plot the data immediately or not.
#' @param ... Other arguments to pass to \code{plot}.
#' 
#' @return A data.table containing the probabilities and their Sensitivity (True Positive Rate).
#' 
#' @export

plotting.max_sensitivity <- function(preds, labels, plots = TRUE, ...) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, sens := tp_v / (tp_v + fn_v)]
  DT <- DT[is.finite(sens)]
  if (plots) {
    plot(x = DT[["y_prob"]], y = DT[["sens"]], ...)
  }
  
  return(DT[, c("y_prob", "sens")])
  
}