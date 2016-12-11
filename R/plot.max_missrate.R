#' Minimum Miss-Rate (False Negative Rate) plotting
#' 
#' This function allows to use a custom thresholding method to minimize the Miss-Rate (False Negative Rate). A data.table of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param plots Type: numeric. Whether to plot the data immediately or not.
#' @param ... Other arguments to pass to \code{plot}.
#' 
#' @return A data.table containing the probabilities and their Miss-Rate (False Negative Rate).
#' 
#' @export

plotting.max_missrate <- function(preds, labels, plots = TRUE, ...) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, miss := fn_v / (tp_v + fn_v)]
  DT[, miss := ifelse(!is.finite(miss), -1, miss)]
  DT <- DT[miss != -1]
  if (plots) {
    plot(x = DT[["y_prob"]], y = DT[["miss"]], ...)
  }
  
  return(DT[, c("y_prob", "miss")])
  
}