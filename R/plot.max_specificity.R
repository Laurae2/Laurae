#' Maximum Specificity (True Negative Rate) plotting
#' 
#' This function allows to use a custom thresholding method to maximize the Specificity (True Negative Rate). A data.table of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param plots Type: numeric. Whether to plot the data immediately or not.
#' @param ... Other arguments to pass to \code{plot}.
#' 
#' @return A data.table containing the probabilities and their Specificity (True Negative Rate).
#' 
#' @export

plotting.max_specificity <- function(preds, labels, plots = TRUE, ...) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, spec := tn_v / (tn_v + fp_v)]
  DT <- DT[is.finite(spec)]
  if (plots) {
    plot(x = DT[["y_prob"]], y = DT[["spec"]], ...)
  }
  
  return(DT[, c("y_prob", "spec")])
  
}