#' Minimum Fall-Out (False Positive Rate) plotting
#' 
#' This function allows to use a custom thresholding method to minimize the Fall-Out (False Positive Rate). A data.table of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param plots Type: numeric. Whether to plot the data immediately or not.
#' @param ... Other arguments to pass to \code{plot}.
#' 
#' @return A data.table containing the probabilities and their Fall-Out (False Positive Rate).
#' 
#' @export

plotting.max_fallout <- function(preds, labels, plots = TRUE, ...) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, fall := fp_v / (fp_v + tn_v)]
  DT[, fall := ifelse(!is.finite(fall), -1, fall)]
  DT <- DT[fall != -1]
  if (plots) {
    plot(x = DT[["y_prob"]], y = DT[["fall"]], ...)
  }
  
  return(DT[, c("y_prob", "fall")])
  
}