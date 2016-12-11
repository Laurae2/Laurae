#' Maximum Kappa statistic plotting
#' 
#' This function allows to use a custom thresholding method to maximize the Kappa statistic. A data.table of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' @param plots Type: numeric. Whether to plot the data immediately or not.
#' @param ... Other arguments to pass to \code{plot}.
#' 
#' @return A data.table containing the probabilities and their Kappa statistic.
#' 
#' @export

plotting.max_kappa <- function(preds, labels, plots = TRUE, ...) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(labels)
  numn <- length(labels) - nump
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, fn_v := numn - tn_v]
  DT[, tp_v := nump - fp_v]
  DT <- DT[cleaner, ]
  DT <- DT[, pObs := (tn_v + tp_v)]
  DT <- DT[, pExp := (((tn_v + fp_v) / pObs * (tn_v + fn_v) / pObs) + ((fn_v + tp_v) / pObs + (fp_v + tp_v) / pObs))]
  DT <- DT[, kappa := (pObs - pExp) / (1 - pExp)]
  DT <- DT[, kappa := ifelse(!is.finite(kappa), -1, kappa)]
  DT <- DT[kappa != -1]
  if (plots) {
    plot(x = DT[["y_prob"]], y = DT[["kappa"]], ...)
  }
  
  return(DT[, c("y_prob", "kappa")])
  
}