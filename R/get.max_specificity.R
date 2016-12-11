#' Maximum Specificity (True Negative Rate)
#' 
#' This function allows to use a custom thresholding method to maximize the Specificity (True Negative Rate). A pair of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return A two element vector containing the maximum Specificity (True Negative Rate) for binary data, and the threshold used.
#' 
#' @export

get.max_specificity <- function(preds, labels) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, spec := tn_v / (tn_v + fp_v)]
  
  best_row <- which.max(DT$spec)
  
  if (length(best_row) > 0) {
    return(c(DT$spec[best_row[1]], DT$y_prob[best_row[1]]))
  } else {
    return(c(-1, -1))
  }
  
}