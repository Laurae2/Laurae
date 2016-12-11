#' Minimum Fall-Out (False Positive Rate)
#' 
#' This function allows to use a custom thresholding method to minimize the Fall-Out (False Positive Rate). A pair of values is returned.
#' 
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return A two element vector containing the minimum Fall-Out (False Positive Rate) for binary data, and the threshold used.
#' 
#' @export

get.max_fallout <- function(preds, labels) {
  
  DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  
  DT[, tn_v := as.numeric(cumsum(y_true == 0))]
  DT[, fp_v := cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, fall := fp_v / (fp_v + tn_v)]
  
  best_row <- which.min(DT$fall)
  
  if (length(best_row) > 0) {
    return(c(DT$fall[best_row[1]], DT$y_prob[best_row[1]]))
  } else {
    return(c(-1, -1))
  }
  
}