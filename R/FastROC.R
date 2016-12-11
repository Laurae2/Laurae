#' Fast AUROC (AUC, ROC) computation
#' 
#' This function computes the AUROC fast.
#'  
#' @param preds Type: numeric. The predictions.
#' @param labels Type: numeric. The labels (0, 1).
#' 
#' @return The AUC.
#' 
#' @export

FastROC <- function(preds, labels) {
  
  x1 = as.numeric(preds[labels == 1])
  n1 = as.numeric(length(x1))
  x2 = as.numeric(preds[labels == 0])
  n2 = as.numeric(length(x2))
  r = rank(c(x1,x2))
  return((sum(r[1:n1]) - n1 * (n1 + 1) / 2) / (n1 * n2))
  
}