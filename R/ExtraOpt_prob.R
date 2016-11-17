#' Cross-Entropy -based Hybrid Optimization Helper (probability)
#'
#' This function is a demonstration helper of the probability function for the Cross-Entropy based hybrid optimization.
#' 
#' This function takes a matrix and returns the prediction from the prior estimator model. The example uses xgboost. Make sure the columns are matching correctly. For convenience, continuous and discrete variables are separated.
#' 
#' @param x Label as the 1st element, all parameters on 2nd to last element.
#' 
#' @return The probability.
#' 
#' @export

.ExtraOpt_prob <- function(x, y, model) {
  
  dpred <- xgb.DMatrix(data = t(as.matrix(c(x, y))), missing = NA)
  predicted <- predict(model, dpred)
  return(predicted)
  
}
