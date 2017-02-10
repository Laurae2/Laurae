#' Partial Dependency, xgboost predictor
#'
#' This function is a helper for partial dependency plots using an xgboost model. Use this as an example for the \code{predictor} argument.
#' 
#' @param model Type: unknown. The xgboost trained model.
#' @param data Type: data.table (mandatory). The data we need to use to sample from for the partial dependency with \code{observation}.
#' 
#' @return A vector of predicted values matching the right order of input.
#' 
#' @export

predictor_xgb <- function(model, data) {
  
  temp_data <- xgb.DMatrix(data = Laurae::DT2mat(data))
  return(predict(model, temp_data))
  
}