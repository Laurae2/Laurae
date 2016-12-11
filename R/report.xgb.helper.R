#' Extreme Gradient Boosting HTML report helper function
#'
#' This function is used to train a model using xgboost in the automated HTML report creator. It must handle three arguments: \code{train}, \code{valid}, and \code{params}.
#' 
#' @param train Type: xgb.DMatrix. The labelled training data.
#' @param valid Type: xgb.DMatrix. The labelled testing data.
#' @param params Type: list. The parameters as a list.
#' @param nrounds Type: numeric. The number of training iterations.
#' @param model Type: xgb.Booster. The xgboost model to train from.
#' 
#' @return A xgboost model.
#' 
#' @examples
#' # No example.
#' \dontrun{
#' }
#' 
#' @export

report.xgb.helper <- function(train, valid, params, nrounds = 1000000, model = NULL) {
  
  if (is.null(model)) {
    set.seed(0)
    xgb_model <- xgb.train(data = train,
                           params = params,
                           nrounds = nrounds,
                           watchlist = list(val = valid),
                           verbose = FALSE,
                           early_stopping_rounds = 25,
                           callbacks = list(cb.evaluation.log()))
  } else {
    set.seed(0)
    xgb_model <- xgb.train(data = train,
                           params = params,
                           nrounds = nrounds,
                           watchlist = list(val = valid),
                           verbose = FALSE,
                           xgb_model = model)
    
  }
  return(xgb_model)
  
}
