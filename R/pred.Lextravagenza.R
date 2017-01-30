#' Laurae's Extravagenza machine learning model prediction
#'
#' This function predicts from data using a trained Extravagenza machine learning model. This does not work on multiclass problems.
#' 
#' @param model Type: list from \code{Lextravgenza}. The trained model.
#' @param data Type: xgb.DMatrix. The data to predict on.
#' @param nrounds Type: integer. The number of boosting iterations to predict from. Defaults to \code{model$best_iter}, which is the best iteration reported during \code{Lextravagenza} boosting.
#' 
#' @return A prediction vector.
#' 
#' @examples
#' \dontrun{
#' library(Laurae)
#' library(xgboost)
#' data(agaricus.train, package='xgboost')
#' data(agaricus.test, package='xgboost')
#' dtrain <- xgb.DMatrix(agaricus.train$data[1:5000, ], label = agaricus.train$label[1:5000])
#' dval <- xgb.DMatrix(agaricus.train$data[5001:6513, ], label = agaricus.train$label[5001:6513])
#' dtest <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)
#' Lex_model <- Lextravagenza(train = dtrain, # Train data
#'                            valid = dval, # Validation data = depth tuner
#'                            test = dtest, # Test data = early stopper
#'                            maximize = FALSE, # Not maximizing RMSE
#'                            personal_rounds = 50, # Boosting for 50 iterations
#'                            personal_depth = 1:8, # Dynamic depth between 1 and 8
#'                            personal_eta = 0.40, # Shrinkage of boosting to 0.40
#'                            auto_stop = 5, # Early stopping of 5 iterations
#'                            base_margin = 0.5, # Start with 0.5 probabilities
#'                            seed = 0, # Random seed
#'                            nthread = 1, # 1 thread for training
#'                            eta = 0.40, # xgboost shrinkage of 0.40 (avoid fast overfit)
#'                            booster = "gbtree", # train trees, can't work with GLM
#'                            objective = "binary:logistic", # classification, binary
#'                            eval_metric = "rmse" # RMSE metric to optimize
#' )
#' 
#' str(Lex_model, max.level = 1) # Get list of the model structure
#' 
#' predictedValues <- pred.Lextravagenza(Lex_model, dtest, nrounds = Lex_model$best_iter)
#' all.equal(sqrt(mean((predictedValues - agaricus.test$label)^2)),
#'           Lex_model$test[Lex_model$best_iter])
#' 
#' }
#' 
#' @export

pred.Lextravagenza <- function(model, data, nrounds = model$best_iter) {
  
  predictedValues <- rep(model$base_margin, nrow(data))
  for (i in 1:nrounds) {
    predictedValues <- predictedValues - (predictedValues - predict(model$model[[i]], data))
  }
  
  return(predictedValues)
  
}