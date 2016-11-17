#' Cross-Entropy -based Hybrid Optimization Helper (estimator)
#'
#' This function is a demonstration helper of the estimator function for the Cross-Entropy based hybrid optimization.
#' 
#' This function attempts to learn a model and optimize the depth parameter by learning slowly, in order to catch as much from the prior variables to optimize. Therefore, it is running 20 xgboosts.
#' Discrete variables are left as is (ordinals) as they optimizable in xgboost using two nodes in a tree only. This reduces the expense of creating a large matrix.
#' It is recommended to set manually the depth to a small value, like \code{6}. Not only the algorithm is faster, but is typically more accurate.
#' 
#' @param x Label on 1st column, all parameters on 2nd to last columns
#' 
#' @return A list with \code{model} as the model and \code{Error} as the error.
#' 
#' @export

.ExtraOpt_estimate <- function(x) {
  
  # Turns data into a xgb.DMatrix for xgboost
  x <- as.matrix(x)
  dtrain <- xgb.DMatrix(data = x[, -1], label = x[, 1], missing = NA)
  
  # Create folds for cross-validation
  folds <- kfold(x[, 1], k = 5, stratified = TRUE, seed = 0, named = FALSE)
  
  depth_score <- numeric(20)
  iterations <- numeric(20)
  for (i in 1:20) {
    set.seed(0)
    naive_model <- xgb.cv(data = dtrain,
                          nthread = 1,
                          nrounds = 10000,
                          early_stopping_rounds = 20,
                          eta = 0.1,
                          depth = i,
                          metrics = "rmse",
                          objective = "reg:linear",
                          booster = "gbtree",
                          verbose = FALSE,
                          folds = folds)
    iterations[i] <- naive_model$best_iteration
    depth_score[i] <- naive_model$evaluation_log[[3]][iterations[i]]
    
  }
  
  my_depth <- which.max(depth_score)
  
  my_model <- xgb.train(data = dtrain,
                        nthread = 1,
                        nrounds = floor(iterations[my_depth] * 1.1),
                        eta = 0.1,
                        detph = my_depth,
                        metrics = "rmse",
                        objective = "reg:linear",
                        booster = "gbtree",
                        verbose = FALSE)
  
  return(list(Model = my_model, Error = depth_score[my_depth]))
  
}