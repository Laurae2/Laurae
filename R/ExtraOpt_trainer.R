#' Cross-Entropy -based Hybrid Optimization Helper (trainer)
#'
#' This function is a demonstration helper of the trainer function for the Cross-Entropy based hybrid optimization.
#' 
#' This function is the function returning the loss to optimize.
#' This example trains a supervised model with multiple arguments passed from ExtraOpt, and returns the loss.
#' 
#' @param x All continuous variables.
#' @param y All discrete variables.
#' @param nthreads THe number of threads to use. Argument passed from \code{ExtraOpt} in \code{...}
#' @param eta The shrinkage rate to use. Argument passed from \code{ExtraOpt} in \code{...}
#' @param early_stop The number of iterations of non-improvement for stopping to use. Argument passed from \code{ExtraOpt} in \code{...}
#' @param X_train The training features to use. Argument passed from \code{ExtraOpt} in \code{...}
#' @param X_test The testing features to use. Argument passed from \code{ExtraOpt} in \code{...}
#' @param Y_train The training label to use. Argument passed from \code{ExtraOpt} in \code{...}
#' @param Y_test The testing label to use. Argument passed from \code{ExtraOpt} in \code{...}
#' 
#' @return The loss to optimize.
#' 
#' @export

.ExtraOpt_trainer <- function(x, y, nthreads, eta, early_stop, X_train, X_test, Y_train, Y_test) {
  
  to_keep <- as.logical(y)
  
  if (sum(to_keep) > 1) {
    
    dtrain <- xgb.DMatrix(data = X_train[, to_keep], label = Y_train, missing = NA)
    dtest <- xgb.DMatrix(data = X_test[, to_keep], label = Y_test, missing = NA)
    set.seed(0)
    naive_model <- xgb.train(data = dtrain,
                             nthread = nthreads,
                             nrounds = 10000,
                             early_stopping_rounds = early_stop,
                             eta = eta,
                             depth = floor(x[1]),
                             min_child_weight = x[2],
                             gamma = x[3],
                             colsample_bytree = x[4],
                             subsample = x[5],
                             metrics = "rmse",
                             objective = "reg:linear",
                             verbose = FALSE,
                             watchlist = list(test = dtest))
    preds <- predict(naive_model, dtest, ntreelimit = naive_model$best_iteration)
    error <- mean((preds - Y_test) * (preds - Y_test))
    
    return(error)
    
  } else {
    
    return(-9999)
    
  }
  
}