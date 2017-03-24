#' Xgboard Metric Evaluation Error (Binary Accuracy)
#' 
#' This function is a custom metric for the logging of the (binary) Accuracy.
#' 
#' @param preds Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' @param dump Type: environment. An environment created by \code{xgboard.init}.
#' 
#' @return The maximum accuracy for binary data.
#' 
#' @examples
#' \dontrun{
#' # First, we must load libraries: xgboost, data.table, and R.utils
#' library(xgboost)
#' library(data.table)
#' library(R.utils)
#' 
#' # Second, we load some data
#' data(agaricus.train, package='xgboost')
#' data(agaricus.test, package='xgboost')
#' 
#' # Third, we create the xgb.DMatrices and the watchlist
#' dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
#' dtest <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)
#' watchlist <- list(train = dtrain, eval = dtest)
#' 
#' # Fourth, we prepare environment for Accuracy/Threshold logging on Train/Test
#' # Stored in D:/debug/log.txt
#' my_envir <- xgboard.init(what = c("Accuracy", "Threshold"),
#'                          watchnames = c("Train", "Test"),
#'                          maximizer = c(TRUE, TRUE),
#'                          log = "D:/debug/log.txt")
#' 
#' # Fifth we spawn the xgboard to open in browser
#' xgboard.run(my_envir)
#' 
#' # Fifth, the model is set for training using these parameters
#' # Take note of eval_metric needing xgboard.xgb(f = your metric, dumper = envir)
#' param <- list(max_depth = 2,
#'               eta = 0.05,
#'               silent = 1,
#'               nthread = 2, 
#'               objective = "binary:logistic",
#'               eval_metric = xgboard.xgb(f = xgboard.eval.error, dumper = my_envir))
#' 
#' # Sixth, we train a model with full logging
#' # We can notice it will update in real time
#' # The number of warning messages = number of file locks which xgboost waits
#' # because the log file is LOCKED when read by Xgboard (to avoid crashes)
#' set.seed(0)
#' bst <- xgb.train(param,
#'                  dtrain,
#'                  nrounds = 500,
#'                  watchlist)
#' 
#' # If you intend to run again xgboost, you have to do the following:
#' # - Reset the dump environment using xgboard.init
#' # - Reset the eval_metric from the parameters (because it will use the previous envir!)
#' # - If you are using an interactive console, use xgboard.time before setting the seed!
#' }
#' 
#' @export

xgboard.eval.error <- function(preds, dtrain, dump) {
  
  y_true <- getinfo(dtrain, "label")
  DT <- data.table(y_true = y_true, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  lens <- length(y_true)
  nump <- sum(y_true)
  DT[, tn_v := cumsum(y_true == 0)]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  DT <- DT[cleaner, ]
  DT[, acc := (tn_v + tp_v) / lens]
  best <- which.max(DT$acc)[1]
  metric <- c(DT$acc[best], DT$y_prob[best])
  
  xgboard.dump(metric, dump)
  return(list(metric = "error", value = metric[1]))
  
}
