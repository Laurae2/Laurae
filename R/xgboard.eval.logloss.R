#' Xgboard Metric Evaluation Logloss (Binary Logloss)
#' 
#' This function is a custom metric for the logging of the (binary) Logarithmic Loss.
#' 
#' @param preds Type: numeric. The predictions.
#' @param dtrain Type: xgb.DMatrix. The training data.
#' @param dump Type: environment. An environment created by \code{xgboard.init}.
#' 
#' @return The logarithmic loss..
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
#' # Fourth, we prepare environment for Logloss logging on Train/Test
#' # Stored in D:/debug/log.txt
#' my_envir <- xgboard.init(what = "Logloss",
#'                          watchnames = c("Train", "Test"),
#'                          maximizer = FALSE,
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
#'               eval_metric = xgboard.xgb(f = xgboard.eval.logloss, dumper = my_envir))
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

xgboard.eval.logloss <- function(preds, dtrain, dump) {
  
  labels <- getinfo(dtrain, "label")
  predicted <- pmin(pmax(preds, 1e-15), 1 - 1e-15)
  metric <- -(mean(labels * log(preds) + (1 - labels) * log(1 - preds)))
  
  xgboard.dump(metric, dump)
  return(list(metric = "logloss", value = metric))
  
}
