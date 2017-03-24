#' Xgboard Dumper
#' 
#' This function performs dumping (logging) of the specified input metrics on the dump (log file). It resists file locks, but if the file lock is permanent, then this function will never stop (and xgboost will be interrupted).
#' 
#' @param metric TYpe: numeric. A single value or a vector of numerics representing the metric.
#' @param dump Type: environment. An environment created by \code{xgboard.init}.
#' 
#' @return NULL
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
#' # Fourth, we create a metric with Accuracy/Threshold logging
#' xgboard.eval.error <- function(preds, dtrain, dump) {
#' 
#'   # Get xgboost label info
#'   y_true <- getinfo(dtrain, "label")
#'   
#'   # Do stuff to get best acc + best threshold
#'   DT <- data.table(y_true = y_true, y_prob = preds, key = "y_prob")
#'   cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
#'   lens <- length(y_true)
#'   nump <- sum(y_true)
#'   DT[, tn_v := cumsum(y_true == 0)]
#'   DT[, tp_v := nump - cumsum(y_true == 1)]
#'   DT <- DT[cleaner, ]
#'   DT[, acc := (tn_v + tp_v) / lens]
#'   
#'   # Stuff is stored here
#'   best <- which.max(DT$acc)[1]
#'   metric <- c(DT$acc[best], DT$y_prob[best])
#'   
#'   # Dump (log) both Accuracy and Threshold
#'   xgboard.dump(metric, dump)
#'   
#'   # Return metric as typically done with xgboost custom evaluation metric
#'   return(list(metric = "error", value = metric[1]))
#' }
#' }
#' 
#' @export

xgboard.dump <- function(metric, dump) {
  
  # Get old parameters
  current_param <- dump[["params"]]
  current_loop <- dump[["loop"]]
  current_dump <- dump[["persist"]][current_loop, ]
  current_time <- unname(Laurae::timer())
  current_time[current_time < dump[["timer"]]] <- dump[["timer"]] # Happens randomly
  
  # Get new parameters to dump: iteration, current data, current time, delta time, metric, delta metric
  return_vec <- c(current_dump[1] + 1, # Iteration
                  current_loop, # Current dataset
                  current_time, # Current time in milliseconds
                  (current_time - dump[["timer"]]), # Delta time in milliseconds (N vs N-1)
                  metric, # Current metric
                  (metric - current_dump[4:(3 + length(current_param[[2]]))]) * (current_dump[1] != 0) * (current_param[[2]] * 2 - 1)) # Delta metric (N vs N-1) depending on maximization/minimization (without IF-THEN-ELSE-ENDIF logic)
  
  # Store a copy of parameters to dump
  parsed_vec <- return_vec
  
  # Store current dataset iteration in a temp variable
  old_parsed <- parsed_vec[2]
  
  # Adjust iteration (if last dataset evaluated => go back to 1, else add 1 - all without IF-THEN-ELSE-ENDIF logic)
  dump[["loop"]] <- (old_parsed %% current_param[[1]]) + 1
  
  # Reset iteration to the new iteration
  return_vec[2] <- dump[["watch"]][parsed_vec[2]]
  
  # Dump time (update absolute time)
  dump[["timer"]] <- current_time
  
  # Dump new parameters of dataset without current time in milliseconds (update parameters of dataset)
  dump[["persist"]][old_parsed, ] <- parsed_vec[-3]
  
  # Write to log and bypasss filelocks (loop forever until it passes)
  while(tryCatch({cat(paste(return_vec, collapse = ","), "\n", sep = "", file = dump[["log"]], append = TRUE); FALSE}, error = function(e) {TRUE})) {}
  
  # Return to original function
  return(NULL)
  
}
