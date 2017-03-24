#' Xgboard Metric Evaluation Creator (Wrapper)
#' 
#' This function is a wrapper to create an evaluation metric.
#' 
#' @param ... Type: nothing. Ignore.
#' @param f Type: function. An xgboost evaluation metric function with \code{dump} input.
#' @param dumper Type: environment. An environment created by \code{xgboard.init}.
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#' # Model will be trained using these parameters
#' # Take note of eval_metric needing xgboard.xgb(f = your metric, dumper = envir)
#' param <- list(max_depth = 2,
#'               eta = 1,
#'               silent = 1,
#'               nthread = 2, 
#'               objective = "binary:logistic",
#'               eval_metric = xgboard.xgb(f = board.evalerror, dumper = my_envir))
#' }
#' 
#' @export

xgboard.xgb <- function(..., f, dumper) {
  return(function(...) {f(..., dump = dumper)})
}
