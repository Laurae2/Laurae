#' Xgboard Metric Evaluation Initialization (Environment)
#' 
#' This function initializes the environment for the dump (logging). This must be run before any xgboost run.
#' 
#' @param what Type: character. A character or a vector of characters corresponding to the names of the metrics to log (in the right order, if there are multiples). It must be the same length of \code{eval_metric} passed to \code{param} on \code{xgb.train}. Do not use more than 3 metrics, as Xgboard is designed for a maximum of 3 metrics.
#' @param watchnames Type: character. A character or a vector of characters corresponding to the names of the watchlist which are evaluated against the metric to log (in the right order, if there are multiples). It must be the same length of \code{watchlist} passed to \code{xgb.train}.
#' @param maximizer Type: logical. A logical or a vector of logicals corresponding to whether to maximize (\code{TRUE}) or minimize (\code{FALSE}) the evaluation metrics (\code{what}). It must be the same length of \code{what}.
#' @param log Type: character. Where to store the dump/log file? It must be an absolute path.
#' 
#' @return An environment for the dumping. Does not store the dump though.
#' 
#' @examples
#' \dontrun{
#' # We prepare environment for Accuracy/Threshold logging on Train/Test
#' # Stored in D:/debug/log.txt
#' my_envir <- xgboard.init(what = c("Accuracy", "Threshold"),
#'                          watchnames = c("Train", "Test"),
#'                          maximizer = c(TRUE, TRUE),
#'                          log = "D:/debug/log.txt")
#' }
#' 
#' @export

xgboard.init <- function(what, watchnames, maximizer, log) {
  
  envir <- new.env()
  envir[["persist"]] <- matrix(rep(c(0, 0, 0, rep(0, 2 * length(what))), length(watchnames)), nrow = length(watchnames), byrow = TRUE)
  envir[["params"]] <- list(length(watchnames), maximizer)
  envir[["timer"]] <- unname(Laurae::timer())
  envir[["log"]] <- log
  envir[["watch"]] <- watchnames
  envir[["what"]] <- what
  envir[["loop"]] <- 1
  cat(paste(c("Iteration", "Watchlist", "CurrentTime", "Time", what, paste0(what, "Improve")), collapse = ","), "\n", sep = "", file = log, append = FALSE)
  return(envir)
  
}
