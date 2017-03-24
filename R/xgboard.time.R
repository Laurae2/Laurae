#' Xgboard Metric Evaluation Time Reset (Environment)
#' 
#' This function resets the timer on the environment for accurate timing of the model. This must be run before any xgboost run.
#' 
#' @param dump Type: environment. An environment created by \code{xgboard.init}.
#' 
#' @return An environment for the dumping. Does not store the dump though.
#' 
#' @export

xgboard.time <- function(dump) {
  
  dump[["timer"]] <- unname(Laurae::timer())
  cat(paste(c("Iteration", "Watchlist", "CurrentTime", "Time", dump[["what"]], paste0(dump[["what"]], "Improve")), collapse = ","), "\n", sep = "", file = dump[["log"]], append = FALSE)
  return(dump)
  
}
