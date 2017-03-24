#' Xgboard Server Launcher (Web Interface Creator)
#' 
#' This function runs the Xgboard Server and opens immediately a web browser if requested. Make sure to close Xgboard properly using the provided Exit button (if you did not close Xgboard properly and you are using RStudio's, you will have to restart RStudio or re-connect to your Xgboard Server). You may also kill the Rscript.exe manually. In addition, the web interface must be launched ONLY if a log file exists. If there is no log file existing, you will end up with an empty dashboard forever until you restart the web interface (a \code{"Restart Xgboard"} button is provided for this) and select your metrics.
#' 
#' @param dump Type: environment. An environment created by \code{xgboard.init}.
#' @param Rscript Type: character. The path to Rscript. Defaults to \code{paste0(R.home(component = "bin"), "/Rscript.exe")}, which means it will take the Rscript from the current R session.
#' @param ip Type: character. The IP to run the Shiny server from. \code{"127.0.0.1"} (safe) allows only local access (only from your computer which is running this script) while \code{"0.0.0.0"} (unsafe) will allow anyone who can connect to your computer to use your Xgboard. However, the latter allows you to access remotely to your Xgboard (the most recommended: SSH + port redirection for Extranet, no security for safe Intranet). Make sure the firewall has opened the port you request for the Xgboard. Note that Xgboard Server runs by default on \code{http} and you need Shiny Server (paid) for \code{https}. Defaults to \code{"127.0.0.1"} for security purposes.
#' @param port Type: numeric. The port to use to connect to the Xgboard. Make sure it is open by your firewall. Defaults to \code{6700}, which is the Shiny default port.
#' @param browser Type: logical. Whether to open immediately a link to the Xgboard Server on your browser once started. Defaults to \code{TRUE}.
#' @param polling Type: numeric. The polling rate to the Xgboard Server in milliseconds. Lower means faster refresh, at the expense of potentially high bandwidth and higher CPU usage (which means slower training, if currently training). Defaults to \code{5000}, which means polling the Xgboard Server for any updates every 5 seconds.
#' 
#' @return The URL to the Xgboard Server.
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

xgboard.run <- function(dump, Rscript = paste0(R.home(component = "bin"), "/Rscript.exe"), ip = "127.0.0.1", port = 6700, browser = TRUE, polling = 5000) {
  system(paste0('"', Rscript, '"', ' "', system.file("board/xgboard_run.R", package = "Laurae"), '" "', dump[["log"]], '" "', ip, '" ', port, " ", browser, " ", polling), wait = FALSE)
  return(paste0("http://", ip, ":", port))
}
