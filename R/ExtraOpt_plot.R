#' Cross-Entropy -based Hybrid Optimization Helper (plotter)
#'
#' This function is a demonstration helper of the plotting function for the Cross-Entropy based hybrid optimization.
#' 
#' Plot iteration vs loss to a file. In this example, we suppose 5 continuous variables and 5 discrete variables to optimize for a supervised machine learning model. It saves the image file \code{"iter.jpg"} under the \code{temp} directory of the current working directory found by \code{getwd()}, with a resolution of \code{"1080x540"} pixels.
#' 
#' @param priors The priors. Label on 1st column, all parameters on 2nd to last columns
#' 
#' @return A string which is not used.
#' 
#' @export

.ExtraOpt_plot <- function(priors) {
  
  to_plot <- rbind(Iteration = 1:nrow(priors), Loss = priors[, 2], Features = rowSums(priors[, 7:11]))
  jpeg(filename = file.path(getwd(), "temp", "iter.jpg"), width = 1080, height = 540, units = "px")
  scatterplotMatrix(to_plot)
  dev.off()
  
  return("Done")
  
}