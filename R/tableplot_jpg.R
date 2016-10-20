#' Batch tableplot generator to JPEG
#'
#' This functions allows you to create JPEGs of tableplots aganist a dataset and a specific label (which must be a factor or a numeric)
#' A numeric target allows to provide an overview of each variable against the ordering of the label
#' A factor target allows to provide an overview of each variable against each factor of the label
#' 
#' @param data The data to load.
#' @param target The target label. Either factor or numeric.
#' @param nBins The amount of bins per histogram. Should be between 50 and 500 if possible (higher may overfit, lower may underfit). Defaults to \code{100}.
#' @param folder The output folder where tableplots will be stored. Defaults to \code{"./autoplots/plot_"}.
#' @param ID Should the filename use the ID of the feature (starts at 1) or the name of the feature? Defaults to \code{FALSE}.
#' @param width The width output of each tableplot, in pixels. Defaults to \code{960}.
#' @param height The height output of each tableplot, in pixels. Defaults to \code{960}.
#' @param pointsize The pointsize output of each tableplot, in pixels. Defaults to \code{12}.
#' 
#' @return Nothing
#' 
#' @examples 
#' tableplot_jpg(train[, !(colnames(train) %in% "target")], train$target,
#' folder = "./plots/my_plot_", ID = FALSE, width = 960, height = 960, pointsize = 12)
#' 
#' @export

tableplot_jpg <- function(data, target, nBins = 100, folder = "./autoplots/plot_", ID = FALSE, width = 960, height = 960, pointsize = 12) {
  
  data <- tablePrepare(data.frame(target = target, data))
  for (i in 2:ncol(data)) {
    jpeg(filename = paste(folder, ifelse(ID == FALSE, colnames(data)[i], i-1), ".jpg", sep = ""), width = width, height = height, units = "px", pointsize = pointsize)
    tableplot(dat = data, select = c(1,i), sortCol = target, sample = FALSE, nBins = nBins)
    dev.off()
  }
  
}

