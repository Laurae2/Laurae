#' data.table colsampling (nearly without) copy
#'
#' This function attempts to subsample one data.table without making copies. Well, you could just use \code{DT[, (mycols) := NULL]} (for removal) or \code{DT <- DT[, (mycols), with = FALSE]} for selecting...
#' 
#' Warning: \code{DT} is a pointer only even if you pass the object to this function. This is how memory efficiency is achieved.
#' 
#' @param DT Type: data.table. The data.table to combine on.
#' @param kept Type: vector of integers or vector of characters. The columns to select to keep.
#' @param remove Type: boolean. Whether the argument \code{kept} acts as a removal (keep all columns which are not in \code{kept}). Defaults to \code{FALSE}.
#' 
#' @return The subsampled data.table.
#' 
#' @examples
#' #library(data.table)
#' DT <- data.frame(matrix(nrow = 50, ncol = 10))
#' DT <- setDT(DT)
#' colnames(DT) <- paste(colnames(DT), "xx", sep = "")
#' DT <- DTcolsample(DT, kept = 1:8, remove = FALSE)
#' DT <- DTcolsample(DT, kept = 1:6, remove = TRUE)
#' 
#' @export

DTcolsample <- function(DT, kept, remove = FALSE) {
  
  if (is.character(kept)) {
    # Work with characters
    kept <- which(kept %in% colnames(DT))
  }
  
  if (!remove) {
    kept <- (1:ncol(DT))[-kept]
  }
  
  kept <- colnames(DT)[kept]
  
  DT[, (kept) := NULL]
  
  return(DT)
  
}