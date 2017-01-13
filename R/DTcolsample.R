#' data.table colsampling (nearly without) copy
#'
#' This function attempts to subsample one data.table without making copies. Well, you could just use \code{DT[, (mycols) := NULL]} (for removal) or \code{DT <- DT[, (mycols), with = FALSE]} for selecting...
#' 
#' Warning: \code{DT} is a pointer only even if you pass the object to this function. This is how memory efficiency is achieved.
#' 
#' @param DT Type: data.table. The data.table to combine on.
#' @param kept Type: vector of integers or vector of characters. The columns to select to keep.
#' @param remove Type: boolean. Whether the argument \code{kept} acts as a removal (keep all columns which are not in \code{kept}). Defaults to \code{FALSE}.
#' @param low_mem Type: boolean. Unallows DT (up to) twice in memory by deleting \code{DT} (WARNING: empties your \code{DT}) to save memory when set to \code{TRUE}. Setting it to \code{FALSE} allow \code{DT} to reside (up to) twice in memory, therefore memory usage increases. Defaults to \code{FALSE}.
#' @param collect Type: integer. Forces a garbage collect every \code{collect} iterations to clear up memory. Setting this to \code{1} along with \code{low_mem} = \code{TRUE} leads to the lowest possible memory usage one can ever get to merge two data.tables. It also prints verbose information about the process everytime it garbage collects. Setting this to \code{0} leads to no garbage collect. Lower values increases the time required to subsample the data.table. Defauls to \code{0}.
#' @param silent Type: boolean. Force silence during garbage collection iterations at no speed cost. Defaults to \code{TRUE}.
#' 
#' @return The subsampled data.table.
#' 
#' @examples
#' library(data.table)
#' DT <- data.frame(matrix(nrow = 50, ncol = 10))
#' DT <- setDT(DT)
#' colnames(DT) <- paste(colnames(DT), "xx", sep = "")
#' DT <- DTcolsample(DT, kept = 1:8, remove = FALSE, low_mem = TRUE)
#' DT <- DTcolsample(DT, kept = 1:6, remove = TRUE, low_mem = TRUE)
#' 
#' @export

DTcolsample <- function(DT, kept, remove = FALSE, low_mem = FALSE, collect = 0, silent = TRUE) {
  
  if (is.character(kept)) {
    # Work with characters
    kept <- which(kept %in% colnames(DT))
  }
  
  if (!remove) {
    kept <- (1:ncol(DT))[-kept]
  }
  
  kept <- colnames(DT)[kept]
  
  if (low_mem == TRUE) {
    
    DT[, (kept) := NULL]
    return(DT)
    
  } else {
    
    cols <- colnames(DT)[which(!colnames(DT) %in% kept)]
    DT_sub = data.table(V1 = DT[[cols[1]]])
    colnames(DT_sub) <- cols[1]
    alloc.col(DT_sub, length(cols))
    
    if (collect == 0) {
      # Don't garbage collect
      
      # not low mem
      for (i in 2:length(cols)) {
        set(DT_sub, j = cols[i], value = DT[[cols[i]]])
      }
      
    } else {
      # Do garbage collect
      
      if (silent == FALSE) {
        # not silent
        
        # not low mem
        for (i in 2:length(cols)) {
          set(DT_sub, j = cols[i], value = DT[[cols[i]]])
          if (!(i %% collect)) {gc(verbose = FALSE); cat("\rIteration: ", which(i == cols), ".", sep = "")}
        }
        
      } else {
        
        # not fast
        for (i in 2:length(cols)) {
          set(DT_sub, j = cols[i], value = DT[[cols[i]]])
          if (!(i %% collect)) {gc(verbose = FALSE)}
        }
        
      }
      
    }
    
    return(DT_sub)
    
  }
  
}
