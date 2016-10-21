#' data.table subsampling (nearly without) copy
#'
#' This function attempts to subsample one data.table without making copies. Compared to direct subsamplingh, this can result to up to 2X memory efficiency. By default, a 1.5X memory efficiency is minimal with frequent garbage collects.
#' 
#' Warning: \code{DT} is a pointer only even if you pass the object to this function. This is how memory efficiency is achieved.
#' 
#' @param DT Type: data.table. The data.table to combine on.
#' @param kept Type: vector of integers. The rows to select for subsampling.
#' @param low_mem Type: boolean. Unallows DT (up to) twice in memory by deleting \code{DT} (WARNING: empties your \code{DT}) to save memory when set to \code{TRUE}. Setting it to \code{FALSE} allow \code{DT} to reside (up to) twice in memory, therefore memory usage increases. Defaults to \code{FALSE}.
#' @param collect Type: integer. Forces a garbage collect every \code{collect} iterations to clear up memory. Setting this to \code{1} along with \code{low_mem} = \code{TRUE} leads to the lowest possible memory usage one can ever get to merge two data.tables. It also prints verbose information about the process everytime it garbage collects. Setting this to \code{0} leads to no garbage collect. Lower values increases the time required to subsample the data.table. Defauls to \code{0}.
#' @param silent Type: boolean. Force silence during garbage collection iterations at no speed cost. Defaults to \code{TRUE}.
#' 
#' @return The subsampled data.table.
#' 
#' @examples
#' library(data.table)
#' DT <- data.frame(matrix(nrow = 5000000, ncol = 10))
#' DT <- setDT(DT)
#' DT[is.na(DT)] <- 1
#' colnames(DT) <- paste(colnames(DT), "xx", sep = "")
#' kept <- 1:4000000
#' DT_sub <- DTsubsample(DT, sample(5e6, 4e6, FALSE), collect = 5, silent = TRUE)
#' # if you are a hero with enough RAM (2GB+ recommended), uncomment the line below and run it
#' # DT_sub <- DT[sample(5e6, 4e6, FALSE), ]
#' 
#' @export

DTsubsample <- function(DT, kept, low_mem = FALSE, collect = 0, silent = FALSE) {
  
  cols <- colnames(DT)
  DT_sub = data.table(V1 = DT[[cols[1]]][kept])
  colnames(DT_sub) <- cols[1]
  
  # Magic required
  alloc.col(DT_sub, length(cols))
  
  if (low_mem == TRUE) {
    DT[[i]] <- NULL
  }
  
  if (collect == 0) {
    # Don't garbage collect
    
    if (low_mem == TRUE) {
      # delete old
      for (i in cols) {
        set(DT_sub, j = i, value = DT[[i]][kept])
        DT[[i]] <- NULL
      }
      
    } else {
      # not low mem
      for (i in cols[2:length(cols)]) {
        set(DT_sub, j = i, value = DT[[i]][kept])
      }
      
    }
    
  } else {
    # Do garbage collect
    
    if (silent == FALSE) {
      # not silent
      
      if (low_mem == TRUE) {
        # delete old
        for (i in cols) {
          set(DT_sub, j = i, value = DT[[i]][kept])
          DT[[i]] <- NULL
          if (!((which(i == cols) - 1) %% collect)) {gc(verbose = FALSE); cat("\rIteration: ", which(i == cols), ".", sep = "")}
        }
        
      } else {
        # not low mem
        for (i in cols[2:length(cols)]) {
          set(DT_sub, j = i, value = DT[[i]][kept])
          if (!((which(i == cols) - 1) %% collect)) {gc(verbose = FALSE); cat("\rIteration: ", which(i == cols), ".", sep = "")}
        }
        
      }
      
    } else {
      
      if (low_mem == TRUE) {
        # delete old
        for (i in cols) {
          set(DT_sub, j = i, value = DT[[i]][kept])
          DT[[i]] <- NULL
          if (!((which(i == cols) - 1) %% collect)) {gc(verbose = FALSE)}
        }
        
      } else {
        # not fast
        for (i in cols) {
          set(DT_sub, j = i, value = DT[[i]][kept])
          if (!((which(i == cols) - 1) %% collect)) {gc(verbose = FALSE)}
        }
        
      }
      
    }
  }
  
  return(DT_sub)
  
}