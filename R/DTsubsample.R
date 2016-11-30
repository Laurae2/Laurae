#' data.table subsampling (nearly without) copy
#'
#' This function attempts to subsample one data.table without making copies. Compared to direct subsampling, this can result to up to 1.1X memory efficiency. In most cases, you get a NEGATIVE memory efficiency even with frequent garbage collects. Use this only if you are working with super large datasets that fills up your RAM.
#' 
#' Warning: \code{DT} is a pointer only even if you pass the object to this function. This is how memory efficiency is achieved.
#' 
#' @param DT Type: data.table. The data.table to combine on.
#' @param kept Type: vector of integers. The rows to select for subsampling.
#' @param remove Type: boolean. Whether the argument \code{kept} acts as a removal (keep all rows which are not in \code{kept}). Defaults to \code{FALSE}.
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
#' #DT_sub <- DT[sample(5e6, 4e6, FALSE), ] #works good
#' DT_sub <- DTsubsample(DT, sample(4e6, 3e6, FALSE), low_mem = TRUE, collect = 5, silent = TRUE)
#' 
#' @export

DTsubsample <- function(DT, kept, remove = FALSE, low_mem = FALSE, collect = 0, silent = TRUE) {
  
  if (remove == TRUE) {
    kept <- (1:nrow(DT))[-kept]
  }
  
  cols <- colnames(DT)
  DT_sub = data.table(V1 = DT[[cols[1]]][kept])
  colnames(DT_sub) <- cols[1]
  
  # Magic required
  alloc.col(DT_sub, length(cols))
  
  if (low_mem == TRUE) {
    DT[[cols[1]]] <- NULL
  }
  
  if (collect == 0) {
    # Don't garbage collect
    
    if (low_mem == TRUE) {
      # delete old
      for (i in cols[2:length(cols)]) {
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
        for (i in cols[2:length(cols)]) {
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
        for (i in cols[2:length(cols)]) {
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