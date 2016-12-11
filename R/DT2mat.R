#' data.table to matrix
#'
#' This function coerces a data.table to a matrix using the least possible memory.
#' 
#' Warning: \code{DT} is a pointer only and is directly modified.
#' 
#' @param DT Type: data.table (or a data.frame, partially supported). The data.table to coerce to matrix.
#' @param low_mem Type: boolean. Unallows \code{DT} twice in memory by modifying \code{DT} in place. (WARNING: empties your \code{DT}) to save memory when set to \code{TRUE}. Setting it to \code{FALSE} allow \code{DT} to reside twice in memory, therefore memory usage increases. Defaults to \code{FALSE}.
#' @param collect Type: integer. Forces a garbage collect every \code{collect} iterations to clear up memory. Setting this to \code{1} along with \code{low_mem} = \code{TRUE} leads to the lowest possible memory usage one can ever get to merge two data.tables. It also prints verbose information about the process everytime it garbage collects. Setting this to \code{0} leads to no garbage collect. Lower values increases the time required to bind the data.tables. Defauls to \code{0}.
#' @param silent Type: boolean. Force silence during garbage collection iterations at no speed cost. Defaults to \code{TRUE}.
#' 
#' @return A matrix.
#' 
#' @examples
#' library(data.table)
#' df <- data.table(matrix(1:50000000, nrow = 50000))
#' mat <- DT2mat(df, low_mem = TRUE, collect = 50, silent = FALSE)
#' 
#' @export

DT2mat <- function(DT, low_mem = FALSE, collect = 0, silent = TRUE) {
  
  # Can't initialize lower
  mat_sub <- matrix(rep(FALSE, nrow(DT) * ncol(DT)), ncol = ncol(DT))
  
  cols <- copy(colnames(DT))
  colnames(mat_sub) <- cols
  
  if (collect == 0) {
    # Don't garbage collect
    
    if (low_mem == TRUE) {
      # delete old
      for (i in cols) {
        mat_sub[, which(cols %in% i)] <- copy(DT[[i]])
        set(DT, j = i, value = NULL)
      }
      
    } else {
      # not low mem
      for (i in cols) {
        mat_sub[, which(cols %in% i)] <- copy(DT[[i]])
      }
      
    }
    
  } else {
    # Do garbage collect
    
    if (silent == FALSE) {
      # not silent
      
      if (low_mem == TRUE) {
        # delete old
        j <- 1
        for (i in cols) {
          j <- j + 1
          mat_sub[, which(cols %in% i)] <- copy(DT[[i]])
          set(DT, j = i, value = NULL)
          if (!(j %% collect)) {gc(verbose = FALSE); cat("\rIteration: ", j, ".", sep = "")}
        }
        
      } else {
        # not low mem
        j <- 0
        for (i in cols) {
          j <- j + 1
          mat_sub[, which(cols %in% i)] <- copy(DT[[i]])
          if (!(j %% collect)) {gc(verbose = FALSE); cat("\rIteration: ", j, ".", sep = "")}
        }
        
      }
      
    } else {
      
      if (low_mem == TRUE) {
        # delete old
        j <- 1
        for (i in cols) {
          j <- j + 1
          mat_sub[, which(cols %in% i)] <- copy(DT[[i]])
          set(DT, j = i, value = NULL)
          if (!(j %% collect)) {gc(verbose = FALSE)}
        }
        
      } else {
        # not fast
        j <- 0
        for (i in cols) {
          j <- j + 1
          mat_sub[, which(cols %in% i)] <- copy(DT[[i]])
          if (!(j %% collect)) {gc(verbose = FALSE)}
        }
        
      }
      
    }
  }
  
  return(mat_sub)
  
}