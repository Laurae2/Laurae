#' data.table column binding (nearly without) copy
#'
#' This function attempts to cbind two data.tables without making copies. Compared to cbind, this can result to up to 3X memory efficiency. By default, a 2X memory efficiency is minimal with frequent garbage collects.
#' 
#' Warning: \code{dt1} and \code{dt2} are pointers only even if you pass the objects to this function. This is how memory efficiency is achieved. \code{dt1} gets overwritten on the fly.
#' 
#' @param dt1 Type: data.table. The data.table to combine on.
#' @param dt2 Type: data.table. The data.table to "copy" on \code{dt1}
#' @param low_mem Type: boolean. Unallows \code{dt2} twice in memory by deleting \code{dt2} (WARNING: empties your \code{dt2}) to save memory when set to \code{TRUE}. Setting it to \code{FALSE} allow \code{dt2} to reside twice in memory, therefore memory usage increases. Defaults to \code{FALSE}.
#' @param collect Type: integer. Forces a garbage collect every \code{collect} iterations to clear up memory. Setting this to \code{1} along with \code{low_mem} = \code{TRUE} leads to the lowest possible memory usage one can ever get to merge two data.tables. It also prints verbose information about the process everytime it garbage collects. Setting this to \code{0} leads to no garbage collect. Lower values increases the time required to bind the data.tables. Defauls to \code{0}.
#' @param silent Type: boolean. Force silence during garbage collection iterations at no speed cost. Defaults to \code{TRUE}.
#' 
#' @return A data.table based on \code{dt1}.
#' 
#' @examples
#' library(data.table)
#' df1 <- data.frame(matrix(nrow = 50000, ncol = 1000))
#' df2 <- data.frame(matrix(nrow = 50000, ncol = 1000))
#' setDT(df1)
#' setDT(df2)
#' df1[is.na(df1)] <- 1
#' gc()
#' df2[is.na(df2)] <- 2
#' colnames(df1) <- paste("x", colnames(df1), sep = "")
#' gc() # look memory usage
#' # open a task manager to check current RAM usage
#' df1 <- DTcbind(df1, df2, low_mem = TRUE, collect = 20, silent = FALSE)
#' # check RAM usage in a task manager: it is identical to what we had previously!
#' gc() # gives no gain
#' df3 <- data.frame(matrix(nrow = 50000, ncol = 1000))
#' setDT(df3)
#' # look on task manager the current RAM usage
#' #df1 <- cbind(df1, df3) # RAM usage explodes!
#' 
#' @export

DTcbind <- function(dt1, dt2, low_mem = FALSE, collect = 0, silent = TRUE) {
  
  cols <- copy(colnames(dt2))
  alloc.col(dt1, length(colnames(dt1)) + length(cols))
  
  if (collect == 0) {
    # Don't garbage collect
    
    if (low_mem == TRUE) {
      # delete old
      for (i in cols) {
        set(dt1, j = i, value = copy(dt2[[i]]))
        set(dt2, j = i, value = NULL)
      }
      
    } else {
      # not fast
      for (i in cols) {
        set(dt1, j = i, value = copy(dt2[[i]]))
      }
      
    }
    
  } else {
    # Do garbage collect
    
    j <- 0
    
    if (silent == FALSE) {
      # not silent
      
      if (low_mem == TRUE) {
        # delete old
        for (i in cols) {
          j <- j + 1
          set(dt1, j = i, value = copy(dt2[[i]]))
          set(dt2, j = i, value = NULL)
          if (!(j %% collect)) {gc(verbose = FALSE); cat("\rIteration: ", j, ".", sep = "")}
        }
        
      } else {
        # not fast
        for (i in cols) {
          j <- j + 1
          set(dt1, j = i, value = copy(dt2[[i]]))
          if (!(j %% collect)) {gc(verbose = FALSE); cat("\rIteration: ", j, ".", sep = "")}
        }
        
      }
      
    } else {
      
      if (low_mem == TRUE) {
        # delete old
        for (i in cols) {
          j <- j + 1
          set(dt1, j = i, value = copy(dt2[[i]]))
          set(dt2, j = i, value = NULL)
          if (!(j %% collect)) {gc(verbose = FALSE)}
        }
        
      } else {
        # not fast
        for (i in cols) {
          j <- j + 1
          set(dt1, j = i, value = copy(dt2[[i]]))
          if (!(j %% collect)) {gc(verbose = FALSE)}
        }
        
      }
      
    }
    
  }
  
  return(dt1)
  
}