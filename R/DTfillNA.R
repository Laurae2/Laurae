#' data.table NA fill (nearly without) copy (or data.frame)
#'
#' This function attempts to fill NA values in a data.table. Compared to \code{DT[is.na(DT)] <- value}, this result in a guaranteed 3X memory efficiency. By default, a 2X memory efficiency is minimal with frequent garbage collects.
#' 
#' Warning: \code{DT} is a pointer only and is directly modified.
#' 
#' @param DT Type: data.table (or a data.frame, partially supported). The data.table to fill NAs on.
#' @param value Type: vector of length 1 or of length \code{ncol(DT)}. If a vector of length 1 is supplied, NA values are replaced by that value. Otherwise, attempts to replace values by matching the column number with the vector. Defaults to \code{0}.
#' @param low_mem Type: boolean. Unallows \code{DT} twice in memory by modifying \code{DT} in place. (WARNING: empties your \code{DT}) to save memory when set to \code{TRUE}. Setting it to \code{FALSE} allow \code{DT} to reside twice in memory, therefore memory usage increases. Defaults to \code{FALSE}.
#' @param collect Type: integer. Forces a garbage collect every \code{collect} iterations to clear up memory. Setting this to \code{1} along with \code{low_mem} = \code{TRUE} leads to the lowest possible memory usage one can ever get to merge two data.tables. It also prints verbose information about the process everytime it garbage collects. Setting this to \code{0} leads to no garbage collect. Lower values increases the time required to bind the data.tables. Defauls to \code{0}.
#' @param silent Type: boolean. Force silence during garbage collection iterations at no speed cost. Defaults to \code{TRUE}.
#' 
#' @return A data.table with filled NA values (if \code{low_mem} is set to \code{TRUE}).
#' 
#' @examples
#' #library(data.table)
#' df1 <- data.frame(matrix(nrow = 50000, ncol = 1000))
#' df2 <- data.frame(matrix(nrow = 50000, ncol = 1000))
#' setDT(df1)
#' setDT(df2)
#' gc() # check memory usage...380MB?
#' DTfillNA(df2, value = 0, low_mem = TRUE, collect = 20, silent = TRUE)
#' gc() # check memory usage peak... 600MB?
#' rm(df2)
#' gc() # 200MB only, lets try with only 1 frame left...
#' df1[is.na(df1)] <- 0
#' gc() # with 1 data.table less, memory still peaked to 850MB (200MB->850MB)
#' # e.g it took at least 3.5X more memory than the object alone
#' 
#' df2 <- data.frame(matrix(nrow = 50000, ncol = 1000))
#' setDT(df2)
#' DTfillNA(df2, value = 0, low_mem = TRUE, collect = 20, silent = TRUE)
#' gc() # all good
#' identical(df1, df2) # TRUE => the same...
#' 
#' rm(df1, df2)
#' gc(reset = TRUE)
#' 
#' # Let's try to make a copy
#' df1 <- data.frame(matrix(nrow = 50000, ncol = 1000))
#' df2 <- DTfillNA(df1, value = 99, low_mem = FALSE, collect = 50, silent = TRUE)
#' gc() # only 650MB, much better than doing df2 <- df1; df2[is.na(df2)] <- 99
#' 
#' rm(df1, df2)
#' gc(reset = TRUE)
#' 
#' # This can't be done in R "easily" without hackery ways (fill 1 to 1000 by column)
#' df1 <- data.frame(matrix(nrow = 50000, ncol = 1000))
#' df2 <- DTfillNA(df1, value = 1:1000, low_mem = FALSE, collect = 50, silent = TRUE)
#' gc() # only 650MB
#' 
#' # You can do this on data.frame too...
#' # It will NOT coerce to data.table
#' # Just remember it doesn't update in real time in RStudio
#' df2 <- data.frame(matrix(nrow = 50000, ncol = 1000))
#' DTfillNA(df2, value = 1:1000, low_mem = TRUE, collect = 50, silent = TRUE)
#' head(df2)
#' is.data.table(df2) # FALSE, we did in-place replacement without parent.env hehe
#' 
#' @export

DTfillNA <- function(DT, value = 0, low_mem = FALSE, collect = 0, silent = TRUE) {
  
  cols <- colnames(DT)
  temp_vec <- numeric(nrow(DT))
  temp_NA <- numeric(nrow(DT))
  
  # Attempt to make the value larger to not have to make more loops
  if (length(value) == 1) {
    value <- rep(value, ncol(DT))
  }
  
  # Attempt to recreate a new data.table from scratch
  if (low_mem == FALSE) {
    temp_vec <- DT[[cols[1]]]
    temp_NA <- is.na(temp_vec)
    temp_vec[temp_NA] <- value[1]
    DT_sub <- as.data.table(temp_vec)
    colnames(DT_sub)[1] <- cols[1]
    alloc.col(DT_sub, length(cols))
    
  }
  
  if (collect == 0) {
    # Don't garbage collect
    
    if (low_mem == TRUE) {
      # delete old
      j <- 0
      for (i in cols) {
        j <- j + 1
        temp_vec <- DT[[i]]
        temp_NA <- is.na(temp_vec)
        temp_vec[temp_NA] <- value[j]
        set(DT, j = i, value = temp_vec)
      }
      
    } else {
      # not fast
      j <- 1
      for (i in cols[2:length(cols)]) {
        j <- j + 1
        temp_vec <- DT[[i]]
        temp_NA <- is.na(temp_vec)
        temp_vec[temp_NA] <- value[j]
        set(DT_sub, j = i, value = temp_vec)
      }
      return(DT_sub)
      
    }
    
  } else {
    # Do garbage collect
    
    if (silent == FALSE) {
      # not silent
      
      if (low_mem == TRUE) {
        # delete old
        j <- 0
        for (i in cols) {
          j <- j + 1
          temp_vec <- DT[[i]]
          temp_NA <- is.na(temp_vec)
          temp_vec[temp_NA] <- value[j]
          set(DT, j = i, value = temp_vec)
          if (!(j %% collect)) {gc(verbose = FALSE); cat("\rIteration: ", j, ".", sep = "")}
        }
        
      } else {
        # not fast
        j <- 1
        for (i in cols[2:length(cols)]) {
          j <- j + 1
          temp_vec <- DT[[i]]
          temp_NA <- is.na(temp_vec)
          temp_vec[temp_NA] <- value[j]
          set(DT_sub, j = i, value = temp_vec)
          if (!(j %% collect)) {gc(verbose = FALSE); cat("\rIteration: ", j, ".", sep = "")}
        }
        return(DT_sub)
        
      }
      
    } else {
      
      if (low_mem == TRUE) {
        # delete old
        j <- 0
        for (i in cols) {
          j <- j + 1
          temp_vec <- DT[[i]]
          temp_NA <- is.na(temp_vec)
          temp_vec[temp_NA] <- value[j]
          set(DT, j = i, value = temp_vec)
          if (!(j %% collect)) {gc(verbose = FALSE)}
        }
        
      } else {
        # not fast
        j <- 1
        for (i in cols[2:length(cols)]) {
          j <- j + 1
          temp_vec <- DT[[i]]
          temp_NA <- is.na(temp_vec)
          temp_vec[temp_NA] <- value[j]
          set(DT_sub, j = i, value = temp_vec)
          if (!(j %% collect)) {gc(verbose = FALSE)}
        }
        return(DT_sub)
        
      }
      
    }
    
  }
  
}