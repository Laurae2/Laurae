#' data.table column list binding
#'
#' This function attempts to replicate \code{rbindlist} for columns.
#' 
#' @param target Type: list. A list to perform \code{cbindlist} on, with vectors only.
#' 
#' @return A data.table based on \code{target}.
#' 
#' @examples
#' library(data.table)
#' 
#' # Create named junk list
#' mega_list <- list(a = rnorm(1000),
#'                   b = rnorm(1000),
#'                   c = rnorm(1000),
#'                   d = rnorm(1000),
#'                   e = rnorm(1000),
#'                   f = rnorm(1000),
#'                   g = rnorm(1000))
#' 
#' # Perform cbindlist, equivalent of rbindlist but for columns, accepting only vectors
#' my_dt <- cbindlist(mega_list)
#' 
#' # Check column name correctness/coverage
#' sum(colnames(my_dt) == c("a", "b", "c", "d", "e", "f", "g")) / 7 # 100% coverage
#' 
#' @export

cbindlist <- function(target) {
  
  # Pre-initialize data.table
  cols <- length(target)
  dt <- data.table(target[[1]])
  
  # More than one element in list?
  if (cols > 1) {
    
    # Pre-allocate columns for data.table
    alloc.col(dt, cols)
    
    if (is.null(names(target))) {
      
      # Add columns with generated name with standardized length
      for (i in 2:cols) {
        set(dt, j = paste0("V", sprintf(paste0("%0", floor(log10(cols)) + 1, "d"), i)), value = target[[i]])
      }
      
    } else {
      
      # Add columns by name
      for (i in names(target)[2:cols]) {
        set(dt, j = i, value = target[[i]])
      }
      
      # Fix first column name - comes last else alloc error (cf alloc.col)
      colnames(dt)[1] <- names(target)[1]
      
    }
    
  } else {
    
    # Fix column name for single list element
    if (!is.null(names(target))) {
      colnames(dt) <- names(target)
    }
    
  }
  
  return(dt)
  
}