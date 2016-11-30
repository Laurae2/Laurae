#' Convert data.table to data.frame without copy
#'
#' This function allows to coerce a data.table to a data.frame without making in-memory copy. Therefore, you do not need 3x your current memory to coerce to a data.frame from a data.table.
#' 
#' @param x Type: data.table. The data.table to convert to data.frame.
#' 
#' @return Nothing (the data.table is transformed to a data.frame already)
#' 
#' @examples
#' library(data.table)
#' df <- data.frame(matrix(nrow = 100, ncol = 10))
#' coln <- colnames(df)
#' rown <- rownames(df)
#' setDT(df)
#' is.data.table(df) # Returns TRUE = OK
#' sum(c(coln == colnames(df), c(rown == rownames(df)))) # Returns 2 = OK
#' setDF(df)
#' is.data.table(df) # Returns FALSE = OK
#' sum(c(coln == colnames(df), c(rown == rownames(df)))) # Returns 2 = OK
#' 
#' @export

setDF <- function(x) {
  if (!is.data.table(x))
    stop("x must be a data.table")
  setattr(x, "row.names", .set_row_names(nrow(x)))
  setattr(x, "class", "data.frame")
  setattr(x, "sorted", NULL)
  setattr(x, ".internal.selfref", NULL)
}