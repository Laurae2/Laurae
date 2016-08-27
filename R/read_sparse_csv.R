#' Read sparse (numeric) CSVs
#'
#' This functions allows you to big load sparse numeric CSVs. Loading in chunks allows to not explode the memory as when the data is imported into R, it is typically a dense matrix.
#' Verbosity is automatic and cannot be removed. In case you need this function without verbosity, please compile the package after removing verbose messages.
#' 
#' @param input The input file name.
#' @param iterfeature The amount of variables loaded per iteration. The smaller the longer it takes to load the whole dataset in its entireity.
#' @param nfeatures The IDs of features to load. Defaults to \code{"NA"} which means loading all columns.
#' @param colClasses The classes of the columns. Defaults to \code{"NA"} which means autoselection as numeric. Do not modify (keep default).
#' @param RDS Whether to store in a RDS file of that name. Defaults to \code{"NA"} which means no RDS file. Otherwise, it takes \code{"RDS"} as filename.
#' @param compress_RDS Whether to compress RDS file. Defaults to \code{"TRUE"}
#' @param NA_sparse Whether sparsity is defined as NA. Defaults to \code{"FALSE"}
#' 
#' @return The sparse matrix
#' 
#' @examples 
#' read_sparse_csv("train_numeric.csv", iterfeature = 100, IDs = c(1:500, 601:1000), colClasses = NA, RDS = TRUE, compress_RDS = FALSE, NA_sparse = FALSE)
#' 
#' @export

read_sparse_csv <- function(input, iterfeature, nfeatures = NA, colClasses = NA, RDS = NA, compress_RDS = TRUE, NA_sparse = FALSE) {
  
  # Load columns to store what are the columns
  columns <- fread(input = input,
                   nrows = 0,
                   stringsAsFactors = FALSE,
                   colClasses = colClasses,
                   data.table = FALSE)
  
  # Assigns colClasses with numeric
  colClasses <- rep("numeric", nfeatures)
  
  if (is.na(nfeatures[1]) == TRUE) {
    
    # Get the amount of columns
    nfeatures <- ncol(columns)
    
  }
  
  # Create splits (iterations = ceiling(number of features / iterfeature))
  features <- split(nfeatures, ceiling(seq_along(nfeatures) / iterfeature))
  
  for (i in 1:length(features)) {
    
    cat("Loading ", i, "th part.\n", sep = "")
    data_temp <- fread(input = input,
                       select = features[[i]],
                       stringsAsFactors = FALSE,
                       colClasses = colClasses,
                       data.table = TRUE)
    gc(verbose = FALSE)
    if (i > 1) {
      if (NA_sparse == TRUE) {
        cat("Coercing to matrix.\n", sep = "")
        data_temp <- as.matrix(data_temp)
        gc(verbose = FALSE)
        cat("Coercing into dgCMatrix with NA as blank.\n", sep = "")
        data_temp <- dropNA(data_temp)
        gc(verbose = FALSE)
        cat("Column binding the full matrix with the newly created matrix.\n\n", sep = "")
        data <- cbind(data, data_temp)
        rm(data_temp)
      } else {
        cat("Coercing to sparse matrix.\n", sep = "")
        data_temp <- Matrix(as.matrix(data_temp), sparse = TRUE)
        gc(verbose = FALSE)
        cat("Column binding the full matrix with the newly created matrix.\n\n", sep = "")
        data <- cbind(data, data_temp)
        rm(data_temp)
      }
      
      gc(verbose = FALSE)
    } else {
      if (NA_sparse == TRUE) {
        cat("Coercing to matrix.\n", sep = "")
        data_temp <- as.matrix(data_temp)
        gc(verbose = FALSE)
        cat("Coercing into dgCMatrix with NA as blank.\n\n", sep = "")
        data <- dropNA(data_temp)
      } else {
        cat("Coercing to sparse matrix.\n", sep = "")
        data <- Matrix(as.matrix(data_temp), sparse = TRUE)
        rm(data_temp)
      }
      gc(verbose = FALSE)
    }
    
  }
  
  if (is.na(RDS) == FALSE) {
    cat("Saving to RDS format.\n")
    saveRDS(data, file = RDS, compress = compress_RDS)
  }
  
  return(data)
  
}
