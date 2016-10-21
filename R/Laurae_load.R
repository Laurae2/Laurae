# #' Laurae
# #' 
# #' Loads the external dependencies.
# #' 
# #' @return Printed messages of success or failure.
# #' 
# #' @import data.table
# #' 
# #' @importFrom grDevices dev.off
# #' @importFrom grDevices jpeg
# #' @importFrom stats cor
# #' @importFrom stats cov
# #' @importFrom stats mahalanobis
# #' @importFrom stats optim
# #' @importFrom stats predict
# #' @importFrom stats sd
# #' @importFrom utils read.csv
# #' @importFrom utils write.table
# #' @importFrom utils setTxtProgressBar
# #' @importFrom utils txtProgressBar
# #' 
# #' @examples
# #' \dontrun{
# #' Laurae_load()
# #' }
# #' 
# #' #@export
# 
# Laurae_load <- function() {
#   try(library(caret))
#   try(library(data.table))
#   try(library(Matrix))
#   try(library(outliers))
#   try(library(recommenderlab))
#   try(library(Rtsne))
#   try(library(R.utils))
#   try(library(stringi))
#   try(library(tabplot))
#   try(library(xgboost))
# }