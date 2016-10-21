#' @importFrom grDevices dev.off
#' @importFrom grDevices jpeg
#' @importFrom stats cor
#' @importFrom stats cov
#' @importFrom stats mahalanobis
#' @importFrom stats optim
#' @importFrom stats predict
#' @importFrom stats sd
#' @importFrom utils read.csv
#' @importFrom utils write.table
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @import data.table

# Pass tests

globalVariables(c("seed",
                  "ID",
                  "Gain_Sum",
                  "Feature",
                  "Freq_Rel_Ratio",
                  "Freq",
                  "Freq_Abs_Ratio",
                  "Gain_Rel_Ratio",
                  "Gain",
                  "Gain_Abs_Ratio",
                  "Gain_Std_Rel_Ratio",
                  "Gain_Std",
                  "Gain_Std_Abs_Ratio",
                  "Laurae.xgb.opt.depth.best",
                  "Laurae.xgb.opt.depth.df",
                  "Laurae.xgb.opt.depth.iter",
                  "Matrix",
                  "Rtsne",
                  "System",
                  "createMultiFolds",
                  "datatable_target",
                  "dropNA",
                  "scores",
                  "stri_replace_last_fixed",
                  "tablePrepare",
                  "tableplot",
                  "test",
                  "train",
                  "train_temp",
                  "xgb.DMatrix",
                  "xgb.cv",
                  "xgb.train",
                  "set",
                  "data.table",
                  "alloc.col",
                  "set",
                  "is.data.table",
                  "setDT",
                  "fwrite",
                  "as.data.table",
                  "setDT",
                  "setcolorder",
                  "fread",
                  ":=",
                  "setattr"))

requireNamespace("Matrix")
requireNamespace("R.utils")
requireNamespace("Rtsne")
requireNamespace("caret")
requireNamespace("outliers")
requireNamespace("recommenderlab")
requireNamespace("tabplot")
requireNamespace("xgboost")
requireNamespace("stringi")