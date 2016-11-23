#' @importFrom grDevices dev.off
#' @importFrom grDevices jpeg
#' @importFrom stats cor
#' @importFrom stats cov
#' @importFrom stats mahalanobis
#' @importFrom stats optim
#' @importFrom stats predict
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @importFrom stats reformulate
#' @importFrom utils read.csv
#' @importFrom utils write.table
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom stats na.omit
#' @import data.table

# Pass CRAN tests

globalVariables(c("Timing_Metric",
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
                  "setattr",
                  "ggplot",
                  "aes",
                  "geom_bar",
                  "coord_flip",
                  "labs",
                  "scale_fill_gradient",
                  "scale_alpha_continuous",
                  "scale_x_discrete",
                  "scale_y_continuous",
                  "scale_y_log10",
                  "theme_bw",
                  "write.svmlight",
                  "is",
                  "CEoptim",
                  "scatterplotMatrix",
                  "rpart",
                  "rpart.control",
                  "plotcp",
                  "rpart.plot"))

requireNamespace("Matrix")
requireNamespace("R.utils")
requireNamespace("Rtsne")
requireNamespace("outliers")
requireNamespace("recommenderlab")
requireNamespace("tabplot")
requireNamespace("xgboost")
requireNamespace("stringi")
requireNamespace("ggplot2")
requireNamespace("sparsity")
requireNamespace("CEoptim")
requireNamespace("car")
requireNamespace("rpart")
requireNamespace("rpart.plot")