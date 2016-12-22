#' @importFrom grDevices dev.off
#' @importFrom grDevices jpeg
#' @importFrom grDevices col2rgb
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics curve
#' @importFrom graphics pie
#' @importFrom stats cor
#' @importFrom stats cov
#' @importFrom stats mahalanobis
#' @importFrom stats optim
#' @importFrom stats predict
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @importFrom stats reformulate
#' @importFrom stats complete.cases
#' @importFrom stats na.omit
#' @importFrom stats dnorm
#' @importFrom stats quantile
#' @importFrom stats var
#' @importFrom utils read.csv
#' @importFrom utils write.table
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom utils browseURL
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
                  "rpart.plot",
                  "Deriv",
                  "getinfo",
                  "xgb.importance",
                  "%>%",
                  "datatable",
                  "formatStyle",
                  "styleColorBar",
                  "formatPercentage",
                  "render",
                  "report_dir",
                  "acc",
                  "cb.evaluation.log",
                  "data",
                  "f1s",
                  "fall",
                  "fn_v",
                  "fp_v",
                  "mcc",
                  "pExp",
                  "pObs",
                  "prec",
                  "sens",
                  "spec",
                  "tn_v",
                  "tp_v",
                  "y_true",
                  "miss",
                  "box",
                  "dashboardBody",
                  "dashboardHeader",
                  "dashboardPage",
                  "dashboardSidebar",
                  "dropdownMenu",
                  "f",
                  "fluidRow",
                  "icon",
                  "messageItem",
                  "plotOutput",
                  "renderPlot",
                  "shinyApp",
                  "textInput",
                  "sidebarMenu",
                  "verbatimTextOutput",
                  "selectInput",
                  "observeEvent",
                  "updateSelectInput",
                  "renderText",
                  "actionButton",
                  "as.party",
                  "isolate",
                  "numericInput",
                  "renderPrint",
                  "renderTable",
                  "tableOutput",
                  "updateNumericInput",
                  "breaks_split",
                  "data_party",
                  "id_node",
                  "index_split",
                  "kids_node",
                  "node_party",
                  "nodeids",
                  "right_split",
                  "split_node",
                  "varid_split",
                  "showOutput",
                  "withProgress",
                  "renderChart2",
                  "brewer.pal",
                  "conditionalPanel",
                  "plotlyOutput",
                  "renderPlotly",
                  "sliderInput",
                  "toRGB",
                  "width.SJ",
                  "x",
                  "predicted",
                  "mini"))

requireNamespace("xgboost")
requireNamespace("rpart")
requireNamespace("rpart.plot")
requireNamespace("partykit")
requireNamespace("tabplot")
requireNamespace("rCharts")
requireNamespace("plotly")
requireNamespace("ggplot2")
requireNamespace("ggthemes")
requireNamespace("plotluck")
requireNamespace("grid")
requireNamespace("gridExtra")
requireNamespace("RColorBrewer")
requireNamespace("lattice")
requireNamespace("car")
requireNamespace("CEoptim")
requireNamespace("DT")
requireNamespace("formattable")
requireNamespace("rmarkdown")
requireNamespace("shiny")
requireNamespace("shinydashboard")
requireNamespace("Matrix")
requireNamespace("matrixStats")
requireNamespace("R.utils")
requireNamespace("Rtsne")
requireNamespace("recommenderlab")
requireNamespace("sparsity")
requireNamespace("RcppArmadillo")
requireNamespace("Deriv")
requireNamespace("outliers")
requireNamespace("MASS")
requireNamespace("stringi")
