#' Laurae Package Loader
#'
#' Tries to load ALL the external dependencies.
#'
#' Loaded packages:
#'
#' \describe{
#'   \item{xgboost}{Extreme Gradient Boosting}
#'   \item{lightgbm}{Light Gradient Boosting Machine}
#'   \item{foreach}{Provide Foreach Looping Construct for R}
#'   \item{doParallel}{Foreach Parallel Adaptor for the 'parallel' Package}
#'   \item{rpart}{Recursive Partitioning and Regression Trees}
#'   \item{rpart.plot}{Plot 'rpart' Models: An Enhanced Version of 'plot.rpart'}
#'   \item{partykit}{A Toolkit for Recursive Partitioning}
#'   \item{tabplot}{Tableplot, a visualization of large datasets}
#'   \item{rCharts}{Interactive Charts using Javascript Visualization Libraries}
#'   \item{plotly}{Create Interactive Web Graphics via 'plotly.js'}
#'   \item{ggplot2}{Create Elegant Data Visualisations Using the Grammar of Graphics}
#'   \item{GGally}{Extension to 'ggplot2'}
#'   \item{ggthemes}{Extra Themes, Scales and Geoms for 'ggplot2'}
#'   \item{plotluck}{"ggplot2" Version of "I'm Feeling Lucky!"}
#'   \item{grid}{The Grid Graphics Package}
#'   \item{gridExtra}{Miscellaneous Functions for "Grid" Graphics}
#'   \item{RColorBrewer}{ColorBrewer Palettes}
#'   \item{lattice}{Trellis Graphics for R}
#'   \item{car}{Commpanion to Applied Regression}
#'   \item{CEoptim}{Cross-Entropy R Package for Optimization}
#'   \item{DT}{A Wrapper of the JavaScript Library 'DataTables'}
#'   \item{formattable}{Create 'Formattable' Data Structures}
#'   \item{rmarkdown}{Dynamic Documents for R}
#'   \item{shiny}{Web Application Framework for R}
#'   \item{shinydashboard}{Create Dashboards with 'Shiny'}
#'   \item{Matrix}{Sparse and Dense Matrix Classes and Methods}
#'   \item{matrixStats}{Functions that Apply to Rows and Columns of Matrices (and to Vectors)}
#'   \item{R.utils}{Various Programming Utilities}
#'   \item{Rtsne}{T-Distributed Stochastic Neighbor Embedding using a Barnes-Hut Implementation}
#'   \item{recommenderlab}{Lab for Developing and Testing Recommender Algorithms}
#'   \item{sparsity}{What the package does (short line)}
#'   \item{RcppArmadillo}{'Rcpp' Integration for the 'Armadillo' Templated Linear Algebra Library}
#'   \item{mgcv}{Mixed GAM Computation Vehicle with GCV/AIC/REML Smoothness Estimation}
#'   \item{Deriv}{Symbolic Differentiation}
#'   \item{outliers}{Tests for outliers}
#'   \item{MASS}{Support Functions and Datasets for Venables and Ripley's MASS}
#'   \item{infotheo}{Information-Theoretic Measures}
#'   \item{trend}{Non-Parametric Trend Tests and Change-Point Detection}
#'   \item{stringi}{Character String Processing Facilities}
#' }
#'
#' @return Printed messages of success or failure.
#'
#' @examples
#' \dontrun{
#' # Load all dependencies.
#' Laurae_load()
#' }
#'
#' @export

Laurae_load <- function() {
  # try(library(xgboost))
  # try(library(lightgbm))
  # try(library(foreach))
  # try(library(doParallel))
  # try(library(rpart))
  # try(library(rpart.plot))
  # try(library(partykit))
  # try(library(tabplot))
  # try(library(rCharts))
  # try(library(plotly))
  # try(library(ggplot2))
  # try(library(GGally))
  # try(library(ggthemes))
  # try(library(plotluck))
  # try(library(grid))
  # try(library(gridExtra))
  # try(library(RColorBewer))
  # try(library(lattice))
  # try(library(car))
  # try(library(CEoptim))
  # try(library(DT))
  # try(library(formattable))
  # try(library(rmarkdown))
  # try(library(shiny))
  # try(library(shinydashboard))
  # try(library(Matrix))
  # try(library(matrixStats))
  # try(library(R.utils))
  # try(library(Rtsne))
  # try(library(recommenderlab))
  # try(library(sparsity))
  # try(library(RcppArmadillo))
  # try(library(mgcv))
  # try(library(Deriv))
  # try(library(outliers))
  # try(library(MASS))
  # try(library(infotheo))
  # try(library(trend))
  # try(library(stringi))
  eval(parse(text = "try(library(xgboost));try(library(lightgbm));try(library(foreach));try(library(doParallel));try(library(rpart));try(library(rpart.plot));try(library(partykit));try(library(tabplot));try;try(library(rCharts));try(library(plotly));try(library(ggplot2));try(library(GGally));try(library(ggthemes));try(library(plotluck));try(library(grid));try(library(gridExtra));try(library(RColorBrewer));try(library(lattice));try(library(car));try(library(CEoptim));try(library(DT));try(library(formattable));try(library(rmarkdown));try(library(shiny));try(library(shinydashboard));try(library(Matrix));try(library(matrixStats));try(library(R.utils));try(library(Rtsne));try(library(recommenderlab));try(library(sparsity));try(library(RcppArmadillo));try(library(mgcv));try(library(Deriv));try(library(outliers));try(library(MASS));try(library(infotheo));try(library(trend));try(library(stringi))"))
}



