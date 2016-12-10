#' Linear Regression Modeling HTML report
#'
#' This function creates a linear regression report as a HTML file. Cross-validation is possible.
#' 
#' @param data Type: data.table. The data to fit a linear regression model on.
#' @param label Type: vector. The label the data must fit to.
#' @param folds Type: list of numeric vectors. The folds used.
#' @param normalize Type: boolean. Whether features should be normalized before being fed to the linear model. Defaults to \code{TRUE}.
#' @param cleaning Type: boolean. Whether NAs are set to 0 (if there are any). Defaults to \code{TRUE}.
#' @param deficiency Type: boolean. Whether rank deficiency computation is done (kappa). Defaults to \code{TRUE}.
#' @param stats Type: boolean. Whether machine learning statistics should be output for model performance diagnosis. When \code{TRUE}, also returns the metrics and the out of fold predictions. Defaults to \code{TRUE}.
#' @param coefficients Type: boolean. Whether feature coefficients should be output. Defaults to \code{TRUE}.
#' @param adv_stats Type: boolean. Whether advanced statistics should be done to analyze in depth the data. Defaults to \code{TRUE}.
#' @param plots Type: boolean. Whether plotting of fitted values vs predicted values should be done. Defaults to \code{TRUE}.
#' @param output_file Type: character. The output report file name. Defaults to \code{"report.lm.html"}.
#' @param output_dir Type: character. The output report directory name. Defaults to \code{getwd()}.
#' @param open_file Type: boolean. Whether to open the output report once it has finished computing. Defaults to \code{TRUE}.
#' @param ... Other arguments to pass to \code{rmarkdown::render}.
#' 
#' @return Returns a list with the machine learning metrics (\code{"Metrics"}), the folds \code{"Folds"}, the fitted values per fold (\code{"Fitted"}), the predicted values per fold (\code{"Predicted"}) if they were computed. Otherwise, returns \code{TRUE}.
#' 
#' @examples
#' # No example.
#' \dontrun{
#'   library(rmarkdown)
#'   library(DT)
#'   library(formattable)
#'   library(RcppArmadillo)
#' }
#' 
#' @export

report.lm <- function(data, label, folds, normalize = TRUE, cleaning = TRUE, deficiency = TRUE, stats = TRUE, coefficients = TRUE, adv_stats = TRUE, plots = TRUE, output_file = "report.lm.html", output_dir = getwd(), open_file = TRUE, ...) {
  
  stats_table <- fitted_values <- fitted_predicted <- 0 # Avoid CRAN issue
  
  # A numeric fold?
  if (length(folds) == 1) {
    folds <- kfold(y = label, k = folds, stratified = TRUE, seed = 0, named = TRUE)
  }
  
  # Create report
  reporting_file <- system.file("template_rmd/LinearRegression.rmd", package = "Laurae")
  render(input = reporting_file,
         output_file = output_file,
         output_dir = output_dir,
         intermediates_dir = output_dir,
         params = list(data = data, label = label, folds = folds, normalize = normalize, cleaning = cleaning, deficiency = deficiency, stats = stats, coefficients = coefficients, adv_stats = adv_stats, plots = plots, fun_options = list()),
         envir = environment(),
         ...)
  
  # Open file?
  if (open_file) {
    report_path <- file.path(output_dir, output_file)
    browseURL(report_path)
  }
  
  # Return stats?
  if (stats) {
    return(list(Metrics = stats_table, Folds = folds, Fitted = fitted_values, Predicted = fitted_predicted))
  } else {
    return(TRUE)
  }
  
}
