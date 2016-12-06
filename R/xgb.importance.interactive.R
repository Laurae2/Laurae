#' xgboost feature importance interactive table
#'
#' This function interactively plots the table of the feature importance from a xgboost model, provided you give the feature names.
#' 
#' @param importance Type: xgboost model, data.table, or data.frame. Your xgboost model to get feature importance from. Or a data.table/data.frame.
#' @param feature_names Type: vector of characters. The feature names to use in the feature importance. If you provide a data.table/data.frame, do not input this argument. Defaults to \code{NULL}.
#' @param digits Type: integer. The number of digits to print in the table.
#' @param pageLength Type: integer. The number of features to print in a single page.
#' @param lengthMenu Type: vector of integers. The selectable number of features to print in a single page.
#' 
#' @return The datatable printed.
#' 
#' @examples
#' \dontrun{
#' # Todo
#' }
#' 
#' @export

xgb.importance.interactive <- function(importance, feature_names = NULL, digits = 6, pageLength = 20, lengthMenu = c(5, 10, 15, 20, 25, 50, 100, 500)) {
  
  if (!is.data.frame(importance)) {
    importance <- as.data.frame(xgb.importance(feature_names = feature_names, model = importance))
  }
  
  datatable(importance,
            filter = "top",
            class = "cell-border stripe",
            options = list(pageLength = pageLength,
                           lengthMenu = lengthMenu)
  ) %>% formatStyle('Gain',
                    background = styleColorBar(range(importance$Gain, na.rm = TRUE, finite = TRUE), 'lightgreen'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
    formatStyle('Cover',
                background = styleColorBar(range(importance$Cover, na.rm = TRUE, finite = TRUE), 'lightblue'),
                backgroundSize = '100% 90%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center') %>%
    formatStyle('Frequency',
                background = styleColorBar(range(importance$Frequency, na.rm = TRUE, finite = TRUE), 'lightgrey'),
                backgroundSize = '100% 90%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center') %>%
    formatPercentage(columns = c("Gain"),
                     digits = digits) %>%
    formatPercentage(columns = c("Cover"),
                     digits = digits) %>%
    formatPercentage(columns = c("Frequency"),
                     digits = digits)
  
}

