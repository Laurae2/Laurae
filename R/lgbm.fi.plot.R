#' LightGBM Feature Importance Plotting
#'
#' This function allows to plot the feature importance on a LightGBM model.
#' 
#' @param model Type: list, data.table, or data.frame. The trained model (with feature importance), or the feature importance table. If a list is provided, the trained model must have had \code{importance} set to \code{TRUE} during training. Otherwise, compute manually the feature importance via \code{lgbm.fi}, and feed the output table to this function argument.
#' @param n_best Type: integer. The maximum amount of features to plot. Defaults to \code{50}.
#' @param is.cv Type: boolean. Whether the input is issued from a cross-validation or not. Defaults to \code{FALSE}.
#' @param multipresence Type: boolean. Whether in a cross-validation, only the features which always appear are kept. Otherwise, they are thrown away for safety. Defaults to \code{TRUE}.
#' @param plot Type: boolean. Whether to print a plot. Defaults to \code{TRUE}.
#' 
#' @return A ggplot2 object which contains the plot.
#' 
#' @examples
#' \dontrun{
#' # Feature importance on a single model without any tree limit.
#' feature_imp <- lgbm.fi(model = trained, feature_names = colnames(data), ntreelimit = 0)
#' feature_plot <- lgbm.fi.plot(feature_imp, is.cv = FALSE, multipresence = FALSE, plot = FALSE)
#' print(featuer_plot)
#' }
#' 
#' @export

lgbm.fi.plot <- function(model, n_best = 50, is.cv = FALSE, multipresence = TRUE, plot = TRUE) {
  
  # Check if a model or not was provided
  if (is.list(model)) {
    model <- model[["FeatureImp"]]
  }
  
  # Attempts to find the number of folds if CV is requested with multipresence
  if (is.cv & multipresence) {
    
    max_freq <- max(model$Freq)
    my_model <- model[model$Freq == max_freq, ]
    
  } else {
    
    my_model <- model
    
  }
  
  # n_best parsing
  my_model <- my_model[order(my_model$Gain_Rel_Ratio, decreasing = TRUE), ]
  if (nrow(my_model) > n_best) {
    my_model <- my_model[1:n_best, ]
  }
  
  # Helper functions for sorting
  my_model$Feature <- as.factor(my_model$Feature)
  reorder_size <- function(x) {
    factor(x, levels = names(sort(table(x))))
  }
  
  # CV + multipresence
  # CV alone
  # no CV
  
  if (is.cv & multipresence) {
    # CV + multipresence
    
    plotted <- ggplot(data = as.data.frame(my_model[, c("Feature", "Gain_Rel_Ratio", "Freq_Rel_Ratio")]), aes(x = reorder_size(Feature), y = Gain_Rel_Ratio, fill = Gain_Rel_Ratio)) + geom_bar(stat = "identity") + coord_flip() + labs(title = paste("Cross-Validated Feature Importance with Multipresence (Folds=", max_freq,")", sep = ""), y = "Gain") + scale_fill_gradient(name = "Relative Gain", low = "white", high = "red", trans = "log", breaks = c(0.005, 0.01, 0.025, 0.10, 0.50), labels = c("0.5%", "1%", "2.5%", "10%", "50%"))
    
  } else {
    
    if (is.cv) {
      # CV alone
      
      plotted <- ggplot(data = as.data.frame(my_model[, c("Feature", "Gain_Rel_Ratio", "Freq_Rel_Ratio")]), aes(x = reorder_size(Feature), y = Gain_Rel_Ratio, fill = Gain_Rel_Ratio, alpha = Freq_Rel_Ratio)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Cross-Validated Feature Importance", y = "Gain") + scale_fill_gradient(name = "Relative Gain", low = "white", high = "red", trans = "log", breaks = c(0.005, 0.01, 0.025, 0.10, 0.50), labels = c("0.5%", "1%", "2.5%", "10%", "50%")) + scale_alpha_continuous(name = "Fold Presence", limits = c(0, 1))
      
    } else {
      # no CV
      
      plotted <- ggplot(data = as.data.frame(my_model[, c("Feature", "Gain_Rel_Ratio")]), aes(x = reorder_size(Feature), y = Gain_Rel_Ratio, fill = Gain_Rel_Ratio, alpha = Freq_Rel_Ratio)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Feature Importance", y = "Gain") + scale_fill_gradient(name = "Relative Gain", low = "white", high = "red", trans = "log", breaks = c(0.005, 0.01, 0.025, 0.10, 0.50), labels = c("0.5%", "1%", "2.5%", "10%", "50%"))
      
    }
    
  }
  
  if (plot) plot(plotted)
  
  return(plotted)
  
}

