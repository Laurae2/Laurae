#' LightGBM Feature Importance Plotting
#'
#' This function allows to plot the feature importance on a LightGBM model.
#' 
#' @param model Type: list, data.table, or data.frame. The trained model (with feature importance), or the feature importance table. If a list is provided, the trained model must have had \code{importance} set to \code{TRUE} during training. Otherwise, compute manually the feature importance via \code{lgbm.fi}, and feed the output table to this function argument.
#' @param n_best Type: integer. The maximum amount of features to plot. Defaults to \code{50}.
#' @param no_log Type: boolean. Whether to NOT apply a log10 scale to the plot. Defaults to \code{TRUE}.
#' @param low Type: character. The color when the relative gain is \code{0}. Defaults to \code{"white"}.
#' @param high Type: character. The color when the relative gain is \code{1}. Defaults to \code{"red"}.
#' @param rescaler Type: character. The transformation of the color scale. Defaults to \code{"log"}. Choose between \code{"asn"}, \code{"atanh"}, \code{"boxcox"}, \code{"exp"}, \code{"identity"} (linear scale), \code{"log"}, \code{"log10"}, \code{"log1p"}, \code{"log2"}, \code{"logit"}, \code{"probability"}, \code{"probit"}, \code{"reciprocal"}, \code{"reverse"}, \code{"sqrt"}, or any other ggplot2 transformation object. 
#' @param is.cv Type: boolean. Whether the input is issued from a cross-validation or not. Defaults to \code{FALSE}.
#' @param multipresence Type: boolean. Whether in a cross-validation, only the features which always appear are kept. Otherwise, they are thrown away for safety. Defaults to \code{TRUE}.
#' @param plot Type: boolean. Whether to print a plot. Defaults to \code{TRUE}.
#' 
#' @return A ggplot2 object which contains the plot.
#' 
#' @examples
#' \dontrun{
#' # Feature importance on a single model without any tree limit, 20 best features are plotted.
#' feature_imp <- lgbm.fi(model = trained, feature_names = colnames(data), ntreelimit = 0)
#' feature_plot <- lgbm.fi.plot(feature_imp, n_best = 20, rescaler = "log",
#' is.cv = FALSE, multipresence = FALSE, plot = FALSE)
#' print(featuer_plot)
#' }
#' 
#' @export

lgbm.fi.plot <- function(model, n_best = 50, no_log = TRUE, low = "white", high = "red", rescaler = "log", is.cv = FALSE, multipresence = TRUE, plot = TRUE) {
  
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
  sorted_features <- rev(my_model$Feature)
  
  # CV + multipresence
  # CV alone
  # no CV
  
  if (is.cv & multipresence) {
    # CV + multipresence
    
    plotted <- ggplot(data = as.data.frame(my_model[, c("Feature", "Gain_Rel_Ratio")]), aes(x = Feature, y = Gain_Rel_Ratio, fill = Gain_Rel_Ratio)) + geom_bar(stat = "identity", position = "identity") + theme_bw() + coord_flip() + labs(title = paste("Cross-Validated Feature Importance with Multipresence (Folds=", max_freq,")", sep = ""), y = "Gain") + scale_x_discrete(limits = sorted_features) + scale_fill_gradient(name = "Relative Gain", low = low, high = high, trans = rescaler, breaks = c(0.005, 0.01, 0.025, 0.10, 0.33), labels = c("0.5%", "1%", "2.5%", "10%", "33%"))
    
  } else {
    
    if (is.cv) {
      # CV alone
      
      plotted <- ggplot(data = as.data.frame(my_model[, c("Feature", "Gain_Rel_Ratio", "Freq_Rel_Ratio")]), aes(x = Feature, y = Gain_Rel_Ratio, fill = Gain_Rel_Ratio, alpha = Freq_Rel_Ratio)) + geom_bar(stat = "identity", position = "identity") + theme_bw() + coord_flip() + labs(title = "Cross-Validated Feature Importance", y = "Gain") + scale_x_discrete(limits = sorted_features) + scale_fill_gradient(name = "Relative Gain", low = low, high = high, trans = rescaler, breaks = c(0.005, 0.01, 0.025, 0.10, 0.33), labels = c("0.5%", "1%", "2.5%", "10%", "33%")) + scale_alpha_continuous(name = "Fold Presence", limits = c(0, 1))
      
    } else {
      # no CV
      
      plotted <- ggplot(data = as.data.frame(my_model[, c("Feature", "Gain_Rel_Ratio")]), aes(x = Feature, y = Gain_Rel_Ratio, fill = Gain_Rel_Ratio)) + geom_bar(stat = "identity", position = "identity") + theme_bw() + coord_flip() + labs(title = "Feature Importance", y = "Gain") + scale_x_discrete(limits = sorted_features) + scale_fill_gradient(name = "Relative Gain", low = low, high = high, trans = rescaler, breaks = c(0.005, 0.01, 0.025, 0.10, 0.33), labels = c("0.5%", "1%", "2.5%", "10%", "33%"))
      
    }
    
  }
  
  if (!no_log) {
    plotted <- plotted + scale_y_log10(name = "Relative Gain (Log10 Scale)", breaks = c(0.005, 0.01, 0.025, 0.10, 0.33, 1.00), labels = c("0.5%", "1%", "2.5%", "10%", "33%", "100%"))
  } else {
    plotted <- plotted + scale_y_continuous(name = "Relative Gain", breaks = c(0.00, 0.20, 0.40, 0.60, 0.80, 1.00), labels = c("0%", "20%", "40%", "60%", "80%", "100%"))
  }
  
  if (plot) plot(plotted)
  
  return(plotted)
  
}

