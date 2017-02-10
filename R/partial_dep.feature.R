#' Partial Dependency, output analyzer
#'
#' This function is a helper for analyzing partial dependency output. It uses information theory to assess the score of a predictor against the label.
#' 
#' There are multiple outputs, all information theory -related values (entropy, mutual information, synergy, total correlation) are provided with the empirical probability distribution by default. One can change them using the parmaeter \code{method}, but it is recommended to leave the default on unless you know what you are doing:
#' 
#' \describe{
#'   \item{"emp"}{(Default) Entropy of the empirical probability distribution.}
#'   \item{"mm"}{Miller-Madow asymptotic bias corrected empirical estimator.}
#'   \item{"shrink"}{Shrinkage estimate of the entropy of a Dirichlet probability distribution.}
#'   \item{"sg"}{Schurmann-Grassberger estimate of the entropy of a Dirichlet probability distribution.}
#' }
#' 
#' Use the function \code{infotheo::natstobits(value)} to convert from nats (\code{base e}) to bits (\code{base 2}).
#' 
#' Table \code{"Features"}:
#' 
#' \describe{
#'   \item{"Feature"}{Column name assessed.}
#'   \item{"Unique_Values"}{Count of unique values of the feature.}
#'   \item{"Entropy"}{Entropy of the feature.}
#'   \item{"Label_Mutual_Info"}{Mutual Information between Feature and Label.}
#'   \item{"Evolution_Mutual_Info"}{Mutual Information between Feature and Evolution.}
#'   \item{"p.Mann_Kendall"}{\code{in_depth = TRUE} p-value of the Partial Mann-Kendall (multivariate) test to detect non-parametric monotonic trends in potentially seasonal data. Low value means confidence in a trend.}
#'   \item{"p.Partial_Spearman"}{\code{in_depth = TRUE} p-value of the Partial Spearman Correlation trend test. Low value means confidence in a correlation greater than 0.}
#' }
#' 
#' Table \code{"Global"}:
#' 
#' \describe{
#'   \item{"Without_Feature"}{Column name NOT assessed.}
#'   \item{"Synergy"}{Synergy/Complementarity (inter information) provided by the data without the feature.}
#'   \item{"Total_Correlation"}{Total Correlation (multi information) provided by the data without the feature.}
#' }
#' 
#' @param grid_data Type: data.table. A \code{partial_dep} \code{grid_exp} output.
#' @param in_depth Type: logical. Whether to perform Partial Mann-Kendall test.
#' @param method Type: character. The method to use to compute information theory -related values. Defaults to \code{"emp"}, see details for more information.
#' 
#' @return Value statistics of the input for variability per column.
#' 
#' @export

partial_dep.feature <- function(grid_data, method = "emp", in_depth = FALSE) {
  
  # Initialize temporary data.table and dummy variables
  exploration <- 1:(ncol(grid_data) - 2) # What to loop over
  label_column <- as.numeric(as.factor(grid_data[[colnames(grid_data)[ncol(grid_data) - 1]]])) # The label column, discretized
  evolution_column <- as.numeric(as.factor(grid_data[[colnames(grid_data)[ncol(grid_data)]]])) # The evolution column, discretized
  
  if (in_depth == FALSE) {
    
    summary_table <- data.table(Feature = colnames(grid_data)[exploration],
                                Unique_Values = rep(0, max(exploration)),
                                Entropy = rep(0, max(exploration)),
                                Label_Mutual_Info = rep(0, max(exploration)),
                                Evolution_Mutual_Info = rep(0, max(exploration))) # The data.table
    
  } else {
    
    summary_table <- data.table(Feature = colnames(grid_data)[exploration],
                                Unique_Values = rep(0, max(exploration)),
                                Entropy = rep(0, max(exploration)),
                                Label_Mutual_Info = rep(0, max(exploration)),
                                Evolution_Mutual_Info = rep(0, max(exploration)),
                                p.Mann_Kendall = rep(0, max(exploration)),
                                p.Partial_Spearman = rep(0, max(exploration))) # The data.table
    
  }
  
  feature_table <- data.table(grid_data[1:nrow(grid_data), ..exploration])
  
  browser()
  
  # Loop over each feature, does not care about order, does not care about spacing
  for (i in exploration) {
    
    # Store temporarily the feature in a dedicated variable
    temp_feature <- grid_data[[i]]
    
    # Destroy any non-numeric relationship, any length relationship
    temp_feature <- as.numeric(as.factor(temp_feature))
    
    # Replace original content in the feature table
    feature_table[[i]] <- temp_feature
    
    # Count all unique values
    summary_table[i, "Unique_Values"] <- length(unique(temp_feature))
    
    # Compute entropy
    summary_table[i, "Entropy"] <- infotheo::entropy(X = temp_feature, method = method)
    
    # Compute mutual information
    summary_table[i, "Label_Mutual_Info"] <- infotheo::mutinformation(X = temp_feature, Y = label_column, method = method)
    summary_table[i, "Evolution_Mutual_Info"] <- infotheo::mutinformation(X = temp_feature, Y = evolution_column, method = method)
    
    if (in_depth == TRUE) {
      
      # Compute Partial Mann-Kendall test (vertical leakage)
      summary_table[i, "p.Mann_Kendall"] <- trend::partial.mk.test(x = temp_feature, z = label_column)$p.value
      
      # Compute Partial Pearson trend test (horizontal leakage)
      summary_table[i, "p.Partial_Spearman"] <- trend::partial.cor.trend.test(x = temp_feature, z = label_column, method = "spearman")$p.value
      
    }
    
    
  }
  
  # Add synergy and total information statistics
  known_info <- data.table(Without_Feature = c("None (All)", colnames(grid_data)[exploration]),
                           Synergy = numeric(1 + max(exploration)),
                           Total_Correlation = numeric(1 + max(exploration)))
  known_info[1, "Synergy"] <- infotheo::interinformation(X = feature_table, method = method)
  known_info[1, "Total_Correlation"] <- infotheo::multiinformation(X = feature_table, method = method)
  
  # Loop over each feature for global statistics
  for (i in exploration) {
    
    select_i <- exploration[-i]
    feature_temp <- feature_table[, ..select_i]
    known_info[i + 1, "Synergy"] <- infotheo::interinformation(X = feature_temp, method = method)
    known_info[i + 1, "Total_Correlation"] <- infotheo::multiinformation(X = feature_temp, method = method)
    
  }
  
  return(list(Features = summary_table,
              Global = known_info))
  
}