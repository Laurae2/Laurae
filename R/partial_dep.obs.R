#' Partial Dependency Observation, Contour (single observation)
#'
#' This function computes partial dependency of a supervised machine learning model over a range of values for a single observation. Does not work for multiclass problems! Check \code{predictor_xgb} to get an example of \code{predictor} to use (so you can create your own).
#' 
#' @param model Type: unknown. The model to pass to \code{predictor}.
#' @param predictor Type: function(model, data). The predictor function which takes a model and data as inputs, and return predictions. \code{data} is provided as data.table for maximum performance.
#' @param data Type: data.table (mandatory). The data we need to use to sample from for the partial dependency with \code{observation}.
#' @param observation Type: data.table (mandatory). The observation we want to get partial dependence from. It is mandatory to use a data.table to retain column names.
#' @param column Type: character. The column we want partial dependence from. You can specify two or more \code{column} as a vector, but it is highly not recommended to go for a lot of columns because the complexity is exponential, think as \code{O^length(column)}. For instance, \code{accuracy = 100} and \code{length(column) = 10} leads to \code{1e+20} theoretical observations, which will explode the memory of any computer.
#' @param accuracy Type: integer. The accuracy of the partial dependence from, exprimed as number of sampled points by percentile of the \code{column} from the the \code{data}. Defaults to \code{min(length(data), 100)}, which means either 100 samples or all samples of \code{data} if the latter has less than 100 observations.
#' @param safeguard Type: logical. Whether to safeguard \code{accuracy^length(column)} value to \code{safeguard_val} observations maximum. If \code{TRUE}, it will prevent that value to go over \code{safeguard_val} (and adjust accordingly the \code{accuracy} value). Note that if safeguard is disabled, you might get at the end less observations than you expected initially (there is cleaning performed for uniqueness.
#' @param safeguard_val Type: integer. The maximum number of observations allowed when \code{safeguard} is \code{TRUE}. Defaults to \code{1048576}, which is \code{4^10}.
#' @param exact_only Type: logical. Whether to select only exact values for data sampling. Defaults to \code{TRUE}.
#' @param label_name Type: character. The column name given to the predicted values in the output table. Defaults to \code{"Target"}, this assumes you do not have a column called \code{"Target"} in your \code{column} vector.
#' @param comparator_name Type: character. The column name given to the evolution value (\code{"Increase"}, \code{"Fixed"}, \code{"Decrease"}) in the output table. Defaults to \code{"Evolution"}, this assumes you do not have a column called \code{"Evolution"} in your \code{column} vector.
#' 
#' @return A list with different elements: \code{grid_init} for the grid before expansion, \code{grid_exp} for the expanded grid with predictions, \code{preds} for the predictions, and \code{obs} for the original prediction on observation.
#' 
#' @examples
#' \dontrun{
#' # Let's load a dummy dataset
#' data(mtcars)
#' setDT(mtcars) # Transform to data.table for easier manipulation
#' 
#' # We train a xgboost model on 31 observations, keep last to analyze later
#' set.seed(0)
#' xgboost_model <- xgboost(data = data.matrix(mtcars[-32, -1]),
#'                          label = mtcars$mpg[-32],
#'                          nrounds = 20)
#' 
#' # Perform partial dependence grid prediction to analyze the behavior of the 32th observation
#' # We want to check how it behaves with:
#' # => horsepower (hp)
#' # => number of cylinders (cyl)
#' # => transmission (am)
#' # => number of carburetors (carb)
#' preds_partial <- partial_dep.obs(model = xgboost_model,
#'                                  predictor = predictor_xgb, # Default for xgboost
#'                                  data = mtcars[-32, -1], # train data = 31 first observations
#'                                  observation = mtcars[32, -1], # 32th observation to analyze
#'                                  column = c("hp", "cyl", "am", "carb"),
#'                                  accuracy = 20, # Up to 20 unique values per column
#'                                  safeguard = TRUE, # Prevent high memory usage
#'                                  safeguard_val = 1048576, # No more than 1048576 observations,
#'                                  exact_only = TRUE, # Not allowing approximations,
#'                                  label_name = "mpg", # Label is supposed "mpg"
#'                                  comparator_name = "evo") # Comparator +/-/eq for analysis
#' 
#' # How many observations? 300
#' nrow(preds_partial$grid_exp)
#' 
#' # How many observations analyzed per column? hp=10, cyl=3, am=2, carb=5
#' summary(preds_partial$grid_init)
#' 
#' # When cyl decreases, mpg increases!
#' partial_dep.plot(grid_data = preds_partial$grid_exp,
#'                  backend = "tableplot",
#'                  label_name = "mpg",
#'                  comparator_name = "evo")
#' 
#' # Another way of plotting... hp/mpg relationship is not obvious
#' partial_dep.plot(grid_data = preds_partial$grid_exp,
#'                  backend = "car",
#'                  label_name = "mpg",
#'                  comparator_name = "evo")
#' 
#' # Do NOT do this on >1k samples, this will kill RStudio
#' # Histograms make it obvious when decrease/increase happens.
#' partial_dep.plot(grid_data = preds_partial$grid_exp,
#'                  backend = "plotly",
#'                  label_name = "mpg",
#'                  comparator_name = "evo")
#' 
#' # Get statistics to analyze fast
#' partial_dep.feature(preds_partial$grid_exp, metric = "emp", in_depth = FALSE)
#' 
#' # Get statistics to analyze, but is very slow when there is large data
#' # Note: unreliable for large amount of observations due to asymptotic infinites
#' partial_dep.feature(preds_partial$grid_exp, metric = "emp", in_depth = TRUE)
#' }
#' 
#' @export

partial_dep.obs <- function(model, predictor, data, observation, column, accuracy = min(length(data), 100), safeguard = TRUE, safeguard_val = 1048576, exact_only = TRUE, label_name = "Target", comparator_name = "Evolution") {
  
  # Prepare dummy variable holding grid search
  grid_search <- list()
  
  # Check whether the user has supplied bad accuracy values
  if (accuracy > length(data)) {
    
    # Correct number of samples needed
    accuracy <- length(data)
    
  }
  
  # Count how many observations we will really need for partial dependence
  count_obs <- accuracy^length(column)
  
  # User has enabled safeguard, enabled by default
  if (safeguard == TRUE) {
    
    # Did user bypass the limit with safeguard on?
    if (count_obs > safeguard_val) {
      
      # Find best accuracy value
      best_accuracy <- floor(safeguard_val^(1/length(column)))
      best_obs <- best_accuracy^length(column)
      
      # Print error
      warning(paste0("Warning: Laurae::partial_dep safeguard has been triggered: from ", count_obs, "(", accuracy, ") to ", best_obs, "(", best_accuracy, ")."))
      
      # Reset initial variables
      accuracy <- best_accuracy
      count_obs <- best_obs
      
    }
    
  }
  
  # Prepare uniform length vector of percentile
  temp_percentile <- (1/accuracy) * (1:accuracy)
  
  # Rescale percentile to [0, 1]
  temp_percentile <- (temp_percentile - 1/accuracy) / (1 - 1/accuracy)
  
  # Harvest percentiles of interest, inexact method
  if (exact_only == FALSE) {
    
    # Loop over each column
    for (i in 1:length(column)) {
      
      # Store values temporarily for percentile computation
      temp_values <- data[[column[i]]]
      unique_values <- unique(temp_values)
      
      # Value has less unique values than what we expect to have
      if (length(unique_values) < accuracy) {
        
        # Get exact unique values
        grid_search[[i]] <- sort(unique_values)
        
      } else {
        
        # Get approximated percentiles, S style (continuous)
        grid_search[[i]] <- sort(unique(quantile(data[[column[i]]], temp_percentile, na.rm = TRUE, names = FALSE, type = 7)))
        
      }
      
      
    }
    
  } else {
    
    # Exact method of percentiles
    
    # Loop over each column
    for (i in 1:length(column)) {
      
      # Store values temporarily for percentile computation
      temp_values <- data[[column[i]]]
      unique_values <- unique(temp_values)
      
      # Value has less unique values than what we expect to have
      if (length(unique_values) < accuracy) {
        
        # Get exact unique values
        grid_search[[i]] <- sort(unique_values)
        
      } else {
        
        # Get exact percentiles, SAS style, (discontinuous)
        grid_search[[i]] <- sort(unique(quantile(data[[column[i]]], temp_percentile, na.rm = TRUE, names = FALSE, type = 3)))
        
      }
      
      
    }
    
  }
  
  # Give names to grid search list
  names(grid_search) <- column
  
  # Explode grid search
  best_grid <- data.table(expand.grid(grid_search))
  
  # Explode data.table
  obs_data <- observation[rep(1, nrow(best_grid)), ]
  
  # Assign data to the exploded data.table
  for (i in 1:length(column)) {
    
    # Data assignment
    obs_data[[column[i]]] <- copy(best_grid[[column[i]]])
    
  }
  
  # Get predicted values for initial observation
  initial_observation <- predictor(model = model, data = observation)
  
  # Get predicted values for partial dependence
  best_grid[[label_name]] <- predictor(model = model, data = obs_data)
  
  # Check for variability
  is_unchanged <- as.character(best_grid[[label_name]] == initial_observation)
  is_unchanged[is_unchanged == TRUE] <- "Fixed"
  is_unchanged[best_grid[[label_name]] < initial_observation] <- "Decreasing"
  is_unchanged[best_grid[[label_name]] > initial_observation] <- "Increasing"
  
  best_grid[[comparator_name]] <- as.factor(is_unchanged)
  
  # Return list of predicted values: initial grid, exploded grid, prediction values
  return(list(grid_init = grid_search,
              grid_exp = best_grid,
              preds = best_grid[[label_name]],
              obs = initial_observation))
  
}
