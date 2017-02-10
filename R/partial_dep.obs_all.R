#' Partial Dependency Observation, Contour (multiple observations)
#'
#' This function computes partial dependency of a supervised machine learning model over a range of values for multiple observation. Does not work for multiclass problems! Beware as the number of data might explode, especially if you have many different data. The amount of observations you will end up explodes quickly and is linear to the \code{sum of each (max(unique value of feature, accuracy))}. For instance, 100 features with 50 unique values and 1000 observations ends up as 5,000,000 observations!!!!! (5000 times the initial dataset)
#' 
#' @param model Type: unknown. The model to pass to \code{predictor}.
#' @param predictor Type: function(model, data). The predictor function which takes a model and data as inputs, and return predictions. \code{data} is provided as data.table for maximum performance.
#' @param data Type: data.table (mandatory). The data we need to use to sample from for the partial dependency.
#' @param column Type: character. The column we want partial dependence from. You can specify two or more \code{column} as a vector, but it is highly not recommended to go for a lot of columns because the complexity is linearly multiplicative, think as \code{O*length(column)*accuracy}. For instance, \code{accuracy = 100}, \code{length(column) = 100}, and \code{nrow(data) = 1000000} leads to \code{1e+10} theoretical observations, which could explode the memory of any computer.
#' @param accuracy Type: integer. The accuracy of the partial dependence from, exprimed as number of sampled points by percentile of the \code{column} from the the \code{data}. Defaults to \code{min(length(data), 10)}, which means either 10 samples or all samples of \code{data} if the latter has less than 10 observations.
#' @param exact_only Type: logical. Whether to select only exact values for data sampling. Defaults to \code{TRUE}.
#' @param label_name Type: character. The column name given to the predicted values in the output table. Defaults to \code{"Target"}, this assumes you do not have a column called \code{"Target"} in your \code{column} vector.
#' @param comparator_name Type: character. The column name given to the evolution value (\code{"Increase"}, \code{"Fixed"}, \code{"Decrease"}) in the output table. Defaults to \code{"Evolution"}, this assumes you do not have a column called \code{"Evolution"} in your \code{column} vector.
#' 
#' @return A list with different elements: \code{grid_init} for the grid before expansion, \code{grid_exp} for the expanded grid with predictions, \code{preds} for the predictions, and \code{obs} for the original predictions on data
#' 
#' @examples
#' \dontrun{
#' # Let's load a dummy dataset
#' data(mtcars)
#' setDT(mtcars) # Transform to data.table for easier manipulation
#' 
#' # We train a xgboost model on 31 observations, keep last to analyze later
#' set.seed(0)
#' xgboost_model <- xgboost(data = data.matrix(mtcars[, -1]),
#'                          label = mtcars$mpg,
#'                          nrounds = 20)
#' 
#' # Perform partial dependence grid prediction to analyze the behavior of the 32th observation
#' # We want to check how it behaves with:
#' # => horsepower (hp)
#' # => number of cylinders (cyl)
#' # => transmission (am)
#' # => number of carburetors (carb)
#' preds_partial <- partial_dep.obs_all(model = xgboost_model,
#'                                      predictor = predictor_xgb, # Default for xgboost
#'                                      data = mtcars[, -1], # train data
#'                                      # when column is not specified => all columns
#'                                      accuracy = 20, # Up to 20 unique values per column
#'                                      exact_only = TRUE, # Not allowing approximations,
#'                                      label_name = "mpg", # Label is supposed "mpg"
#'                                      comparator_name = "evo") # Comparator +/-/eq for analysis
#' 
#' # How many observations? 3360, that's a lot coming from original 32 observations.
#' nrow(preds_partial$grid_exp)
#' 
#' # How many observations analyzed per column?
#' summary(preds_partial$grid_init)
#' #      Length Class  Mode   
#' # cyl   3     -none- numeric
#' # disp 19     -none- numeric
#' # hp   16     -none- numeric
#' # drat 16     -none- numeric
#' # wt   19     -none- numeric
#' # qsec 19     -none- numeric
#' # vs    2     -none- numeric
#' # am    2     -none- numeric
#' # gear  3     -none- numeric
#' # carb  6     -none- numeric
#' 
#' # Great plotting skills!
#' partial_dep.plot(preds_partial$grid_exp,
#'                  backend = c("plotly", "line"),
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



partial_dep.obs_all <- function(model, predictor, data, column = colnames(data), accuracy = min(length(data), 10), exact_only = TRUE, label_name = "Target", comparator_name = "Evolution") {
  
  # Get copy of data to process
  temp_data <- data
  
  # Prepare uniform length vector of percentile
  temp_percentile <- (1/accuracy) * (1:accuracy)
  
  # Rescale percentile to [0, 1]
  temp_percentile <- (temp_percentile - 1/accuracy) / (1 - 1/accuracy)
  
  # Get predicted values for initial observation
  initial_observation <- predictor(model = model, data = data)
  
  # Initalize dummy lists
  grid_search <- list()
  best_grid <- list()
  
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
      
      # Give names to grid search list
      names(grid_search)[i] <- column[i]
      
      # Store current values
      temp_values <- data[[column[i]]]
      
      best_grid[[i]] <- rbindlist(sapply(grid_search[[i]], function(x, temp_data, predictor, label_name, initial_observation) {
        
        # Replace current column
        temp_data[[column[i]]] <- rep(x, nrow(temp_data))
        
        # Setup data storage grid
        best_grid <- data.table(Feature = rep(column[i], nrow(temp_data)),
                                Value = rep(x, nrow(temp_data)))
        
        # Get predicted values for partial dependence
        best_grid[[label_name]] <- predictor(model = model, data = temp_data)
        
        # Check for variability
        is_unchanged <- as.character(best_grid[[label_name]] == initial_observation)
        is_unchanged[is_unchanged == TRUE] <- "Fixed"
        is_unchanged[best_grid[[label_name]] < initial_observation] <- "Decreasing"
        is_unchanged[best_grid[[label_name]] > initial_observation] <- "Increasing"
        
        best_grid[[comparator_name]] <- is_unchanged
        
        return(best_grid)
        
      }, temp_data = temp_data, predictor = predictor, label_name = label_name, initial_observation = initial_observation, simplify = FALSE, USE.NAMES = FALSE))
      
      # Restore values
      temp_data[[column[i]]] <- temp_values
      
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
      
      # Give names to grid search list
      names(grid_search)[i] <- column[i]
      
      # Store current values
      temp_values <- data[[column[i]]]
      
      best_grid[[i]] <- rbindlist(sapply(grid_search[[i]], function(x, temp_data, predictor, label_name, initial_observation) {
        
        # Replace current column
        temp_data[[column[i]]] <- rep(x, nrow(temp_data))
        
        # Setup data storage grid
        best_grid <- data.table(Feature = rep(column[i], nrow(temp_data)),
                                Value = rep(x, nrow(temp_data)))
        
        # Get predicted values for partial dependence
        best_grid[[label_name]] <- predictor(model = model, data = temp_data)
        
        # Check for variability
        is_unchanged <- as.character(best_grid[[label_name]] == initial_observation)
        is_unchanged[is_unchanged == TRUE] <- "Fixed"
        is_unchanged[best_grid[[label_name]] < initial_observation] <- "Decreasing"
        is_unchanged[best_grid[[label_name]] > initial_observation] <- "Increasing"
        
        best_grid[[comparator_name]] <- is_unchanged
        
        return(best_grid)
        
      }, temp_data = temp_data, predictor = predictor, label_name = label_name, initial_observation = initial_observation, simplify = FALSE, USE.NAMES = FALSE))
      
      # Restore values
      temp_data[[column[i]]] <- temp_values
      
    }
    
  }
  
  # Regroup lists to single data.table
  best_grid <- rbindlist(best_grid)
  
  # Convert character column to feature
  best_grid$Feature <- factor(best_grid$Feature, levels = column)
  best_grid[[comparator_name]] <- factor(best_grid[[comparator_name]], levels = if ("Fixed" %in% best_grid[[comparator_name]]) {c("Decreasing", "Fixed", "Increasing")} else {c("Decreasing", "Increasing")})
  
  # Return list of predicted values: initial grid, exploded grid, prediction values
  return(list(grid_init = grid_search,
              grid_exp = best_grid,
              preds = best_grid[[label_name]],
              obs = initial_observation))
  
}
