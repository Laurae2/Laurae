#' Laurae's Machine Learning (xgboost gblinear helper function)
#'
#' This function is a demonstration function for using xgboost gblinear in \code{LauraeML} without premade folds. It has \code{alpha}, \code{lambda}, and \code{lambda_bias} as tunable hyperparameters. It also accepts feature selection, and performs full logging (every part is commented in the source) with writing to an external file in order to follow the hyperparameters and feature count.
#' 
#' @param x Type: vector (numeric). The hyperparameters to use.
#' @param y Type: vector (numeric). The features to use, as binary format (0 for not using, 1 for using).
#' @param mobile Type: environment. The environment passed from \code{LauraeML}.
#' @param parallelized Type: parallel socket cluster (makeCluster or similar). The \code{parallelized} parameter passed from \code{LauraeML} (whether to parallelize training per folds or not).
#' @param maximize Type: boolean. The \code{maximize} parameter passed from \code{LauraeML} (whether to maximize or not the metric).
#' @param logging Type: character. The \code{logging} parameter passed from \code{LauraeML} (where to store log file).
#' @param data Type: data.table (mandatory). The data features. Comes from \code{LauraeML}.
#' @param label Type: vector (numeric). The labels. Comes from \code{LauraeML}.
#' @param folds Type: list of numerics. The folds as list. Comes from \code{LauraeML}.
#' 
#' @return The score of the cross-validated xgboost gblinear model, for the provided hyperparameters and features to use.
#' 
#' @examples
#' \dontrun{
#' # To add
#' }
#' 
#' @export

LauraeML_gblinear <- function(x, y, mobile, parallelized, maximize, logging, data, label, folds) {
  
  # The earliest thing to do is to increment the iteration count (we commented that line, just for educational purposes)
  # iters <<- iters + 1 # For logging
  
  # What if we have no feature selected?
  if (sum(y) == 0) {
    
    # Logging specific
    LauraeML_utils.badlog(mobile, logging, x, y, NA)
    
    # Last, we return an absurd score which is so high you would rather have a random model than this 0-feature model
    return(LauraeML_utils.badscore(maximize, 999999999)) # This iteration will be ignored by the optimizer if it does not belong to the elite proportion of the optimization iteration
    
  } else {
    
    # Column sampling of the data depending on the features
    mini_data <- LauraeML_utils.feat_sel(data, y)
    
    # Prepare the placeholder for scores
    score <- numeric(length(folds))
    
    # Loop through each fold
    for (i in 1:length(folds)) {
      
      # First we create the training data
      temp_train <- LauraeML_utils.xgb_data(data = mini_data,
                                            fold = folds[[i]],
                                            label = label,
                                            is_train = TRUE)
      
      # Second we create the testing data
      temp_test <- LauraeML_utils.xgb_data(data = mini_data,
                                           fold = folds[[i]],
                                           label = label,
                                           is_train = FALSE)
      
      # Third we train the model, here we use root mean squared error (RMSE) as demonstration
      set.seed(0) # Essential!
      my_model <- xgb.train(params = list(booster = "gblinear",
                                          nthread = 1, # gblinear with nthread > 1 is NOT reproducible
                                          eta = 0.1,
                                          alpha = x[1],
                                          lambda = x[2],
                                          lambda_bias = x[3],
                                          objective = "reg:linear",
                                          eval_metric = "rmse"),
                            data = temp_train,
                            nrounds = 1000000,
                            watchlist = list(val = temp_test),
                            verbose = FALSE, # we don't want to print
                            early_stopping_rounds = 10, # gblinear can be stopped very early without major convergence issues, unless your data is very noisy
                            maximize = FALSE,
                            callbacks = list(cb.evaluation.log()))
      
      # Fourth, we must harvest the best performance of the model
      score[i] <- my_model$evaluation_log[[2]][my_model$best_iteration] # This is where is stored the metric value
      
      
    }
    
    # Fifth, get the mean per score
    score <- mean(score)
    
    # Logging specific
    LauraeML_utils.newlog(mobile, logging, x, y, maximize, score,
                          c("RMSE", score, 9, 5),
                          list(c("alpha", x[1], 2, 6),
                               c("lambda", x[2], 2, 6),
                               c("lambda_bias", x[3], 2, 6)))
    
    # Eventually, we can return the score!
    return(score)
    
  }
  
}