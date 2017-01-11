#' Laurae's Machine Learning (xgboost gblinear helper function)
#'
#' This function is a demonstration function for using xgboost gblinear in \code{LauraeML} without premade folds. It has \code{alpha}, \code{lambda}, and \code{lambda_bias} as tunable hyperparameters. It also accepts feature selection, and performs full logging (every part is commented in the source) with writing to an external file in order to follow the hyperparameters and feature count.
#' 
#' @param x Type: vector (numeric). The hyperparameters to use.
#' @param y Type: vector (numeric). The features to use, as binary format (0 for not using, 1 for using).
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

LauraeML_gblinear <- function(x, y, data, label, folds) {
  
  # The earliest thing to do is to increment the iteration count (we commented that line, just for educational purposes)
  # iters <<- iters + 1 # For logging
  
  # What if we have no feature selected?
  if (sum(y) == 0) {
    
    # First, we do some logging in the global environment (we commented that line, just for educational purposes)
    # temp_params[iters, ] <<- c(iters, 9999999, x, y) # SIMPLE AS THAT!
    
    # Second, lets print some logging (we commented that line, just for educational purposes)
    # We print the time, the number of iterations with 5 digits, and that we had an error.
    # cat("(   ) [", format(Sys.time(), "%X"), "] Pass ", sprintf("%04d", iters), ": error\n", sep = "", file = "log.txt", append = TRUE)
    
    # Last, we return an absurd score which is so high you would rather have a random model than this 0-feature model
    return(9999999) # This iteration will be ignored by the optimizer if it does not belong to the elite proportion of the optimization iteration
    
  } else {
    
    # Column sampling of the data depending on the features
    if (sum(y) < length(y)) {
      
      mini_data <- Laurae::DTcolsample(data,
                                       kept = which(as.logical(y)),
                                       remove = FALSE,
                                       low_mem = FALSE,
                                       collect = 0,
                                       silent = TRUE)
      
    } else {
      
      # No column sampling because we select all columns
      mini_data <- data
      
    }
    
    # Prepare the placeholder for scores
    score <- numeric(length(folds))
    
    # Loop through each fold
    for (i in 1:length(folds)) {
      
      # First we subsample the training data
      temp_train <- Laurae::DTsubsample(mini_data,
                                        kept = folds[[i]],
                                        remove = TRUE, # TRUE because we don't want the fold in the training data
                                        low_mem = FALSE,
                                        collect = 0,
                                        silent = TRUE)
      
      # Then we create the xgb.DMatrix training data
      temp_train <- xgb.DMatrix(Laurae::DT2mat(temp_train,
                                               low_mem = FALSE,
                                               collect = 0,
                                               silent = TRUE),
                                label = label[-folds[[i]]])
      
      # Second we subsample the testing data
      temp_test <- Laurae::DTsubsample(mini_data,
                                       kept = folds[[i]],
                                       remove = FALSE, # FALSE because we want the fold in the testing data
                                       low_mem = FALSE,
                                       collect = 0,
                                       silent = TRUE)
      
      # Then we create the xgb.DMatrix testing data
      temp_test <- xgb.DMatrix(Laurae::DT2mat(temp_test,
                                              low_mem = FALSE,
                                              collect = 0,
                                              silent = TRUE),
                               label = label[folds[[i]]])
      
      # Third we train the model, here we use root mean squared error (RMSE) as demonstration
      set.seed(0) # Essential!
      my_model <- xgb.train(params = list(booster = "gblinear",
                                          nthread = 1, # gblinear with nthread > 1 is NOT reproducible
                                          eta = 0.10,
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
    
    # Sixth, we do some logging in the global environment (we commented that line, just for educational purposes)
    # temp_params[iters, ] <<- c(iters, score, x, y) # SIMPLE AS THAT!
    
    # Seventh, did we get the highest score? (we commented those lines because they are only for educational purposes)
    # if (score > hi_score) { # Is our score better (lower) than the highest score in the global environment?
    #   hi_score <<- score # Assign to the global environment our new best score
    #   star <- c("(***) ") # Brag about it in logs
    # } else {
    #   star <- "(   ) " # Can't brag higher scores!
    # }
    
    # Eighth, we print some logging! (we commented those lines because they are only for educational purposes)
    # We print the time, the iteration with 5 digits, the RMSE with 9 spaces (8 numbers when excluding the dot) including 5 digits, the number of features with 3 digits, each parameter with 8 spaces (7 numbers when excluding the dot) including 6 digits, and a line break.
    # cat(star, "[", format(Sys.time(), "%X"), "] Pass ", sprintf("%05d", iters), ": RMSE=", sprintf("%09.05f", error), " - feats=", sprintf("%03d", sum(y)), " - alpha=", sprintf("%08.06f", x[1]), ", lambda=", sprintf("%08.06f", x[2]), ", lambda_bias=", sprintf("%08.06f", x[3]), "\n", sep = "", file = "log.txt", append = TRUE)
    
    # Ninth, we can return the score!
    return(score)
    
  }
  
}