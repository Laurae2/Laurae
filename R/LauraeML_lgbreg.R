#' Laurae's Machine Learning (LightGBM regression helper function)
#'
#' This function is a demonstration function for using LightGBM regression in \code{LauraeML} without premade folds. It has \code{alpha}, \code{lambda}, and \code{lambda_bias} as tunable hyperparameters. It also accepts feature selection, and performs full logging (every part is commented in the source) with writing to an external file in order to follow the hyperparameters and feature count.
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

LauraeML_lgbreg <- function(x, y, data, label, folds) {
  
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
      temp_train <- lgb.Dataset(Laurae::DT2mat(temp_train,
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
      temp_test <- lgb.Dataset.create.valid(temp_train,
                                            Laurae::DT2mat(temp_test,
                                                           low_mem = FALSE,
                                                           collect = 0,
                                                           silent = TRUE),
                                            label = label[folds[[i]]])
      
      # Third we train the model, here we use root mean squared error (RMSE) as demonstration
      set.seed(0) # Essential!
      my_model <- lgb.train(list(objective = "regression"),
                            data = temp_train,
                            valids = list(test = temp_test),
                            max_depth = floor(x[1]),
                            min_data_in_leaf = floor(x[2]),
                            min_sum_hessian_in_leaf = x[3],
                            feature_fraction = x[4],
                            bagging_fraction = x[5],
                            lambda_l1 = x[6],
                            lambda_l2 = x[7],
                            feature_fraction_seed = 0,
                            bagging_freq = 1,
                            bagging_seed = 0,
                            max_bin = 255,
                            bin_construct_sample_cnt = 50000,
                            nrounds = 100000,
                            num_threads = 1,
                            boosting = "gbdt",
                            learning_rate = 1,
                            early_stopping_rounds = 10,
                            metric = "l2",
                            verbosity = -1,
                            verbose = -1,
                            record = TRUE)
      
      # Fourth, we must harvest the best performance of the model
      score[i] <- my_model$record_evals[[2]][[1]][[1]][[my_model$best_iter]] # This is where is stored the metric value
      
      
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
    # We print the time, the iteration with 5 digits, the RMSE with 9 spaces (8 numbers when excluding the dot) including 5 digits, the number of features with 3 digits, each parameter, and a line break.
    # cat(star, "[", format(Sys.time(), "%X"), "] Pass ", sprintf("%05d", iters), ": RMSE=", sprintf("%09.05f", error), " - feats=", sprintf("%03d", sum(y)), " - max_depth=", sprintf("%02d", floor(x[1])), ", min_data=", sprintf("%08.06f", floor(x[2])), ", min_hessian=", sprintf("%08.06f", x[3]), feature_fraction=", sprintf("%08.06f", x[4]), bagging_fraction=", sprintf("%08.06f", x[5]), lambda_l1=", sprintf("%08.06f", x[6]), lambda_l2=", sprintf("%08.06f", x[7]), "\n", sep = "", file = "log.txt", append = TRUE)
    
    # Ninth, we can return the score!
    return(score)
    
  }
  
}