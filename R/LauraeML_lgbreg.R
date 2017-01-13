#' Laurae's Machine Learning (LightGBM regression helper function)
#'
#' This function is a demonstration function for using LightGBM regression in \code{LauraeML} without premade folds. It has \code{alpha}, \code{lambda}, and \code{lambda_bias} as tunable hyperparameters. It also accepts feature selection, and performs full logging (every part is commented in the source) with writing to an external file in order to follow the hyperparameters and feature count.
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

LauraeML_lgbreg <- function(x, y, mobile, parallelized, maximize, logging, data, label, folds) {
  
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
      temp_train <- LauraeML_utils.lgb_data(data = mini_data,
                                            fold = folds[[i]],
                                            label = label,
                                            is_train = TRUE)
      
      # Second we create the testing data
      temp_test <- LauraeML_utils.lgb_data(data = mini_data,
                                           fold = folds[[i]],
                                           label = label,
                                           is_train = FALSE)
      
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
    
    # Logging specific
    LauraeML_utils.newlog(mobile, logging, x, y, maximize, score,
                          c("RMSE", score, 9, 5),
                          list(c("max_depth", floor(x[1]), 2, -1),
                               c("min_data", floor(x[2]), 3, -1),
                               c("min_hessian", x[3], 2, 6),
                               c("feature_fraction", x[4], 1, 6),
                               c("bagging_fraction", x[5], 1, 6),
                               c("lambda_l1", x[6], 1, 6),
                               c("lambda_l2", x[7], 1, 6)))
    
    # Eventually, we can return the score!
    return(score)
    
  }
  
}