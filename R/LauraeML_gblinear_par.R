#' Laurae's Machine Learning (xgboost gblinear helper parallel function)
#'
#' This function is a demonstration function for using xgboost gblinear in \code{LauraeML} with premade folds (in addition to being parallelized over folds, assuming \code{mcl} in the global environment is the parallel cluster). It has \code{alpha}, \code{lambda}, and \code{lambda_bias} as tunable hyperparameters. It also accepts feature selection, and performs full logging (every part is commented in the source) with writing to an external file in order to follow the hyperparameters and feature count.
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
#' # To run before using LauraeML
#' library(doParallel)
#' library(foreach)
#' mcl <- makeCluster(4)
#' invisible(clusterEvalQ(mcl, library("xgboost")))
#' invisible(clusterEvalQ(mcl, library("data.table")))
#' invisible(clusterEvalQ(mcl, library("Laurae")))
#' 
#' # In case you are doing manual training, try this.
#' # We suppose our data is in the variable "data" and labels in "label".
#' 
#' folds <- Laurae::kfold(label, k = 5)
#' temp_data <- list()
#' temp_label <- list()
#' 
#' for (i in 1:length(folds)) {
#' 
#' temp_data[[i]] <- list()
#' temp_data[[i]][[1]] <- Laurae::DTsubsample(data,
#'                                            kept = folds[[i]],
#'                                            remove = TRUE,
#'                                            low_mem = FALSE,
#'                                            collect = 0,
#'                                            silent = TRUE)
#' temp_data[[i]][[2]] <- Laurae::DTsubsample(data,
#'                                            kept = folds[[i]],
#'                                            remove = FALSE,
#'                                            low_mem = FALSE,
#'                                            collect = 0,
#'                                            silent = TRUE)
#' temp_label[[i]] <- list()
#' temp_label[[i]][[1]] <- label[-folds[[i]]]
#' temp_label[[i]][[2]] <- label[folds[[i]]]
#'
#' }
#' 
#' clusterExport(mcl, c("temp_data", "temp_label"), envir = environment())
#' registerDoParallel(cl = mcl)
#' 
#' # This will not run correctly because it's not made to be used like that
#' LauraeML_gblinear_par(x = c(1, 1, 1),
#'                       y = rep(1, ncol(data)),
#'                       mobile = NA,
#'                       parallelized = mcl,
#'                       maximize = TRUE,
#'                       logging = NULL,
#'                       data = temp_data,
#'                       label = temp_label,
#'                       folds = folds)
#' 
#' # Stops the cluster
#' registerDoSEQ()
#' stopCluster(mcl)
#' #closeAllConnections() # In case of emergency if your cluster do not answer
#' }
#' 
#' @export

LauraeML_gblinear_par <- function(x, y, mobile, parallelized, maximize, logging, data, label, folds) {
  
  # The earliest thing to do is to increment the iteration count (we commented that line, just for educational purposes)
  # iters <<- iters + 1 # For logging
  
  # What if we have no feature selected?
  if (sum(y) == 0) {
    
    # Logging specific
    LauraeML_utils.badlog(mobile, logging, x, y, NA)
    
    # Last, we return an absurd score which is so high you would rather have a random model than this 0-feature model
    return(LauraeML_utils.badscore(maximize, 999999999)) # This iteration will be ignored by the optimizer if it does not belong to the elite proportion of the optimization iteration
    
  } else {
    
    # Export features to keep
    clusterExport(parallelized, c("x", "y"), envir = environment())
    
    # Loop through each fold
    score <- foreach(i = 1:length(folds), .combine = "c", .inorder = FALSE, .verbose = FALSE, .noexport = c("temp_train", "temp_test", "mini_data", "label", "folds", "my_model")) %dopar% {
      
      # We split the columns first
      if (sum(y) < length(y)) {
        
        temp_train <- LauraeML_utils.feat_sel(data[[i]][[1]], y)
        temp_test <- LauraeML_utils.feat_sel(data[[i]][[2]], y)
        
      } else {
        
        temp_train <- data[[i]][[1]]
        temp_test <- data[[i]][[2]]
        
      }
      
      # Then we create the xgb.DMatrix training data
      temp_train <- xgb.DMatrix(Laurae::DT2mat(temp_train,
                                               low_mem = FALSE,
                                               collect = 0,
                                               silent = TRUE),
                                label = label[[i]][[1]])
      
      # Then we create the xgb.DMatrix testing data
      temp_test <- xgb.DMatrix(Laurae::DT2mat(temp_test,
                                              low_mem = FALSE,
                                              collect = 0,
                                              silent = TRUE),
                               label = label[[i]][[2]])
      
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
      return(my_model$evaluation_log[[2]][my_model$best_iteration]) # This is where is stored the metric value
      
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