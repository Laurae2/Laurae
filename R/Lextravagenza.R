#' Laurae's Extravagenza machine learning model
#'
#' This function is a machine learning model using dynamic depth with xgboost but ignores the gradient boosting enhancements of xgboost. It outperforms xgboost in nearly every scenario where the number of boosting iterations is small. When the number of boosting iterations is large (like: 100), this model has worse performance than typical gradient boosted tree implementations. This does not work on multiclass problems.
#' 
#' Dynamic depth allows to train dynamic boosted trees that fits better the data early, thus overfitting quickly the data. As it uses a validation set as feedback during training, it is necessary to have a second validation set (test set), an uncommon scenario in machine learning.
#' 
#' The Extravagenza model does not leverage the properties of gradient and hessian to optimize the learning appropriately, hence overfitting faster without using any knowledge of previous trainings (but the last tree only).
#' 
#' Do not use this method when you attempt to predict large trees, as not being able to use the previous gradients/hessians leads to a poor generalization (but still better than most non-ensemble models). Usually, a xgboost model needing only 75 iterations will require 200 iterations for the Extravagenza machine learning model to (potentially) outperform the initial xgboost model.
#' 
#' For example, on House Prices data set using RMSE, you can try to beat xgboost:
#' 
# * To beat on House Prices (train: 1000, valid = 459, test = 1460): 31072.654297, 24377.464844, eta = 0.20, depth = 5
# * To beat on House Prices (train: 1000, valid = 459, test = 1460): 29599.361328, 22291.093750, eta = 0.05, depth = 5
#' 
#' In addition, you will need to use the latest xgboost repo (from pull request 1964 at least) if you want to train without spamming the console (\code{verbose = 0} used to not record metric!).
#' 
#' @param train Type: xgb.DMatrix. The training data. It will be used for training the models.
#' @param valid Type: xgb.DMatrix. The validation data. It will be used for selecting the model depth per iteration to assess generalization.
#' @param test Type: xgb.DMatrix. The testing data. It will be used for early stopping.
#' @param maximize Type: boolean. Whether to maximize or minimize the loss function. Defaults to \code{FALSE}.
#' @param personal_rounds Type: integer. The number of separate boosting iterations. Defaults to \code{100}.
#' @param personal_depth Type: vector of integers. The possible depth values during boosting of trees. Defaults to \code{1:10}, which means a depth between \code{1} and \code{10}, therefore \code{c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)}.
#' @param personal_eta Type: numeric. The shrinkage (learning rate). Lower values mean lower overfitting. Defaults to \code{0.20}.
#' @param auto_stop Type: integer. The early stopping value. When the metric does not improve for \code{auto_stop} iterations, the training is interrupted and the model returned to the user. Defaults to \code{10}.
#' @param base_margin Type: numeric. The base prediction value. For binary classification, it is recommended to be the number of label \code{1} divided by the number of observations, although it is not mandatory. Defaults to \code{0.5}.
#' @param seed Type: integer. Random seed used for training. Defaults to \code{0}.
#' @param ... Other arguments to pass to \code{xgb.train}. Examples: \code{nthread = 1}, \code{eta = 0.4}...
#' 
#' @return A list with the model (\code{model}), the parameters (\code{eta}, \code{base_margin}), the best training iteration for generalization (\code{best_iter}), the depth evolution over the number of iterations (\code{depth_tree}), the validation score (\code{valid_loss}), and the test score (\code{test_loss}).
#' 
#' @examples
#' \dontrun{
#' library(Laurae)
#' library(xgboost)
#' data(agaricus.train, package='xgboost')
#' data(agaricus.test, package='xgboost')
#' dtrain <- xgb.DMatrix(agaricus.train$data[1:5000, ], label = agaricus.train$label[1:5000])
#' dval <- xgb.DMatrix(agaricus.train$data[5001:6513, ], label = agaricus.train$label[5001:6513])
#' dtest <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)
#' Lex_model <- Lextravagenza(train = dtrain, # Train data
#'                            valid = dval, # Validation data = depth tuner
#'                            test = dtest, # Test data = early stopper
#'                            maximize = FALSE, # Not maximizing RMSE
#'                            personal_rounds = 50, # Boosting for 50 iterations
#'                            personal_depth = 1:8, # Dynamic depth between 1 and 8
#'                            personal_eta = 0.40, # Shrinkage of boosting to 0.40
#'                            auto_stop = 5, # Early stopping of 5 iterations
#'                            base_margin = 0.5, # Start with 0.5 probabilities
#'                            seed = 0, # Random seed
#'                            nthread = 1, # 1 thread for training
#'                            eta = 0.40, # xgboost shrinkage of 0.40 (avoid fast overfit)
#'                            booster = "gbtree", # train trees, can't work with GLM
#'                            objective = "binary:logistic", # classification, binary
#'                            eval_metric = "rmse" # RMSE metric to optimize
#' )
#' 
#' str(Lex_model, max.level = 1) # Get list of the model structure
#' 
#' predictedValues <- pred.Lextravagenza(Lex_model, dtest, nrounds = Lex_model$best_iter)
#' all.equal(sqrt(mean((predictedValues - agaricus.test$label)^2)),
#'           Lex_model$test[Lex_model$best_iter])
#' 
#' # Get depth evolution vs number of boosting iterations
#' plot(x = 1:length(Lex_model$depth),
#'      y = Lex_model$depth,
#'      main = "Depth vs iterations",
#'      xlab = "Iterations",
#'      ylab = "Depth")
#' 
#' # Get validation evolution vs number of boosting iterations
#' plot(x = 1:length(Lex_model$valid),
#'      y = Lex_model$valid,
#'      main = "Validation loss vs iterations",
#'      xlab = "Iterations",
#'      ylab = "Validation loss")
#' 
#' # Get testing evolution vs number of boosting iterations
#' plot(x = 1:length(Lex_model$test),
#'      y = Lex_model$test,
#'      main = "Testing loss vs iterations",
#'      xlab = "Iterations",
#'      ylab = "Testing loss")
#' }
#' 
#' @export

Lextravagenza <- function(train,
                          valid,
                          test,
                          maximize = FALSE,
                          personal_rounds = 100,
                          personal_depth = 1:10,
                          personal_eta = 0.20,
                          auto_stop = 10,
                          base_margin = 0.5,
                          seed = 0,
                          ...) {
  
  # Avoid change in-place by copying
  internal_train <- train
  internal_valid <- valid
  internal_test <- test
  
  # Store margin information
  initial_train <- getinfo(train, "base_margin")
  initial_valid <- getinfo(valid, "base_margin")
  initial_test <- getinfo(test, "base_margin")
  setinfo(internal_train, "base_margin", rep(base_margin, nrow(internal_train)))
  setinfo(internal_valid, "base_margin", rep(base_margin, nrow(internal_valid)))
  setinfo(internal_test, "base_margin", rep(base_margin, nrow(internal_test)))
  
  # Prepare dummy variables
  model_output <- list()
  depth_tree <- numeric(personal_rounds)
  valid_loss <- numeric(personal_rounds)
  test_loss <- numeric(personal_rounds)
  patience <- 0
  best_loss <- 999999999 * (1 - 2*maximize)
  
  # Do super boosting
  for (i in 1:personal_rounds) {
    
    model_list <- list()
    model_score <- numeric(length(personal_depth))
    loss_score <- numeric(length(personal_depth))
    
    for (j in personal_depth) {
      
      set.seed(seed)
      model_list[[j]] <- xgb.train(data = internal_train,
                                   watchlist = list(valid = internal_valid,
                                                    test = internal_test),
                                   max_depth = j,
                                   nrounds = 1,
                                   ...
                                   )
      
      # Store temporary metrics
      model_score[j] <- model_list[[j]]$evaluation_log[[2]][1]
      loss_score[j] <- model_list[[j]]$evaluation_log[[3]][1]
      #cat("[Tempo ", i, "] Current score: ", model_score[best_model], sep = "")
      
    }
    
    # Store best model outputs
    best_model <- (which.min(model_score)[1] * !maximize) + (which.max(model_score)[1] * maximize)
    depth_tree[i] <- best_model
    valid_loss[i] <- model_score[best_model]
    test_loss[i] <- loss_score[best_model]
    model_output[[i]] <- model_list[[best_model]]
    
    # Predict data
    predict1s <- predict(model_list[[best_model]], internal_train)
    predict2s <- predict(model_list[[best_model]], internal_valid)
    predict3s <- predict(model_list[[best_model]], internal_test)
    
    # Reset difference
    setinfo(internal_train, "base_margin", getinfo(internal_train, "base_margin") - (getinfo(internal_train, "base_margin") - predict1s) * personal_eta)
    setinfo(internal_valid, "base_margin", getinfo(internal_valid, "base_margin") - (getinfo(internal_valid, "base_margin") - predict2s) * personal_eta)
    setinfo(internal_test, "base_margin", getinfo(internal_test, "base_margin") - (getinfo(internal_test, "base_margin") - predict3s) * personal_eta)
    
    # Compare to best
    if (((loss_score[best_model] < best_loss) * !maximize) | ((loss_score[best_model] > best_loss) * maximize)) {
      best_loss <- loss_score[best_model]
      patience <- 0
      cat("[Final ", i, "] train: ", model_score[best_model], ", test: ", loss_score[best_model], ", with best loss being ", best_loss, "  \n", sep = "")
    } else {
      patience <- patience + 1
      if (patience > auto_stop) {
        cat("[Final ", i, "] train: ", model_score[best_model], ", test: ", loss_score[best_model], ", with best loss being ", best_loss, "(", patience, ")  \nEarly stopping triggered!", sep = "")
        depth_tree <- depth_tree[1:i]
        valid_loss <- valid_loss[1:i]
        test_loss <- test_loss[1:i]
        break
      }
    }
    
  }
  
  # Reset to initial values
  setinfo(internal_train, "base_margin", initial_train)
  setinfo(internal_valid, "base_margin", initial_valid)
  setinfo(internal_test, "base_margin", initial_test)
  
  return(list(model = model_output,
              eta = personal_eta,
              base_margin = base_margin,
              best_iter = length(depth_tree) - patience,
              depth = depth_tree,
              valid = valid_loss,
              test = test_loss))
  
}