#' Print appropriately formatted integer
#'
#' This function is a helper function to print integers for your own convenience. If the number doesn't fit, it will be stretched which might cause an issue (for aligning your prints). Remember that negative value signs take 1 one space.
#' 
#' @param number Type: numeric. The integer you want formatted.
#' @param digits Type: integer. The number of digits you want to print. Unlike \code{print_fp}, it does not need to handle negative values in a special case.
#' 
#' @return The formatted integer.
#' 
#' @examples
#' print_int(12, 7) # "0000012"
#' print_int(12, 6) # "000012"
#' print_int(12, 5) # "00012"
#' print_int(12, 4) # "0012"
#' print_int(12, 3) # "012"
#' print_int(12, 1) # "12"
#' print_int(12, 2) # "12"
#' print_int(123456, 7) # "0123456"
#' print_int(123456789, 7) # "123456789"
#' print_int(-12, 4) # "-012"
#' print_int(-12, 3) # "-12"
#' print_int(-12, 2) # "-12"
#' print_int(-12, 1) # "-12"
#' print_int(-1234, 6) # "-01234"
#' print_int(-1234, 5) # "-1234"
#' print_int(-1234, 4) # "-1234"
#' 
#' @export

print_int <- function(number, digits = 8) {
  return(sprintf(paste0("%0", digits, "d"), number))
}


#' Print appropriately formatted fixed point
#'
#' This function is a helper function to print double precision values for your own convenience. If the number doesn't fit, it will be stretched which might cause an issue (for aligning your prints). Remember that negative value signs take 1 one space in digits.
#' 
#' @param number Type: numeric. The double precision value you want formatted.
#' @param digits Type: integer. The number of digits you want to print (pre-dot).
#' @param decimals Type: integer. The number of decimals you want to print (post-dot).
#' 
#' @return The formatted double precision value.
#' 
#' @examples
#' print_fp(123.456, 5, 5) # "00123.45600"
#' print_fp(123.456, 5, 4) # "00123.4560"
#' print_fp(123.456, 5, 3) # "00123.456"
#' print_fp(123.456, 5, 2) # "00123.46"
#' print_fp(123.456, 5, 1) # "00123.5"
#' print_fp(123.456, 5, 0) # "00123"
#' print_fp(123.456, 4, 0) # "0123"
#' print_fp(123.456, 3, 0) # "123"
#' print_fp(123.456, 2, 0) # "123"
#' print_fp(123.456, 1, 0) # "123"
#' print_fp(-123.456, 5, 5) # "-0123.45600"
#' print_fp(-123.456, 5, 4) # "-0123.4560"
#' print_fp(-123.456, 5, 3) # "-0123.456"
#' print_fp(-123.456, 5, 2) # "-0123.46"
#' print_fp(-123.456, 5, 1) # "-0123.5"
#' print_fp(-123.456, 5, 0) # "-0123"
#' print_fp(-123.456, 4, 0) # "-123"
#' print_fp(-123.456, 3, 0) # "-123"
#' print_fp(-123.456, 2, 0) # "-123"
#' print_fp(-123.456, 1, 0) # "-123"
#' 
#' @export

print_fp <- function(number, digits = 4, decimals = 4) {
  return(sprintf(paste0("%0", digits + decimals + (!(floor(number) == number) * (decimals > 0)), ".0", decimals, "f"), number))
}


#' Print appropriately formatted integer or fixed point (hybrid)
#'
#' This function is a helper function to print integers or double precision values for your own convenience. If the number doesn't fit, it will be stretched which might cause an issue (for aligning your prints). Remember that negative value signs take 1 one space in digits.
#' 
#' @param number Type: numeric. The integer / double precision value you want formatted.
#' @param digits Type: integer. The number of digits you want to print (pre-dot).
#' @param decimals Type: integer. The number of decimals you want to print (post-dot).
#' 
#' @return The formatted double precision value.
#' 
#' @examples
#' print_hyb(12, 7) # "0000012"
#' print_hyb(12, 6) # "000012"
#' print_hyb(12, 5) # "00012"
#' print_hyb(12, 4) # "0012"
#' print_hyb(12, 3) # "012"
#' print_hyb(12, 1) # "12"
#' print_hyb(12, 2) # "12"
#' print_hyb(123456, 7) # "0123456"
#' print_hyb(123456789, 7) # "123456789"
#' print_hyb(-12, 4) # "-012"
#' print_hyb(-12, 3) # "-12"
#' print_hyb(-12, 2) # "-12"
#' print_hyb(-12, 1) # "-12"
#' print_hyb(-1234, 6) # "-01234"
#' print_hyb(-1234, 5) # "-1234"
#' print_hyb(-1234, 4) # "-1234"
#' print_hyb(123.456, 5, 5) # "00123.45600"
#' print_hyb(123.456, 5, 4) # "00123.4560"
#' print_hyb(123.456, 5, 3) # "00123.456"
#' print_hyb(123.456, 5, 2) # "00123.46"
#' print_hyb(123.456, 5, 1) # "00123.5"
#' print_hyb(123.456, 5, 0) # "00123"
#' print_hyb(123.456, 4, 0) # "0123"
#' print_hyb(123.456, 3, 0) # "123"
#' print_hyb(123.456, 2, 0) # "123"
#' print_hyb(123.456, 1, 0) # "123"
#' print_hyb(-123.456, 5, 5) # "-0123.45600"
#' print_hyb(-123.456, 5, 4) # "-0123.4560"
#' print_hyb(-123.456, 5, 3) # "-0123.456"
#' print_hyb(-123.456, 5, 2) # "-0123.46"
#' print_hyb(-123.456, 5, 1) # "-0123.5"
#' print_hyb(-123.456, 5, 0) # "-0123"
#' print_hyb(-123.456, 4, 0) # "-123"
#' print_hyb(-123.456, 3, 0) # "-123"
#' print_hyb(-123.456, 2, 0) # "-123"
#' print_hyb(-123.456, 1, 0) # "-123"
#' 
#' @export

print_hyb <- function(number, digits = 4, decimals = -1) {
  
  number <- as.numeric(number)
  digits <- as.integer(digits)
  decimals <- as.integer(decimals)
  return(sprintf(paste0("%0", digits + (decimals + (!(floor(number) == number) * (decimals > 0))), rep(paste0(".0", decimals), 1 * (decimals > -1)), paste0(rep("f", 1 * (decimals > -1)), rep("d", 1 * (decimals == -1)))), number))
  
}


#' Print appropriately formatted hyperparameters and error
#'
#' This function is a helper function to print the hyperparameters and error appropriately in a simple call. The first element refers to the name, the second is for the value, the third for the number of digits, and the fourth for the number of decimals. Specify the last parameter as \code{-1} in order to pass the value as integer.
#' 
#' @param params Type: list of vectors. A list of vectors formatted like \code{list(c("alpha", 1.248, 2, 5), c("beta", 2.58, 2, 5), c("integer", 8, 2, -1))}, which is here (alpha = 1.248 to print with 2 decimals and 5 digits => "01.24800") (beta = 2.58 to print with 2 digits and 5 decimals => "02.58000") (integer = 8 to print with 2 decimals => "08").
#' 
#' @return The formatted double precision value.
#' 
#' @examples
#' best_params <- list(c("alpha", 1.248, 2, 5), c("beta", 2.58, 2, 5), c("integer", 8, 2, -1))
#' print_multi(best_params) # "alpha=01.24800, beta=02.58000, integer=08"
#' 
#' @export

print_multi <- function(params) {
  
  to_print <- paste0(paste(sapply(params, function(x) {paste0(x[1], "=", print_hyb(as.numeric(x[2]), as.numeric(x[3]), as.numeric(x[4])))}), collapse = ", "))
  return(to_print)
  
}


#' Laurae's Machine Learning Utility: new input logger
#'
#' This function is a helper function to do the logging when a new input is existing for \code{LauraeML} during a training iteration of the optimizer. For each elements, the first element refers to the name, the second is for the value, the third for the number of digits, and the fourth for the number of decimals. It is assumed integer value when the decimal number is not specified.
#' 
#' 
#' @param logging Type: character. The \code{logging} parameter passed from \code{LauraeML} (where to store log file).
#' @param x Type: vector (numeric). The hyperparameters to use passed from the trainer.
#' @param y Type: vector (numeric). The features to use, as binary format (0 for not using, 1 for using) passed from the trainer.
#' @param mobile Type: environment. The environment passed from \code{LauraeML}.
#' @param error Type: vector. A vector formatted like \code{c("RMSE", 23534.372, 6, 5)} which is here RMSE = 23534.372 to print with 6 digits and 5 decimals => "023534.37200."
#' @param params Type: list of vectors. A list of vectors formatted like \code{list(c("alpha", 1.248, 2, 5), c("beta", 2.58, 2, 5), c("integer", 8, 2, -1))}, which is here (alpha = 1.248 to print with 2 decimals and 5 digits => "01.24800") (beta = 2.58 to print with 2 digits and 5 decimals => "02.58000") (integer = 8 to print with 2 decimals => "08").
#' @param maximize Type: boolean. The \code{maximize} parameter passed from \code{LauraeML} (whether to maximize or not the metric).
#' @param score Type: numeric. The \code{score} parameter passed from \code{LauraeML} training functions.
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#' hi_score <- 1000
#' iters <- 10
#' y <- c(1, 0, 1, 0, 1)
#' best_params <- list(c("alpha", 1.248, 2, 5), c("beta", 2.58, 2, 5), c("integer", 8, 2))
#' best_error <- c("RMSE", 23534.372, 6, 5)
#' LauraeML_utils.newlog("whatever.txt", c(1,2,3,4), y, TRUE, 1000, best_error, best_params)
#' }
#' 
#' @export

LauraeML_utils.newlog <- function(mobile, logging, x, y, maximize, score, error, params) {
  
  if (!is.null(logging)) {
    
    # Sixth, we do some logging in the global environment
    mobile$iters <- with(mobile, iters) + 1
    mobile$temp_params[with(mobile, iters), ] <- c(with(mobile, iters), score, x, y) # SIMPLE AS THAT!
    
    # Seventh, did we get the highest score?
    if (((maximize) & (score > with(mobile, hi_score))) | ((!maximize) & (score < with(mobile, hi_score)))) { # Is our score better than the highest score in the global environment?
      mobile$hi_score <- score # Assign to the global environment our new best score
      star <- c("(***) ") # Brag about it in logs
    } else {
      star <- "(   ) " # Can't brag higher scores!
    }
    
    # Eighth, we print some logging!
    # We print the time, the iteration with 5 digits, the RMSE with 9 spaces (8 numbers when excluding the dot) including 5 digits, the number of features with 3 digits, each parameter with 8 spaces (7 numbers when excluding the dot) including 6 digits, and a line break.
    cat(star, "[", format(Sys.time(), "%X"), "] Pass ", print_int(with(mobile, iters), 5), ": ", error[1], "=", print_hyb(error[2], error[3], error[4]), " - feats=", print_int(sum(y), 3), " - ", print_multi(params), "\n", sep = "", file = logging, append = TRUE)
    
  }
  
}


#' Laurae's Machine Learning Utility: bad input score
#'
#' This function is a helper function to return a bad input score for \code{LauraeML} during a training iteration of the optimizer, specifically used when you have no features during training.
#' 
#' @param maximize Type: boolean. The \code{maximize} parameter passed from \code{LauraeML} (whether to maximize or not the metric).
#' @param score Type: numeric. The score to optimize.
#' 
#' @return A so bad input score you better avoid such thing.
#' 
#' @examples
#' \dontrun{
#' # What if we have no feature selected?
#' if (sum(y) == 0) {
#' 
#'   # Logging specific
#'   LauraeML_utils.badlog(logging, x, y, score = NA)
#'   
#'   # Last, we return an absurd score which is so high
#'   # you would rather have a random model than this 0-feature model
#'   # 
#'   # This iteration will be ignored by the optimizer if
#'   # it does not belong to the elite proportion of the optimization iteration
#'   # which should be obviously true
#'   return(LauraeML_utils.badinput(maximize,
#'                                  score = 9999999))
#' }
#' }
#' 
#' @export

LauraeML_utils.badscore <- function(maximize, score = 9999999) {
  return((-maximize * 2 + 1) * score)
}


#' Laurae's Machine Learning Utility: bad input logger
#'
#' This function is a helper function to do the logging when a bad input is existing for \code{LauraeML} during a training iteration of the optimizer, specifically used when you have no features during training.
#' 
#' @param mobile Type: environment. The environment passed from \code{LauraeML}.
#' @param logging Type: character. The \code{logging} parameter passed from \code{LauraeML} (where to store log file).
#' @param x Type: vector (numeric). The hyperparameters to use passed from the trainer.
#' @param y Type: vector (numeric). The features to use, as binary format (0 for not using, 1 for using) passed from the trainer.
#' @param score Type: numeric. The score to optimize.
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#' # What if we have no feature selected?
#' if (sum(y) == 0) {
#' 
#'   # Logging specific
#'   LauraeML_utils.badlog(logging, x, y, score = NA)
#'   
#'   # Last, we return an absurd score which is so high
#'   # you would rather have a random model than this 0-feature model
#'   # 
#'   # This iteration will be ignored by the optimizer if
#'   # it does not belong to the elite proportion of the optimization iteration
#'   # which should be obviously true
#'   return(LauraeML_utils.badinput(maximize,
#'                                  score = 9999999))
#' }
#' }
#' 
#' @export

LauraeML_utils.badlog <- function(mobile, logging, x, y, score = NA) {
  
  if (!is.null(logging)) {
    
    # We do some logging in the global environment
    mobile$iters <- with(mobile, iters) + 1
    mobile$temp_params[with(mobile, iters), ] <- c(with(mobile, iters), score, x, y) # SIMPLE AS THAT!
    
    # Lets print some logging
    # We print the time, the number of iterations with 5 digits, and that we had an error.
    cat("(   ) [", format(Sys.time(), "%X"), "] Pass ", print_int(with(mobile, iters), 5), ": error\n", sep = "", file = logging, append = TRUE)
  }
  
}


#' Laurae's Machine Learning Utility: subset features to select during training
#'
#' This function is a helper function to do the feature sampling for \code{LauraeML} during a training iteration of the optimizer.
#' 
#' @param data Type: data.table. The data features. Comes from \code{LauraeML}.
#' @param y Type: vector (numeric). The features to use, as binary format (0 for not using, 1 for using) passed from the trainer.
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#' mini_data <- LauraeML_utils.feat_sel(data, y)
#' }
#' 
#' @export

LauraeML_utils.feat_sel <- function(data, y) {
  
  # Column sampling of the data depending on the features
  if (sum(y) < length(y)) {
    
    return(Laurae::DTcolsample(data,
                               kept = which(as.logical(y)),
                               remove = FALSE,
                               low_mem = FALSE,
                               collect = 0,
                               silent = TRUE))
    
  } else {
    
    # No column sampling because we select all columns
    return(data)
    
  }
  
}


#' Laurae's Machine Learning Utility: create LightGBM dataset
#'
#' This function is a helper function to do create a LightGBM dataset for \code{LauraeML} during a training iteration of the optimizer.
#' 
#' @param data Type: data.table. The data features. Comes from \code{LauraeML}.
#' @param fold Type: vector (numeric). The observation rows to keep/remove. Comes from \code{LauraeML}.
#' @param label Type: vector (numeric). The labels. Comes from \code{LauraeML}.
#' @param is_train Type: logical. Whether the fold refers to testing data (\code{FALSE}) or training data (\code{TRUE}).
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#' for (i in 1:5) {
#'   # First we create the training data
#'   temp_train <- LauraeML_utils.lgb_data(data = mini_data,
#'                                         fold = folds[[i]],
#'                                         label = label,
#'                                         is_train = TRUE)
#' 
#'   # Second we create the testing data
#'   temp_test <- LauraeML_utils.lgb_data(data = mini_data,
#'                                        fold = folds[[i]],
#'                                        label = label,
#'                                        is_train = FALSE)
#' }
#' }
#' 
#' @export

LauraeML_utils.lgb_data <- function(data, fold, label, is_train) {
  
  # First we subsample the training data
  temp_data <- Laurae::DTsubsample(data,
                                   kept = fold,
                                   remove = is_train, # TRUE because we don't want the fold in the training data, FALSE keeps for test data
                                   low_mem = FALSE,
                                   collect = 0,
                                   silent = TRUE)
  
  # Then we create the lgb.Dataset training data
  temp_data <- lgb.Dataset(Laurae::DT2mat(temp_data,
                                          low_mem = FALSE,
                                          collect = 0,
                                          silent = TRUE),
                           label = label[fold * (((!is_train) * 2) - 1)])
  
  return(temp_data)
  
}


#' Laurae's Machine Learning Utility: create xgboost dataset
#'
#' This function is a helper function to do create a xgboost dataset for \code{LauraeML} during a training iteration of the optimizer.
#' 
#' @param data Type: data.table. The data features. Comes from \code{LauraeML}.
#' @param fold Type: vector (numeric). The observation rows to keep/remove. Comes from \code{LauraeML}.
#' @param label Type: vector (numeric). The labels. Comes from \code{LauraeML}.
#' @param is_train Type: logical. Whether the fold refers to testing data (\code{FALSE}) or training data (\code{TRUE}).
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#' for (i in 1:5) {
#'   # First we create the training data
#'   temp_train <- LauraeML_utils.xgb_data(data = mini_data,
#'                                         fold = folds[[i]],
#'                                         label = label,
#'                                         is_train = TRUE)
#' 
#'   # Second we create the testing data
#'   temp_test <- LauraeML_utils.xgb_data(data = mini_data,
#'                                        fold = folds[[i]],
#'                                        label = label,
#'                                        is_train = FALSE)
#' }
#' }
#' 
#' @export

LauraeML_utils.xgb_data <- function(data, fold, label, is_train) {
  
  # First we subsample the training data
  temp_data <- Laurae::DTsubsample(data,
                                   kept = fold,
                                   remove = is_train, # TRUE because we don't want the fold in the training data, FALSE keeps for test data
                                   low_mem = FALSE,
                                   collect = 0,
                                   silent = TRUE)
  
  # Then we create the lgb.Dataset training data
  temp_data <- xgb.DMatrix(Laurae::DT2mat(temp_data,
                                          low_mem = FALSE,
                                          collect = 0,
                                          silent = TRUE),
                           label = label[fold * (((!is_train) * 2) - 1)])
  
  return(temp_data)
  
}
