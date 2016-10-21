#' Outlying univariate continuous association rule finder
#'
#' This function allows you to search for association rules on outlying univariate continuous features against a binary label. The predicted label is 0, and the overfitting severity is very high (see: Kaggle's Santander Customer Satisfaction competition).
#' It can be used to score outliers first, then make rules afterwards if needed.
#' Verbosity is automatic and cannot be removed. In case you need this function without verbosity, please compile the package after removing verbose messages.
#' 
#' @param data The data.frame containing the features to make association rules on, or the scoring matrix. Missing values are not allowed.
#' @param label The target label as an integer vector (each value must be either 0 or 1). 1 must be the miniority label.
#' @param train_rows The rows used for training the association rules. Must be your training set, whose length is equal to \code{length(labels)}. Defaults to \code{length(label)}.
#' @param iterations The amount of iterations allowed for limited-memory Gradient Descent
#' @param minimal_score The association rule finder will not accept any node under the allowed outlying score. Defaults to \code{25}.
#' @param minimal_node The association rule finder will not accept any node containing under that specific amount of samples. Defaults to \code{5}.
#' @param false_negatives The association rule will allow at most (\code{false_negatives - 1}) false negatives. A higher allows a more permissive algorithm, lower makes it very difficult to converge (or to find any rule at all). Defaults to \code{2}.
#' @param seed The random seed for reproducibility. Defaults to \code{11111}.
#' @param scoring Whether to score features before computing the association rules. Defaults to \code{TRUE}.
#' @param ruling Whether to rule features (useful when you only want the scores). Defaults to \code{TRUE}.
#' 
#' @return A list with one to three elements: \code{"scores"} the outlying scores for features, \code{"parsed_scores"} for the association rule result on specific features, and \code{"output"} for the association rule general result per observation.
#' 
#' @examples 
#' \dontrun{
#' scored_data <- rule_single(data = data, label = NA, scoring = TRUE, ruling = FALSE)
#' rules <- rule_single(data = scored_data, label = target,
#' iterations = 100, scoring = FALSE, ruling = TRUE)
#' preds <- preds[rules$output[(length(target)+1):(nrow(data))] == 0] <- 0
#' }
#' 
#' @export

rule_single <- function(data,
                        label,
                        train_rows = length(label),
                        iterations = 1000,
                        minimal_score = 25,
                        minimal_node = 5,
                        false_negatives = 2,
                        seed = 11111,
                        scoring = TRUE,
                        ruling = TRUE) {
  
  # Initialize optimizer helper function
  optimized_func <- function(target, scores, min_score, min_node, false_neg, cutoff) {
    
    # cutoff has two values: the minimum and maximum. Everything between is dished out.
    if (cutoff[2] < cutoff[1]) {
      
      known <- integer(0)
      
    } else {
      
      # takes values in exterior to [cutoff1, cutoff2], else dishes out empty numeric
      known <- target[which(((scores >= cutoff[2]) == TRUE) | ((scores <= cutoff[1]) == TRUE))]
      
    }
    
    # if cutoff not leading to empty variable
    if (length(known) >= min_node) {
      
      # return 0 if pure rule, else return the probability if ratio over 25 (better than random), else return 999 (worse than random)
      best <- ifelse(sum(known == 1) == 0, 0, ifelse(((sum(known == 0) / sum(known == 1)) >= min_score) & (sum(known == 1) <= false_neg), sum(known == 1) / sum(known == 0), 999))
      
    } else {
      
      # node empty
      best <- 999 
      
    }
    
    # objective: return the purest node
    return(best)
    
  }
  
  if (scoring == TRUE) {
    
    prog_bar <- txtProgressBar(style = 3)
    
    # Compute outlying scores
    for (i in colnames(data)) {
      
      data[[i]] <- scores(data[[i]])
      setTxtProgressBar(prog_bar, which(i == colnames(data))/length(colnames(data)))
      
    }
    
    close(prog_bar)
    
  }
  
  if (ruling == TRUE) {
    
    # Prepare dataframes
    data_scores_parsed <- data.frame(matrix(ncol = ncol(data)+1, nrow = nrow(data)))
    colnames(data_scores_parsed) <- c(colnames(data), "Final")
    data_scores_parsed$Final <- rep(1, nrow(data))
    
    # Loop for finding rules
    for (i in colnames(data)) {
      
      data_scores_parsed[[i]] <- rep(1, nrow(data))
      scoring_input <- data[[i]][1:train_rows] #get scores from train set
      min_allowance <- min(scoring_input) #get the maximum allowed score
      max_allowance <- max(scoring_input) #get the maximum allowed score
      
      set.seed(seed)
      optimized_output <- optim(par = c(min_allowance, max_allowance), optimized_func, method = "L-BFGS-B", target = label, scores = scoring_input, min_score = minimal_score, min_node = minimal_node, false_neg = false_negatives, lower = min_allowance, upper = max_allowance, control = list(maxit = iterations, trace = 0))
      cat("[", which(i == colnames(data)), ": ", i, "] ", ifelse(optimized_output$value >= 999, "Failed to optimize with gradient descent (you should loose conditions!).", paste("Best node: ", ifelse(optimized_output$value == 0, "Inf", 1/optimized_output$value), ":1 (", round(optimized_output$value*100, digits = 3) , "%) [ ", sum((scoring_input >= optimized_output$par[2]) | (scoring_input <= optimized_output$par[1])), "(train) | ", sum((data[[i]][(nrow(train)+1):NROW(data_scores_parsed[[i]])] >= optimized_output$par[2]) | (data[[i]][(nrow(train)+1):NROW(data_scores_parsed[[i]])] <= optimized_output$par[1])), "(test) ] for params (", optimized_output$par[1], ", ", optimized_output$par[2], ").", sep = "")), sep = "")
      
      if ((optimized_output$value >= 999) | (sum((data[[i]][(nrow(train)+1):NROW(data_scores_parsed[[i]])] >= optimized_output$par[2]) | (data[[i]][(nrow(train)+1):NROW(data_scores_parsed[[i]])] <= optimized_output$par[1]))) == 0) {
        
        #do nothing
        cat(" | was useless!\n", sep = "")
        
      } else {
        
        data_output <- ifelse(optimized_output$value == 0, 0, optimized_output$value)
        data_scores_parsed[[i]][(scoring_input >= optimized_output$par[2]) | (scoring_input <= optimized_output$par[1])] <- data_output
        data_scores_parsed$Final <- data_scores_parsed$Final * data_scores_parsed[[i]]
        cat(" | was stored!\n", sep = "")
        
      }
      
    }
    
    cat("\n-----\nSummary:\nTrain rows soft-ruled: ", nrow(train) - sum(data_scores_parsed$Final[1:nrow(train)] == 1), " (pure: ", sum(data_scores_parsed$Final[1:nrow(train)] == 0), ")\nTest rows soft-ruled: ", nrow(test) - sum(data_scores_parsed$Final[(nrow(train)+1):nrow(data_scores_parsed)] == 1), " (pure: ", sum(data_scores_parsed$Final[(nrow(train)+1):nrow(data_scores_parsed)] == 0), ")", sep = "")
    
  }
  
  if (ruling == TRUE) {
    
    return(list(scores = data, parsed_scores = data_scores_parsed, output = data_scores_parsed$Final))
    
  } else {
    
    return(list(scores = data))
    
  }
  
}
