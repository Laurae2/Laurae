#' Outlying bivariate linear continuous association rule finder
#'
#' This function allows you to search for association rules on outlying bivariate linear continuous features against a binary label. The predicted label is 0, and the overfitting severity is very high (see: Kaggle's Santander Customer Satisfaction competition).
#' Unlike the univariate rule finder, it cannot be used to score outliers first (a 300 feature matrix can get to about 9000 features...).
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
#' 
#' @return A vector with \code{nrow(data)} elements: the general result for each observation using bivariate rules.
#' 
#' @examples
#' \dontrun{
#' rules <- rule_double(data = scored_data, label = target, iterations = 100)
#' preds <- preds[rules[(length(target)+1):(nrow(data))] == 0] <- 0
#' }
#' 
#' @export

rule_double <- function(data,
                        label,
                        train_rows = length(label),
                        iterations = 1000,
                        minimal_score = 25,
                        minimal_node = 5,
                        false_negatives = 2,
                        seed = 11111) {
  
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
  
  scored_rows <- rep(1, nrow(data))
  Counter <- 0
  MaxCounter <- ncol(data)*(ncol(data) - 1)
  MaxChar <- 0
  Paster <- paste("%0", nchar(MaxCounter, "width"), "d", sep = "")
  TrainRows <- 1:train_rows
  TestRows <- (train_rows+1):(train_rows+nrow(data))
  StartTime <- System$currentTimeMillis()
  
  for (i in colnames(data)) {
    
    for (j in colnames(data)[-which(i == colnames(data))]) {
      
      #print text for default checking
      Counter <- Counter + 1
      CurrentTime <- System$currentTimeMillis()
      SpentTime <- (CurrentTime - StartTime) / 1000
      tempText <- paste("\r[", sprintf(Paster, Counter) , "/", MaxCounter, " | CPU = ", round(SpentTime, digits = 2), "s | ETA = ", round((MaxCounter - Counter) * SpentTime / Counter, 2), "s]: ", i, ":", j, " parsed!", sep = "")
      cat(tempText, sep = "")
      
      #merge columns
      tempCol <- data.frame(v1 = data[[i]], v2 = data[[j]], check.names = FALSE, stringsAsFactors = FALSE)
      
      #compute Mahalonobis distance (df, m, sx) with near-zero tolerance to avoid unexpected interruptions
      if (sum(cor(tempCol)) > 3.999999) {
        
        #give up computing the inverse matrix and Mahalonobis distance due to singularity/ill-conditioned matrix
        
      } else {
        
        #try compute inverse matrix and Mahalonobis distance
        tryCatch(tempCol <- mahalanobis(tempCol, colMeans(tempCol), cov(tempCol), tol=1e-30))
        
      }
      
      if (class(tempCol) == "data.frame") {
        
        #computation failed, ignore what to do
        MaxChar <- nchar(tempText)
        cat("\r", rep(" ", MaxChar), sep = "")
        CurrentTime <- System$currentTimeMillis()
        SpentTime <- (CurrentTime - StartTime) / 1000
        tempText <- paste("\r[", sprintf(Paster, Counter) , "/", MaxCounter, " | CPU = ", round(SpentTime, digits = 2), "s | ETA = ", round((MaxCounter - Counter) * SpentTime / Counter, 2), "s]: ", i, ":", j, " is singular.\n", sep = "")
        cat(tempText, sep = "")
        
        
      } else {
        
        #successful computation, continue
        
        #score the data against outliers locally
        data_scores <- scores(tempCol)
        scoring_input <- data_scores[TrainRows] #get scores from train set
        min_allowance <- min(scoring_input) #get the maximum allowed score
        max_allowance <- max(scoring_input) #get the maximum allowed score
        
        #gradient descent the outliers to find local isolated nodes
        set.seed(seed)
        optimized_output <- optim(par = c(min_allowance, max_allowance), optimized_func, method = "L-BFGS-B", target = label, scores = scoring_input, min_score = minimal_score, min_node = minimal_node, false_neg = false_negatives, lower = min_allowance, upper = max_allowance, control = list(maxit = iterations, trace = 0))
        
        if (!(optimized_output$value == 0)) {
          
          #not pure node?
          #has no value for us
          #overwrite print
          
          MaxChar <- nchar(tempText)
          
          cat("\r", rep(" ", MaxChar), sep = "")
          
        } else {
          
          #pure node?
          #has value for us
          
          #MaxChar <- 0
          
          #compute rows found
          tempRows <- (data_scores >= optimized_output$par[2]) | (data_scores <= optimized_output$par[1])
          tempRows_train <- sum(tempRows[TrainRows])
          tempRows_test <- sum(tempRows[TestRows])
          
          #update target rows
          tempInt <- sum(scored_rows[TestRows] == 0)
          scored_rows[tempRows] <- 0
          tempInt <- sum(scored_rows[TestRows] == 0) - tempInt
          
          #rewrite the current line
          CurrentTime <- System$currentTimeMillis()
          SpentTime <- (CurrentTime - StartTime) / 1000
          cat("\r[", sprintf(Paster, Counter) , "/", MaxCounter, " | CPU = ", round(SpentTime, digits = 2), "s | ETA = ", round((MaxCounter - Counter) * SpentTime / Counter, 2),"s]: ", i, ":", j, " analysis led to: ", tempRows_train, "|", tempRows_test, " (", sum(scored_rows[TrainRows] == 0), "|", sum(scored_rows[TestRows] == 0), ")", sep = "")
          
          if (tempInt == 0) {
            
            #if it added nothing to our test set
            cat(" - No improvement.\n", sep = "")
            
          } else {
            
            #if it added something to our test set
            cat(" | improved slightly! (+", tempInt, ")\n", sep = "")
            
          }
          
        }
        
      }
      
    }
    
  }
  
  return(scored_rows)
  
}
