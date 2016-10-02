#' xgboost depth automated optimizer
#'
#' This function allows you to optimize the depth of xgboost in gbtree/dart booster given the other parameters constant.
#' Output is intentionally pushed to the global environment, specifically in \code{Laurae.xgb.opt.depth.df}, \code{Laurae.xgb.opt.depth.iter}, and \code{Laurae.xgb.opt.depth.best} to allow manual interruption without losing data.
#' Verbosity is automatic and cannot be removed. In case you need this function without verbosity, please compile the package after removing verbose messages.
#' In addition, a sink is forced. Make sure to run \code{sink()} if you interrupt (or if xgboost interrupts) prematurely the execution of the function. Otherwise, you end up with no more messages printed to your R console.
#' initial = 8, min_depth = 1, max_depth = 25, patience = 2, sd_effect = 0.001, worst_score = 0, learner = NA, better = max_better
#' @param initial The initial starting search depth. This is the starting point, along with \code{initial - 2} and \code{initial + 2} depths. Defaults to \code{8}.
#' @param min_depth The minimum accepted depth. If it is reached, the computation stops. Defaults to \code{1}.
#' @param max_depth The maximum accepted depth. If it is reached, the computation stops. Defaults to \code{25}.
#' @param patience How many iterations are allowed without improvement, excluding the initialization (the three first computations). Larger means more patience before stopping due to no improvement of the scored metric. Defaults to \code{2}.
#' @param sd_effect How much the standard deviation accounts in the score to determine the best depth parameter. Default to \code{0.001}.
#' @param worst_score The worst possible score of the metric used, as a numeric (non NA / Infinite) value. Defaults to \code{0}.
#' @param learner The learner function. It fetches everything needed from the global environment. Defaults to \code{my_learner}, which is an example of using that function.
#' @param better Should we optimize for the minimum or the maximum value of the performance? Defaults to \code{max_better} for maximization of the scored metric. Use \code{min_better} for the minimization of the scored metric.

#' @return Three elements forced in the global environment: \code{"Laurae.xgb.opt.depth.df"} for the dataframe with depth log (data.frame), \code{"Laurae.xgb.opt.depth.iter"} for the dataframe with iteration log (list), and \code{"Laurae.xgb.opt.depth.best"} for a length 1 vector with the best depth found (numeric).
#' 
#' @examples 
#' Please check xgb.opt.utils.R file in GitHub.
#' 
#' @export

xgb.opt.depth <- function(initial = 8, min_depth = 1, max_depth = 25, patience = 2, sd_effect = 0.001, worst_score = 0, learner = NA, better = max_better) {
  
  # initial = starting point of the optimizer
  # min_depth = minimum depth allowed
  # max_depth = maximum depth allowed
  # patience = how long to wait when doing a deep search towards a tail?
  # sd_effect = additional effect to mean to be put into score
  # learner = the function which returns c(mean, sd, nrounds) from a training whose input is depth
  # better = the function which compares a vector with mixed content (NAs and numeric values) and the current score, and return whether the current score is better than the known scores
  
  # output1 = Laurae.xgb.opt.depth.df data.frame of scores
  # output2 = Laurae.xgb.opt.depth.best numeric best depth found
  # output3 = Laurae.xgb.opt.depth.iter data.frame of iterations
  
  Laurae.xgb.opt.depth.df <<- data.frame(depth = 1:max_depth, nrounds = rep(NA, max_depth), mean = rep(NA, max_depth), sd = rep(NA, max_depth), score = rep(NA, max_depth))
  
  # Do not use an initial starting point below [min_depth+2, max_depth-2]
  Laurae.xgb.opt.depth.iter <<- data.frame(Iteration = 1:3, Depth = c(initial, initial - 2, initial + 2), Score = rep(worst_score, 3), Best = rep(worst_score, 3))
  
  # Example of initial = 8 => look [8, 6, 10]
  for (i in 1:3) {
    
    xgb.opt.depth.callback(i, learner, better, sd_effect)
    
  }
  
  # Is 8 better than 6 and 10?
  if (Laurae.xgb.opt.depth.iter[1, "Score"] == better(Laurae.xgb.opt.depth.df[, "score"])) {
    
    Laurae.xgb.opt.depth.iter[4, ] <<- c(4, initial - 1, worst_score, worst_score)
    Laurae.xgb.opt.depth.iter[5, ] <<- c(5, initial + 1, worst_score, worst_score)
    
    for (i in 4:5) {
      
      xgb.opt.depth.callback(i, learner, better, sd_effect)
      
    }
    
    cat("\n")
    cat("Best depth found was: ", Laurae.xgb.opt.depth.best, ".\n", sep = "")
    return()
    
    # Is 6 better than 8 & 10?
  } else if (Laurae.xgb.opt.depth.iter[2, "Score"] == better(Laurae.xgb.opt.depth.df[, "score"])) {
    
    # Check 7 for safety
    Laurae.xgb.opt.depth.iter[4, ] <<- c(4, initial - 1, worst_score, worst_score)
    xgb.opt.depth.callback(4, learner, better, sd_effect)
    
    if (Laurae.xgb.opt.depth.iter[4, "Best"] == Laurae.xgb.opt.depth.iter[4, "Score"]) {
      cat("\n")
      cat("Best depth found was: ", Laurae.xgb.opt.depth.best, ".\n", sep = "")
      return()
    }
    
    j <- 0
    
    for (i in 5:99999) {
      
      Laurae.xgb.opt.depth.iter[i, ] <<- c(i, initial - (i - 2), worst_score, worst_score)
      
      xgb.opt.depth.callback(i, learner, better, sd_effect)
      
      # Has improved
      if ((Laurae.xgb.opt.depth.best == (initial - (i - 2))) & (Laurae.xgb.opt.depth.iter[i, "Depth"] > min_depth)) {
        j <- 0
      } else {
        j <- j + 1
        # Reached the patience threshold?
        if ((j == patience) | (Laurae.xgb.opt.depth.iter[i, "Depth"] == min_depth)) {
          cat("\n")
          cat("Best depth found was: ", Laurae.xgb.opt.depth.best, ".\n", sep = "")
          return()
        }
      }
      
    }
    
    # Is 10 better than 6 & 8?
  } else {
    
    # Check 9 for safety
    Laurae.xgb.opt.depth.iter[4, ] <<- c(4, initial + 1, worst_score, worst_score)
    xgb.opt.depth.callback(4, learner, better, sd_effect)
    
    if (Laurae.xgb.opt.depth.iter[4, "Best"] == Laurae.xgb.opt.depth.iter[4, "Score"]) {
      cat("\n")
      cat("Best depth found was: ", Laurae.xgb.opt.depth.best, ".\n", sep = "")
      return()
    }
    
    j <- 0
    
    for (i in 5:99999) {
      
      Laurae.xgb.opt.depth.iter[i, ] <<- c(i, initial + (i - 2), worst_score, worst_score)
      
      xgb.opt.depth.callback(i, learner, better, sd_effect)
      
      # Has improved
      if ((Laurae.xgb.opt.depth.best == (initial + (i - 2))) & (Laurae.xgb.opt.depth.iter[i, "Depth"] < max_depth)) {
        j <- 0
      } else {
        j <- j + 1
        # Reached the patience threshold?
        if ((j == patience) | (Laurae.xgb.opt.depth.iter[i, "Depth"] == max_depth)) {
          cat("\n")
          cat("Best depth found was: ", Laurae.xgb.opt.depth.best, ".\n", sep = "")
          return()
        }
      }
      
    }
    
  }
  
}