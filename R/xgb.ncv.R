#' xgboost repeated cross-validation (Repeated k-fold)
#'
#' This function allows you to run a repeated cross-validation using xgboost, to get out of fold predictions, and to get predictions from each fold on external data.
#' It currently does not work for non 1-column prediction (only works for binary classification and regression).
#' Verbosity is automatic and cannot be removed. In case you need this function without verbosity, please compile the package after removing verbose messages.
#' In addition, a sink is forced. Make sure to run \code{sink()} if you interrupt (or if xgboost interrupts) prematurely the execution of the function. Otherwise, you end up with no more messages printed to your R console.
#' 
#' @param data The data as a matrix or sparse matrix.
#' @param label The label associated with the data.
#' @param extra_data The data you want to predict on using the fold models.
#' @param out_of_fold Should we predict out of fold? (this includes both \code{data} and \code{extra_data}). Defaults to \code{TRUE}.
#' @param nfolds How many folds should we use for the validation? The greater the better (increases linearly*ntimes the computation time. Defaults to \code{5}.
#' @param ntimes How many folds should we use? The greater the more stable results (increases linearly*nfolds the computation time.) Defaults to \code{3}.
#' @param nthread How many threads to run for xgboost? Defaults to \code{2}.
#' @param seed Which seed should we use globally for all commands dependent on a random seed? Defaults to \code{11111}.
#' @param verbose Should we print verbose data in xgboost? xgboost messages will be sinked in any case. Defaults to \code{1}.
#' @param print_every_n Every how many iterations should we print verbose data? xgboost messages will be sinked in any case.Defaults to \code{1}.
#' @param sinkfile What file name to give to the sink? This is where printed messages of xgboost will be stored.  Defaults to \code{"debug.txt"}.
#' @param booster What xgboost booster to use? Defaults to \code{"gbtree"} and must not be changed (does NOT work otherwise).
#' @param eta The shrinkage in xgboost. The lower the better, but increases exponentially the computation time as it gets lower. Defaults to \code{0.3}.
#' @param max_depth The maximum depth of each tree in xgboost. Defaults to \code{6}.
#' @param min_child_weight The minimum hessian weight needed in a child node. Defaults to \code{1}.
#' @param gamma The minimum loss reduction needed in a child node. Defaults to \code{0}.
#' @param subsample The sampling ratio of observations during each iteration. Use \code{0.632} to simulate Random Forests. Defaults to \code{1}.
#' @param colsample_bytree The sampling ratio of features during each iteration. Defaults to \code{1}.
#' @param num_parallel_tree How many trees to grow per iteration? A number higher than \code{1} simulates boosted Random Forests. Defaults to \code{1}.
#' @param maximum_rounds How many rounds until giving up boosting if not stopped early? Defaults to \code{100000}.
#' @param objective The objective function. Defaults to \code{"binary:logistic"}.
#' @param eval_metric The evaluation metric. Defaults to \code{"logloss"}.
#' @param maximize Should we maximize the evaluation metric? Defaults to \code{FALSE}.
#' @param early_stopping_rounds How many rounds the evaluation metric does not follow the maximization rule to force stopping a boosting iteration of xgboost on a fold? Defaults to \code{50}.
#' 
#' @return A list with two to four elements: \code{"scores"} for the scored folds (data.frame), \code{"folds"} for the folds IDs (list), \code{"preds"} for out of fold predictions (data.frame), and \code{"extra"} for extra data predictions per fold (data.frame).
#' 
#' @examples 
#' Pick your xgb.cv function, replace data by the initial matrix, insert the label,
#' check ntimes to the value you want, and change the sinkfile.
#' Unlist params if needed, and add the seed as a parameter.
#' 
#' @export

xgb.ncv <- function(data,
                    label,
                    extra_data = NA,
                    out_of_fold = TRUE,
                    nfolds = 5,
                    ntimes = 3,
                    nthread = 2,
                    seed = 11111,
                    verbose = 1,
                    print_every_n = 1,
                    sinkfile = "debug.txt",
                    booster = "gbtree",
                    eta = 0.30,
                    max_depth = 6,
                    min_child_weight = 1,
                    gamma = 0,
                    subsample = 1.00,
                    colsample_bytree = 1.00,
                    num_parallel_tree = 1,
                    maximum_rounds = 100000,
                    objective = "binary:logistic",
                    eval_metric = "logloss",
                    maximize = FALSE,
                    early_stopping_rounds = 50) {
  
  set.seed(seed)
  folds <- createMultiFolds(label, k = nfolds, times = ntimes)
  
  score_list <- data.frame(folds = seq(1, NROW(names(folds))), scores = rep(0, NROW(names(folds))), rounds = rep(0, NROW(names(folds))))
  
  if (out_of_fold == TRUE) {
    
    preds_list <- data.frame(matrix(ncol = ntimes, nrow = nrow(data)))
    colnames(preds_list) <- paste("Repeat", 1:ntimes)
    
    if (is.na(extra_data)[[1]][1] == FALSE) {
      
      extra_list <- data.frame(matrix(ncol = (nfolds * ntimes), nrow = nrow(data)))
      colnames(extra_list) <- paste(names(folds))
      extra_xgb <- xgb.DMatrix(data = extra_data)
      gc(verbose = FALSE)
      
    }
    
  }
  
  
  StartTime <- System$currentTimeMillis()
  
  for (i in names(folds)) {
    
    gc(verbose = FALSE)
    training_xgb <- xgb.DMatrix(data = data[folds[[i]], ], label = label[folds[[i]]])
    gc(verbose = FALSE)
    testing_xgb <- xgb.DMatrix(data = data[-folds[[i]], ], label = label[-folds[[i]]])
    
    gc(verbose = FALSE)
    set.seed(seed)
    
    sink(file = sinkfile, append = TRUE, split = FALSE)
    cat("Repeat ", (((which(i == names(folds)) - 1) %/% nfolds) + 1), " - Fold ", (((which(i == names(folds)) - 1) %/% nfolds) + 1), "\n", sep = "")
    best_out <- xgb.train(eta = eta,
                          max_depth = max_depth,
                          min_child_weight = min_child_weight,
                          gamma = gamma,
                          subsample = subsample,
                          colsample_bytree = colsample_bytree,
                          num_parallel_tree = num_parallel_tree,
                          data = training_xgb, 
                          maximum_rounds = maximum_rounds, 
                          verbose = verbose,
                          maximize = maximize,
                          nthread = nthread,
                          early.stop.round = early_stopping_rounds,
                          print.every.n = print_every_n,
                          objective = objective,
                          booster = "gbtree",
                          metrics = eval_metric,
                          watchlist = list(train = training_xgb, test = testing_xgb))
    sink()
    
    gc(verbose = FALSE)
    score_list[which(i == names(folds)), "rounds"] <- which.min(best_out$evaluation_log[[2]])
    score_list[which(i == names(folds)), "scores"] <- best_out$evaluation_log[[2]][score_list[which(i == names(folds)), "rounds"]]
    
    if (out_of_fold == TRUE) {
      
      gc(verbose = FALSE)
      preds_list[-folds[[i]], (((which(i == names(folds)) - 1) %/% nfolds) + 1)] <- predict(best_out, newdata = testing_xgb, ntreelimit = score_list[which(i == names(folds)), "rounds"])
      
      gc(verbose = FALSE)
      if (is.na(extra_data)[[1]][1] == FALSE) {
        extra_list[, which(i == names(folds))] <- predict(extra_data, newdata = extra_xgb, ntreelimit = score_list[which(i == names(folds)), "rounds"])
      }
      
    }
    
    gc(verbose = FALSE)
    CurrentTime <- System$currentTimeMillis()
    SpentTime <- (CurrentTime - StartTime) / 1000
    cat("[Fold ", sprintf("%02d", which(i == names(folds))), "/", sprintf("%02d", NROW(names(folds))), " | CPU: ", sprintf("%.02f", SpentTime), "s | ETA: ", sprintf("%.02f", (NROW(names(folds)) - which(i == names(folds))) * SpentTime / which(i == names(folds))), "s]: ", names(best_out$evaluation_log)[2], "=", sprintf("%.06f", score_list[which(i == names(folds)), "scores"]), " (", sprintf("%04d", score_list[which(i == names(folds)), "rounds"]), " rounds). Currently M/SD: ", sprintf("%.03f", mean(score_list[1:(which(i == names(folds))), 2])), "+", sprintf("%.03f", sd(score_list[1:(which(i == names(folds))), 2])), "\n", sep = "")
    
  }
  
  cat("-----\nFinal results (M/SD): ", sprintf("%.06f", mean(score_list[, 2])), "+", sprintf("%.06f", sd(score_list[, 2])), " with ", sprintf("04d", mean(score_list[, 3])), "+", sprintf("04d", sd(score_list[, 3])), " rounds", sep = "")
  
  if (out_of_fold == TRUE) {
    
    if (is.na(extra_data)[[1]][1] == FALSE) {
      return(list(scores = score_list, folds = folds, preds = preds_list, extra = extra_list))
    } else {
      return(list(scores = score_list, folds = folds, preds = preds_list))
    }
    
  } else {
    
    return (list(scores = score_list, folds = folds))
    
  }
  
}