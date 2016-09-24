# Return minimum as best

min_better <- function(cp) {
  
  return(min(cp, na.rm = TRUE))
  
}

# Return maximum as best

max_better <- function(cp) {
  
  return(max(cp, na.rm = TRUE))
  
}

# Example of learner function

my_learner <- function(depth) {
  
  sink(file = "log.txt", append = TRUE, split = FALSE)
  gc()
  set.seed(11111)
  temp_model <- xgb.cv(data = train_temp,
                       nthread = 2,
                       nfold = 2,
                       nrounds = 10,
                       max_depth = depth,
                       eta = 1,
                       #gamma = 0.1,
                       subsample = 0.7,
                       colsample_bytree = 0.7,
                       booster = "gbtree",
                       eval_metric = "error",
                       maximize = TRUE,
                       early_stopping_rounds = 2,
                       objective = "binary:logistic",
                       verbose = TRUE,
                       prediction = TRUE)
  sink()
  return(c(temp_model$evaluation_log[[3]][temp_model$best_iteration], temp_model$evaluation_log[[4]][temp_model$best_iteration], temp_model$best_iteration))
  
}

# Example of callback function

xgb.opt.depth.callback <- function(i, learner, better, sd_effect) {
  
  cat("\nExploring depth ", sprintf("%02d", Laurae.xgb.opt.depth.iter[i, "Depth"]), ": ")
  Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], c("mean", "sd", "nrounds")] <<- learner(Laurae.xgb.opt.depth.iter[i, "Depth"])
  Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], "score"] <<- Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], "mean"] + (Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], "sd"] * sd_effect)
  Laurae.xgb.opt.depth.iter[i, "Score"] <<- Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], "score"]
  Laurae.xgb.opt.depth.iter[i, "Best"] <<- better(Laurae.xgb.opt.depth.df[, "score"])
  Laurae.xgb.opt.depth.best <<- which(Laurae.xgb.opt.depth.df[, "score"] == Laurae.xgb.opt.depth.iter[i, "Best"])[1]
  cat("[",
      sprintf("%05d", Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], "nrounds"]),
      "] ",
      sprintf("%.08f", Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], "mean"]),
      ifelse(is.na(Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], "mean"]) == TRUE, "", paste("+", sprintf("%.08f", Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], "sd"]), sep = "")),
      " (Score: ",
      sprintf("%.08f", Laurae.xgb.opt.depth.df[Laurae.xgb.opt.depth.iter[i, "Depth"], "score"]),
      ifelse(Laurae.xgb.opt.depth.iter[i, "Best"] == Laurae.xgb.opt.depth.iter[i, "Score"], " <<<)", "    )"),
      " - best is: ",
      Laurae.xgb.opt.depth.best,
      sep = "")
  
}