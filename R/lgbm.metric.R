#' LightGBM Metric Output
#'
#' This function allows to get the metric values from a LightGBM log.
#' 
#' @param model Type: list, vector of characters, character. If a list, it should come from a trained model. If a vector of characters, it must be the model text. If a character, it must be the exact file path to the log file. It cannot work without training with \code{verbose} set to \code{FALSE}.
#' @param metrics Type: boolean. Whether to return the metrics table (\code{TRUE}) or the best iteration (\code{FALSE}).
#' @param data.table Type: boolean. Whether to return a data.table (\code{TRUE}) or a data.frame (\code{FALSE}). Defaults to \code{TRUE}. Useless when \code{metrics} is set to \code{FALSE}.
#' 
#' @return A data.table (or data.frame) with 3 to N columns, with at least: \code{c("Iteration", "Total_Time", "Round_Time")}.
#' 
#' @examples
#' \dontrun{
#' # Get the metrics of the model trained (requires verbose = FALSE during training)
#' lgbm.metric(workingdir = trained[["Path"]],
#' log_name = trained[["Log"]],
#' metrics = TRUE,
#' data.table = TRUE)
#' 
#' # Get the best iteration of the model trained (requires verbose = FALSE during training)
#' lgbm.metric(workingdir = trained[["Path"]],
#' log_name = trained[["Log"]],
#' metrics = FALSE)
#' }
#' 
#' @export

lgbm.metric <- function(
  model,
  metrics = TRUE,
  data.table = TRUE) {
  
  # Load data
  if (is.list(model)) {
    # Model was provided
    model <- readLines(file.path(model[["Path"]], model[["Log"]]))
  } else {
    if (length(model) == 1) {
      # Full path was provided
      model <- readLines(model)
    } # Log was provided directly
  }
  model <- model[grep("iteration", model, ignore.case = TRUE)]
  model <- gsub("LightGBM Info ", "", gsub("\\[|\\]", "", model))
  model <- gsub("LightGBM Error ", "", gsub("\\[|\\]", "", model))
  model <- gsub("LightGBM Fatel ", "", gsub("\\[|\\]", "", model))
  
  if (substr(model[length(model)], 1, 5) == "Early") {
    # Capture the early stopping message
    best_round <- unique(na.omit(as.numeric(unlist(strsplit(unlist(model[length(model)]), "[^0-9]+")))))
    iterations <- best_round[1]
    if (!metrics) return(best_round[length(best_round)])
    #best_round <- best_round[length(best_round)]
    model <- model[1:(length(model) - 1)]
  } else {
    iterations <- as.numeric(gsub(".*finished \\s*| iteration.*", "", model[length(model)]))
    if (!metrics) return(iterations)
  }
  
  # Get timings
  timing_model <- model[grep("seconds elapsed", model, ignore.case = TRUE)]
  timing_positions <- regexpr(" ", timing_model)
  timing_metric <- as.numeric(substr(timing_model, 1, timing_positions - 1))
  output <- data.table(Timing_Absolute = timing_metric)
  
  # Decoalesce timing
  output[, Timing_Metric := c(timing_metric[1], c(timing_metric[2:length(timing_metric)] - timing_metric[1:(length(timing_metric) - 1)]))]
  
  loss_model <- model[grep("Iteration:", model, ignore.case = TRUE)]
  temp_grep <- grep("training", loss_model, ignore.case = TRUE)
  testing_model <- loss_model[which(!(1:length(loss_model)) %in% temp_grep)]
  training_model <- loss_model[temp_grep]
  
  if ((length(training_model) > 0) & (length(testing_model) > 0)) {
    # Has train & test at the same time
    
    loss_labels <- gsub(".*'s |: *", "", training_model)[1:(length(training_model) / iterations)]
    loss_labels <- gsub("loss", "", loss_labels)
    loss_position <- regexpr("[0-9]", substr(loss_labels, 3, nchar(loss_labels)))
    loss_labels <- substr(loss_labels, 1, loss_position)
    loss_labels[loss_labels == "l1 "] <- "L1"
    loss_labels[loss_labels == "l2 "] <- "L2"
    loss_labels[loss_labels == "au"] <- "AUC"
    loss_labels[loss_labels == "log"] <- "Log"
    loss_labels[loss_labels == "error rat"] <- "Error"
    loss_training <- regexpr("'s", training_model)
    loss_training <- substr(training_model, loss_training + 3, nchar(training_model))
    loss_thresh <- regexpr(":", loss_training)
    loss_training <- as.numeric(substr(loss_training, loss_thresh + 2, nchar(loss_training)))
    loss_testing <- regexpr("'s", testing_model)
    loss_testing <- substr(testing_model, loss_testing + 3, nchar(testing_model))
    loss_thresh <- regexpr(":", loss_testing)
    loss_testing <- as.numeric(substr(loss_testing, loss_thresh + 2, nchar(loss_testing)))
    
    for (i in 1:length(loss_labels)) {
      output[[paste("Train_", loss_labels[i], sep = "")]] <- loss_training[(1:(length(loss_training) / length(loss_labels))) * length(loss_labels) - length(loss_labels) + i]
      output[[paste("Test_", loss_labels[i], sep = "")]] <- loss_testing[(1:(length(loss_testing) / length(loss_labels))) * length(loss_labels) - length(loss_labels) + i]
    }
    
  } else {
    
    if (length(training_model) > 0) {
      # Has train
      
      loss_labels <- gsub(".*'s |: *", "", training_model)[1:(length(training_model) / iterations)]
      loss_labels <- gsub("loss", "", loss_labels)
      loss_position <- regexpr("[0-9]", substr(loss_labels, 3, nchar(loss_labels)))
      loss_labels <- substr(loss_labels, 1, loss_position)
      loss_labels[loss_labels == "l1 "] <- "L1"
      loss_labels[loss_labels == "l2 "] <- "L2"
      loss_labels[loss_labels == "au"] <- "AUC"
      loss_labels[loss_labels == "log"] <- "Log"
      loss_labels[loss_labels == "error rat"] <- "Error"
      loss_training <- regexpr("'s", training_model)
      loss_training <- substr(training_model, loss_training + 3, nchar(training_model))
      loss_thresh <- regexpr(":", loss_training)
      loss_training <- as.numeric(substr(loss_training, loss_thresh + 2, nchar(loss_training)))
      
      for (i in 1:length(loss_labels)) {
        output[[paste("Train_", loss_labels[i], sep = "")]] <- loss_training[(1:(length(loss_training) / length(loss_labels))) * length(loss_labels) - length(loss_labels) + i]
      }
      
    }
    
    if (length(testing_model) > 0) {
      # Has test
      
      loss_labels <- gsub(".*'s |: *", "", testing_model)[1:(length(testing_model) / iterations)]
      loss_labels <- gsub("loss", "", loss_labels)
      loss_position <- regexpr("[0-9]", substr(loss_labels, 3, nchar(loss_labels)))
      loss_labels <- substr(loss_labels, 1, loss_position)
      loss_labels[loss_labels == "l1 "] <- "L1"
      loss_labels[loss_labels == "l2 "] <- "L2"
      loss_labels[loss_labels == "au"] <- "AUC"
      loss_labels[loss_labels == "log"] <- "Log"
      loss_labels[loss_labels == "error rat"] <- "Error"
      loss_testing <- regexpr("'s", testing_model)
      loss_testing <- substr(testing_model, loss_testing + 3, nchar(testing_model))
      loss_thresh <- regexpr(":", loss_testing)
      loss_testing <- as.numeric(substr(loss_testing, loss_thresh + 2, nchar(loss_testing)))
      
      for (i in 1:length(loss_labels)) {
        output[[paste("Test_", loss_labels[i], sep = "")]] <- loss_testing[(1:(length(loss_testing) / length(loss_labels))) * length(loss_labels) - length(loss_labels) + i]
      }
      
    }
    
  }
  
  # Check for data.table need
  if (!data.table) setDF(output)
  
  # Return
  return(output)
  
}