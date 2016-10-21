#' (Un)Stratified k-fold for any type of label
#'
#' This function allows to create (un)stratified folds from a label vector.
#' 
#' @param y Type: The label vector.
#' @param k Type: integer. The amount of folds to create. Causes issues if \code{length(y) < k} (e.g more folds than samples). Defaults to \code{5}.
#' @param stratified Type: boolean. Whether the folds should be stratified (keep the same label proportions) or not. Defaults to \code{TRUE}.
#' @param seed Type: integer. The seed for the random number generator. Defaults to \code{0}.
#' 
#' @return A list of vectors for each fold, where an integer represents the row number.
#' 
#' @examples
#' # Reproducible Stratified folds
#' data <- 1:5000
#' folds1 <- kfold(y = data, k = 5, stratified = TRUE, seed = 111)
#' folds2 <- kfold(y = data, k = 5, stratified = TRUE, seed = 111)
#' identical(folds1, folds2)
#' 
#' # Stratified Regression
#' data <- 1:5000
#' folds <- kfold(y = data, k = 5, stratified = TRUE)
#' for (i in 1:length(folds)) {
#'   print(mean(data[folds[[i]]]))
#' }
#' 
#' # Stratified Multi-class Classification
#' data <- c(rep(0, 250), rep(1, 250), rep(2, 250))
#' folds <- kfold(y = data, k = 5, stratified = TRUE)
#' for (i in 1:length(folds)) {
#'   print(mean(data[folds[[i]]]))
#' }
#' 
#' # Unstratified Regression
#' data <- 1:5000
#' folds <- kfold(y = data, k = 5, stratified = FALSE)
#' for (i in 1:length(folds)) {
#'   print(mean(data[folds[[i]]]))
#' }
#' 
#' # Unstratified Multi-class Classification
#' data <- c(rep(0, 250), rep(1, 250), rep(2, 250))
#' folds <- kfold(y = data, k = 5, stratified = FALSE)
#' for (i in 1:length(folds)) {
#'   print(mean(data[folds[[i]]]))
#' }
#' 
#' @export

kfold <- function(y, k = 5, stratified = TRUE, seed = 0) {
  
  set.seed(seed)
  
  if (stratified) {
    # Stratified k-fold cross-validation
    # Source: https://github.com/dmlc/xgboost/blob/master/R-package/R/utils.R
    
    if (is.numeric(y)) {
      
      cuts <- floor(length(y) / k)
      if (cuts < 2) cuts <- 2
      if (cuts > 5) cuts <- 5
      y <- cut(y,
               unique(stats::quantile(y, probs = seq(0, 1, length = cuts))),
               include.lowest = TRUE)
    }
    
    if (k < length(y)) {
      y <- factor(as.character(y))
      numInClass <- table(y)
      foldVector <- vector(mode = "integer", length(y))
      
      for (i in 1:length(numInClass)) {
        seqVector <- rep(1:k, numInClass[i] %/% k)
        if (numInClass[i] %% k > 0) seqVector <- c(seqVector, sample(1:k, numInClass[i] %% k))
        foldVector[which(y == dimnames(numInClass)$y[i])] <- sample(seqVector)
      }
    } else {
      foldVector <- seq(along = y)
    }
    
    out <- split(seq(along = y), foldVector)
    names(out) <- NULL
    return(out)
    
  } else {
    # Unstratfied k-fold cross-validation
    
    mini_y <- 1:length(y)
    folded <- list()
    for (i in 1:k) {
      folded[[i]] <- sample(mini_y, floor(length(mini_y) / (k + 1 - i)))
      mini_y <- mini_y[!mini_y %in% folded[[i]]]
      folded[[i]] <- c(folded[[i]], folded[[i]])
    }
    
    return(folded)
    
  }
  
}
