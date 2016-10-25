#' (Un)Stratified Repeated k-fold for any type of label
#'
#' This function allows to create (un)stratified repeated folds from a label vector.
#' 
#' @param y Type: The label vector.
#' @param n Type: integer. The amount of repeated fold computations to perform. Defaults to \code{2}.
#' @param k Type: integer or vector of integers. The amount of folds to create. Causes issues if \code{length(y) < k} (e.g more folds than samples). If a vector of integers is supplied, then for each k-fold in the repeat N, k[N] is selected as the number of folds. Defaults to \code{5}.
#' @param stratified Type: boolean. Whether the folds should be stratified (keep the same label proportions) or not. Defaults to \code{TRUE}.
#' @param seed Type: integer or vector of integers. The seed for the random number generator. If a vector of integer is provided, its length should be at least longer than \code{n}. Otherwise (if an integer is supplied), it starts each fold with the provided seed, and adds 1 to the seed for every repeat. Defaults to \code{0}.
#' @param named Type: boolean. Whether the folds should be named. Defaults to \code{TRUE}.
#' @param weight Type: boolean. Whether to return the weights of each fold so their sum is equal to \code{1}. Defaults to \code{TRUE}.
#' 
#' @return A list of vectors for each fold, where an integer represents the row number, or a list of list containing \code{Folds} and \code{Weights} if \code{weight = TRUE}.
#' 
#' @examples
#' # Reproducible Stratified Repeated folds
#' data <- 1:5000
#' folds1 <- nkfold(y = data, n = 2, k = 5, stratified = TRUE, seed = 111)
#' folds2 <- nkfold(y = data, n = 2, k = 5, stratified = TRUE, seed = c(111, 112))
#' identical(folds1, folds2)
#' 
#' # Stratified Repeated Regression
#' data <- 1:5000
#' folds <- nkfold(y = data, n = 2, k = 5, stratified = TRUE)
#' for (i in 1:length(folds)) {
#'   print(mean(data[folds[[i]]]))
#' }
#' 
#' # Stratified Repeated Multi-class Classification
#' data <- c(rep(0, 250), rep(1, 250), rep(2, 250))
#' folds <- nkfold(y = data, n = 2, k = 5, stratified = TRUE)
#' for (i in 1:length(folds)) {
#'   print(mean(data[folds[[i]]]))
#' }
#' 
#' # Unstratified Repeated Regression
#' data <- 1:5000
#' folds <- nkfold(y = data, n = 2, k = 5, stratified = FALSE)
#' for (i in 1:length(folds)) {
#'   print(mean(data[folds[[i]]]))
#' }
#' 
#' # Unstratified Repeated Multi-class Classification
#' data <- c(rep(0, 250), rep(1, 250), rep(2, 250))
#' folds <- nkfold(y = data, n = 2, k = 5, stratified = FALSE)
#' for (i in 1:length(folds)) {
#'   print(mean(data[folds[[i]]]))
#' }
#' 
#' # Stratified Repeated 3-5-10 fold Cross-Validation all in one
#' data <- c(rep(0, 250), rep(1, 250), rep(2, 250))
#' str(nkfold(data, n = 3, k = c(3, 5, 10)))
#' 
#' @export

nkfold <- function(y, n = 2, k = 5, stratified = TRUE, seed = 0, named = TRUE, weight = FALSE) {
  
  folds <- list()
  
  if (length(k) == 1) {
    k <- rep(k, n)
  }
  
  if (weight) {
    
    list_weight <- numeric(0)
    nmind <- 0
    for (i in 1:n) {
      folded <- kfold(y = y, k = k[i], stratified = stratified, seed = ifelse(length(seed) == 1, seed + i - 1, seed[i]), named = FALSE)
      for (j in 1:k[i]) {
        nmind <- nmind + 1
        folds[[length(folds) + 1]] <- folded[[j]]
        names(folds)[length(folds)] <- paste("Rep", sprintf(paste("%0", floor(log10(n) + 1), "d", sep = ""), i), "Fold", sprintf(paste("%0", floor(log10(max(k)) + 1), "d", sep = ""), j), sep = "")
        list_weight[nmind] <- 1 / k[i] / n
      }
    }
    
    folds <- list(Folds = folds, Weights = list_weight)
    
  } else {
    
    for (i in 1:n) {
      folded <- kfold(y = y, k = k[i], stratified = stratified, seed = ifelse(length(seed) == 1, seed + i - 1, seed[i]), named = FALSE)
      for (j in 1:k[i]) {
        folds[[length(folds) + 1]] <- folded[[j]]
        names(folds)[length(folds)] <- paste("Rep", sprintf(paste("%0", floor(log10(n) + 1), "d", sep = ""), i), "Fold", sprintf(paste("%0", floor(log10(max(k)) + 1), "d", sep = ""), j), sep = "")
      }
    }
    
  }
  
  
  return(folds)
  
}