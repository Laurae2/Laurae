#' t-SNE grid search function
#'
#' This function allows you to search a perplexity hyperparameter range along with different seeds.
#' Verbosity is automatic and cannot be removed. In case you need this function without verbosity, please compile the package after removing verbose messages.
#' 
#' @param data The data.frame input into t-SNE
#' @param output_dims How many dimensions to output? (increases exponentially the computation time)
#' @param input_dims How many input dimensions to use? (defaults to \code{ncol(data)}) - this should be changed when using pca to a value below the default value
#' @param perplexity_range What hyperparameter interval to look for? (should be formatted as (min, max)) - defaults to \code{c(1, floor((ncol(data)-1)/3))} - to grid search a seed for a fixed perplexity value, use min = max as inputs - the best pragmatic perpelxity for the lowest loss is typically \code{floor((ncol(data)-1)/3)}
#' @param tries How many seeds to test t-SNE per perplexity value? (this increases linearly the computation time)
#' @param iterations How many iterations per t-SNE are performed? (this increases approximately linearly the computation time)
#' @param theta Use exact t-SNE (0) or Barnes-Hut t-SNE? (in ]0, 1] interval)
#' @param check_duplicates Should t-SNE check for duplicates? (unlike common beliefs, t-SNE works perfectly with the existance of identical observations)
#' @param pca Should a PCA (Principal Component Analysis) be performed? (note: it is performed every iteration, therefore it is computationally intensive and should be avoided - if you need PCA, please input the PCA instead of the data)
#' @param is_distance Is the input a distance matrix? (assumes the diagonal cuts in half the input data.frame)
#' 
#' @return A list with the best (lowest loss at a specific iteration) t-SNE elements from Rtsne
#' 
#' @examples 
#' tsne_model <- tsne_grid(initial_diag = initial_diag, dims = 3, perplexity_range = c(floor((ncol(initial_diag)-1)/3), floor((ncol(initial_diag)-1)/3)), tries = 100, iterations = 10000, theta = 0.0, check_duplicates = FALSE, pca = FALSE, is_distance = TRUE)
#' 
#' @export

tsne_grid <- function(data, output_dims, input_dims = ncol(data), perplexity_range = c(1, floor((ncol(data)-1)/3)), tries = 10, iterations = 10000, theta = 0.00, check_duplicates = FALSE, pca = FALSE, is_distance = FALSE) {
  
  best_cost <- 99999999
  
  for (i in perplexity_range[1]:perplexity_range[2]) {
    
    for (j in 1:tries) {
      
      set.seed(j)
      tsne_model <- Rtsne(data,
                          dims = output_dims,
                          initial_dims = input_dims,
                          perplexity = i,
                          theta = theta,
                          check_duplicates = check_duplicates,
                          pca = pca,
                          max_iter = iterations,
                          verbose = FALSE,
                          is_distance = is_distance)
      
      cat("[Grid Search: perplexity ", sprintf(paste("%", floor(log10(perplexity_range[2]))+1, "d", sep = ""), i), "/", perplexity_range[2], ", ", sprintf(paste("%", floor(log10(tries))+1, "d", sep = ""), j), " out of ", tries, "]: Sum of Squared Errors = ", sprintf("%11.9f", min(tsne_model$itercosts)), sep = "")
      
      if (best_cost > min(tsne_model$itercosts)) {
        best_run <- j
        best_perp <- i
        best_cost <- min(tsne_model$itercosts)
        best_iter <- which.min(tsne_model$itercosts)[1]
        cat(" - Best is now ", sprintf("%11.9f", best_cost), " @ perplexity ", best_perp, " (***)\n", sep = "")
      } else {
        cat(" -    Best was ", sprintf("%11.9f", best_cost), " @ perplexity ", best_perp, " - NULL\n", sep = "")
      }
      
    }
    
  }
  
  cat("\nRunning best T-SNE\n")
  gc(verbose = FALSE)
  set.seed(best_run)
  best <- Rtsne(data,
                dims = output_dims,
                initial_dims = input_dims,
                perplexity = best_perp,
                theta = theta,
                check_duplicates = check_duplicates,
                pca = pca,
                max_iter = best_iter*50,
                verbose = FALSE,
                is_distance = is_distance)
  cat("[Seed ", best_run, " | Iteration ", best_iter*50, ": perplexity ", best_perp, "]: Sum of Squared Errors = ", sprintf("%11.9f", min(best$itercosts)), "\nReturning best model...", sep = "")
  
  return(best)
  
}
