#' The Non-Linear Feature Engineering Assistant
#'
#' This function is a massive helper in feature engineering, supposing your variables are already conditioned well enough for 2-way or deeper interactions and you are looking for non-linear relationships. It uses a decision tree (Classification and Regression Trees), and supports factors, integer, and numeric variables.
#' 
#' To use this function properly, you require to set the \code{max_depth} to a very small value (like \code{3}). This ensures interpretability.
#' 
#' Moreover, if you have a sparse frame (with lot of missing values), it is important to keep an eye at \code{surrogate_type} and \code{surrogate_style} as they will dictate whether a split point will be made depending on the missing values. Default values are made to handle them appropriately. However, if your intent is to penalize missing values (for instance if missing values are anomalies), changing their values respectively to \code{0} and \code{1} is recommended.
#' 
#' @param data Type: data.frame (preferred) or data.table. Your data, preferably a data.frame but it "should" also work perfectly with data.table.
#' @param label Type: vector. Your labels.
#' @param ban Type: vector of characters or of numerics The names (or column numbers) of variables to be banned from the decision tree. Defaults to \code{NULL}, which means no variables are banned (all variables are potentially used for the decision tree).
#' @param antiban Type: boolean. Whether banned variable selection should be inverted, which means if \code{TRUE}, the \code{ban} transforms into a selection (which bans all other variables not "banned" initially). Defaults to \code{FALSE}.
#' @param type Type: character. The type of problem to solve. Either classification (\code{"class"}), regression (\code{"anova"}), count (\code{"poisson"}), or survival (\code{"exp"}). Defaults to \code{"auto"}, which will attempt to find the base type (classification / regression) of model to create using simple heuristics.
#' @param split Type: character. If a classification task has been requested (\code{type = "class"}), then the split must be either set to \code{"gini"} (for Gini index) or \code{"information"} (for Information Gain) as the splitting rule. Defaults to \code{"information"} as it is less biased than \code{"gini"} when it comes to cardinalities.
#' @param folds Type: integer or list of vectors. The folds to use for cross-validation. If you intend to keep the same folds over and over, it is preferrable to provide your own list of folds. A numeric vector matching the length of \code{label} is also valid.
#' @param seed Type: integer. The random seed applied to the decision tree and the fold generation (if required).
#' @param verbose Type: boolean. Whether to print debug information about the model. For each node, a maximum of \code{competing_splits + surrogate_search} rows will be printed. Defaults to \code{TRUE}.
#' @param plots Type: boolean. Whether to plot debug information about the model. If using knitr / Rmarkdown, you will have two plots printed: the complexity plot, and the decision tree. Without knitr / Rmarkdown, make sure you look at both. Defaults to \code{TRUE}.
#' @param min_split Type: integer. The minimum number of observations in a node to allow a split to be made. If this number is not reached in a node, the node is kept but any other potential splits are cancelled. Keep it large to avoid overfitting. Defaults to \code{max(20, nrow(data) / 1000)}, which is the maximum between 20 and the 0.1\% of the number of observations.
#' @param max_depth Type: numeric. The maximum depth of the decision tree. Do not set to large values if the intent is for analysis. Defaults to \code{3}. Any value greater than \code{30} will cause issues on 32-bit operating systems due to C code.
#' @param min_bucket Type: integer. The minimum number of observations in a leaf. If this number is not reached in a leaf, the leaf is destroyed. Defaults to \code{round(min_split/3)}, which means by defaults at least 7 to approximately 0.033\% of the number of observations.
#' @param min_improve Type: numeric. The minimum fitting improvement to create a node (complexity parameter in Classification and Regression Trees). For regression, the requirement for a leaf to be created and kept is an R-squared increase by at least \code{min_improve}. For classification, the purity (issued from Gini or Information Gain) must increase by at least \code{min_improve}.
#' @param competing_splits Type: numeric. The number of best splitting rules retained per split. When using \code{verbose = TRUE}, each node will have \code{competing_splits} rules printed, if they are adequate enough (instead of only one splitting rule). This allows the user to lookup for more details. Defaults to \code{4}.
#' @param surrogate_search Type: numeric. The number of surrogate splits to look for. A greater number means more surrogates will be looked for, but increased computation time is required. They are also printed when \code{verbose = TRUE}. Defaults to \code{5}.
#' @param surrogate_type Type: numeric. Controls the surrogate creation, with three possible values. If set to \code{0}, any surrogates with missing values are not used for the tree. If set to \code{1}, when all surrogates are with missing values, they are not used the tree. If set to \code{2}, when all surrogates are not used, the majority rule is used (Breiman tree). Sparse frames should preferably use \code{2}. It is recommended to use \code{2} as it handles better missing values, which is the default. Set to \code{0} if you need to ignore as much as possible missing values.
#' @param surrogate_style Type: numeric. Controls the selection of the best surrogate, with two values. If set to \code{1}, any missing values in the surrogate is removed to compute the correctness of the surrogate. If set to \code{0}, it ignores any missing values and takes into account all observations to compute the correctness of the surrogate. Defaults to \code{0}. Set to \code{1} if you need to ignore as much as possible missing values.
#' 
#' @return The fitted \code{rpart} model.
#' 
#' @examples
#' \dontrun{
#' # An example of a heavily regularized decision tree
#' # Settings are intentionally difficult enough for a decision tree
#' # This way, only great split points are reported
#' FeatureLookup(data,
#'               label,
#'               ban = c("CAR", "TOBACCO"),
#'               antiban = FALSE,
#'               type = "anova",
#'               folds = 20,
#'               seed = 11111,
#'               verbose = TRUE,
#'               plots = TRUE,
#'               max_depth = 3,
#'               min_split = 1000,
#'               min_bucket = 200,
#'               min_improve = 0.10,
#'               competing_splits = 10,
#'               surrogate_search = 10,
#'               surrogate_type = 2,
#'               surrogate_style = 0)
#' }
#' 
#' @export

FeatureLookup <- function(data, label, ban = NULL, antiban = FALSE, type = "auto", split = "information", folds = 5, seed = 0, verbose = TRUE, plots = TRUE, max_depth = 4, min_split = max(20, nrow(data) / 1000), min_bucket = round(min_split/3), min_improve = 0.01, competing_splits = 2, surrogate_search = 5, surrogate_type = 2, surrogate_style = 0) {
  # data = your data (data.frame)
  # label = your label vector
  # type = either classification ("class"), regression ("anova"), count ("poisson"), or survival ("exp")
  # inplace = if TRUE, transforms data into a data.table and appends directly the label to it
  
  # check if folds is a single number
  if ((length(folds) == 1)) {
    folds <- kfold(y = label, k = folds, stratified = TRUE, seed = seed, named = FALSE)
  }
  
  # Unscramble folds if needed
  if (is.list(folds)) {
    folds_temp <- folds
    folds <- numeric(length(label))
    for (i in 1:length(folds_temp)) {
      folds[folds_temp[[i]]] <- i
    }
    rm(folds_temp)
  }
  
  if (type == "auto") {
    if (length(unique(label)) > 4) {
      type <- "anova"
    } else {
      type <- "class"
    }
  }
  
  if (length(ban) == 0) {
    formula <- reformulate(termlabels = paste0("`", colnames(data), "`"), response = "label")
  } else {
    if (is.numeric(ban)) {
      formula <- reformulate(termlabels = paste0("`", colnames(data)[-ban & (!antiban)], "`"), response = "label")
    } else {
      formula <- reformulate(termlabels = paste0("`", colnames(data)[which((!colnames(data) %in% ban) & (!antiban))], "`"), response = "label")
    }
  }
  
  set.seed(seed)
  mini_model <- rpart(formula = formula,
                      data = data,
                      method = type,
                      parms = list(split = split),
                      control = rpart.control(minsplit = min_split,
                                              minbucket = min_bucket,
                                              cp = min_improve,
                                              maxcompete = competing_splits,
                                              maxsurrogate = surrogate_search,
                                              usesurrogate = surrogate_type,
                                              xval = folds,
                                              surrogatestyle = surrogate_style,
                                              maxdepth = max_depth))
  
  if (verbose) {
    print(summary(mini_model))
  }
  
  if (plots) {
    plotcp(mini_model)
    rpart.plot(mini_model, main = "Decision Tree")
  }
  
  return(mini_model)
  
}