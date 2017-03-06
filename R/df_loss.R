#' Pearson Coefficient of Correlation (R) (computation function, any size)
#' 
#' This function computes the Pearson Coefficient of Correlation loss (R) provided \code{preds} and \code{labels}, while handling multiclass problems (mean).
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Pearson Coefficient of Correlation.
#' 
#' @examples
#' library(data.table)
#' 
#' # Regression problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- runif(n = 9*6*10)
#' df_r(my_preds, my_labels)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_r(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_r(my_preds, my_labels)
#' 
#' @export

df_r <- function(preds, labels) {
  if (!is.null(dim(preds))) {
    x = cor(x = preds, y = labels, method = "pearson")
    return(mean(x))
  } else {
    x <- cor(x = preds, y = labels, method = "pearson")
    return(x)
  }
}

#' Coefficient of Determination (R^2) (computation function, any size)
#' 
#' This function computes the Coefficient of Determination loss (R^2) provided \code{preds} and \code{labels}, while handling multiclass problems (mean).
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Coefficient of Determination.
#' 
#' @examples
#' library(data.table)
#' 
#' # Regression problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- runif(n = 9*6*10)
#' df_r2(my_preds, my_labels)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_r2(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_r2(my_preds, my_labels)
#' 
#' @export

df_r2 <- function(preds, labels) {
  if (!is.null(dim(preds))) {
    x = cor(x = preds, y = labels, method = "pearson")
    return(mean(x * x))
  } else {
    x <- cor(x = preds, y = labels, method = "pearson")
    return(x * x)
  }
}

#' Spearman Coefficient of Correlation (R) (computation function, any size)
#' 
#' This function computes the Spearman Coefficient of Correlation loss (R) provided \code{preds} and \code{labels}, while handling multiclass problems (mean).
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Spearman Coefficient of Correlation.
#' 
#' @examples
#' library(data.table)
#' 
#' # Regression problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- runif(n = 9*6*10)
#' df_spearman(my_preds, my_labels)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_spearman(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_spearman(my_preds, my_labels)
#' 
#' @export

df_spearman <- function(preds, labels) {
  if (!is.null(dim(preds))) {
    x = cor(x = preds, y = labels, method = "spearman")
    return(mean(x))
  } else {
    x <- cor(x = preds, y = labels, method = "spearman")
    return(x)
  }
}

#' Mean Squared Error (MSE) (computation function, any size)
#' 
#' This function computes the Mean Squared Error loss (MSE) provided \code{preds} and \code{labels}, while handling multiclass problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Mean Squared Error.
#' 
#' @examples
#' library(data.table)
#' 
#' # Regression problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- runif(n = 9*6*10)
#' df_mse(my_preds, my_labels)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_mse(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_mse(my_preds, my_labels)
#' 
#' @export

df_mse <- function(preds, labels) {
  if (!is.null(dim(preds))) {
    x <- preds - model.matrix(~.+0, data.table(as.factor(labels)))
    return(mean(rowSums(x * x)))
  } else {
    # fastest version, but overflow risk with large values: crossprod(preds - labels) / length(labels)
    x <- preds - labels
    return(mean(x * x))
  }
}

#' Root Mean Squared Error (RMSE) (computation function, any size)
#' 
#' This function computes the Root Mean Squared Error loss (MSE) provided \code{preds} and \code{labels}, while handling multiclass problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Root Mean Squared Error.
#' 
#' @examples
#' library(data.table)
#' 
#' # Regression problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- runif(n = 9*6*10)
#' df_rmse(my_preds, my_labels)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_rmse(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_rmse(my_preds, my_labels)
#' 
#' @export

df_rmse <- function(preds, labels) {
  if (!is.null(dim(preds))) {
    x <- preds - model.matrix(~.+0, data.table(as.factor(labels)))
    return(sqrt(mean(rowSums(x * x))))
  } else {
    x <- preds - labels
    return(sqrt(mean(x * x)))
  }
}

#' Mean Absolute Error (MAE) (computation function, any size)
#' 
#' This function computes the Mean Absolute Error loss (MAE) provided \code{preds} and \code{labels}, while handling multiclass problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Mean Absolute Error.
#' 
#' @examples
#' library(data.table)
#' 
#' # Regression problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- runif(n = 9*6*10)
#' df_mae(my_preds, my_labels)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_mae(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_mae(my_preds, my_labels)
#' 
#' @export

df_mae <- function(preds, labels) {
  if (!is.null(dim(preds))) {
    x <- preds - model.matrix(~.+0, data.table(as.factor(labels)))
    return(mean(abs(rowSums(x))))
  } else {
    x <- preds - labels
    return(mean(abs(x)))
  }
}

#' Mean Absolute Percentage Error (MAPE) (computation function, single vector)
#' 
#' This function computes the Mean Absolute Percentage Error loss (MAPE) provided \code{preds} and \code{labels}, but cannot handle multiclass problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Mean Absolute Percentage Error.
#' 
#' @examples
#' library(data.table)
#' 
#' # Regression problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- runif(n = 9*6*10)
#' df_mape(my_preds, my_labels)
#' 
#' @export

df_mape <- function(preds, labels) {
  z <- (labels == 0)
  return(mean(abs((labels[z] - preds[z]) / labels[z])))
}

#' Median Absolute Error (MedAE) (computation function, single vector)
#' 
#' This function computes the Median Absolute Error loss (MedAE) provided \code{preds} and \code{labels}, but cannot handle multiclass problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Median Absolute Error.
#' 
#' @examples
#' library(data.table)
#' 
#' # Regression problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- runif(n = 9*6*10)
#' df_medae(my_preds, my_labels)
#' 
#' @export

df_medae <- function(preds, labels) {
  return(median(abs(preds - labels)))
}

#' Median Absolute Percentage Error (MedPAE) (computation function, single vector)
#' 
#' This function computes the Median Absolute Percentage Error loss (MedPAE) provided \code{preds} and \code{labels}, but cannot handle multiclass problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Median Absolute Percentage Error.
#' 
#' @examples
#' library(data.table)
#' 
#' # Regression problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- runif(n = 9*6*10)
#' df_medpae(my_preds, my_labels)
#' 
#' @export

df_medpae <- function(preds, labels) {
  z <- (labels == 0)
  return(median(abs((preds[z] - labels[z]) / labels[z])))
}

#' Mean Cubic Error (MCE) (computation function, any size)
#' 
#' This function computes the Mean Cubic Error loss (MCE) provided \code{preds} and \code{labels}, while handling multiclass problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Mean Cubic Error.
#' 
#' @examples
#' library(data.table)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_mce(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_mce(my_preds, my_labels)
#' 
#' @export

df_mce <- function(preds, labels) {
  if (!is.null(dim(preds))) {
    x <- preds - model.matrix(~.+0, data.table(as.factor(labels)))
    return(mean(abs(rowSums(x * x * x))))
  } else {
    x <- preds - labels
    return(mean(abs(x * x * x)))
  }
}

#' Logarithmic Loss (Logloss) (computation function, any size)
#' 
#' This function computes the Logarithmic Loss (Logloss) provided \code{preds} and \code{labels}, while handling multiclass problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' @param eps The threshold for predictions so you do not have infinite loss issued from an observation. Defaults to \code{1e-15}.
#' 
#' @return The Logarithmic Loss.
#' 
#' @examples
#' library(data.table)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_logloss(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_logloss(my_preds, my_labels)
#' 
#' @export

df_logloss <- function(preds, labels, eps = 1e-15) {
  if (!is.null(dim(preds))) {
    x <- c(t(preds))
    z <- (0:(length(labels) -1)) * ncol(preds) + labels + 1
    return(mean(df_logloss(x[z], rep(1, length(labels)), eps)))
  } else {
    x <- preds
    x[x < eps] <- eps
    x[x > (1 - eps)] <- 1 - eps
    return(-mean(labels * log(x) + (1 - labels) * log(1 - x)))
  }
}

#' Accuracy loss (Acc) (computation function, any size)
#' 
#' This function computes the Accuracy loss (Acc) provided \code{preds} and \code{labels}, while handling multiclass problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' @param threshold The \code{threshold} for binary classification, if using binary classification. Defaults to \code{0.5}.
#' 
#' @return The Accuracy.
#' 
#' @examples
#' library(data.table)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_acc(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_acc(my_preds, my_labels)
#' 
#' @export

df_acc <- function(preds, labels, threshold = 0.5) {
  if (!is.null(dim(preds))) {
    if (is.matrix(preds)) {
      x <- preds
    } else {
      x <- as.matrix(preds)
    }
    x <- apply(x, 1, which.max) - 1
    return(sum(x == labels) / length(labels))
  } else {
    return((sum(labels[preds >= threshold] == 1) + sum(labels[preds < threshold] == 0)) / length(labels))
  }
}

#' Accuracy loss (Acc) (computation function, binary optimization)
#' 
#' This function computes the Accuracy loss (Acc) provided \code{preds} and \code{labels}, while optimizing for binary problems.
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The optimized Accuracy.
#' 
#' @examples
#' library(data.table)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_acc_bin(my_preds, my_labels)
#' 
#' @export

df_acc_bin <- function(preds, labels) {
  return(Laurae::get.max_acc(preds, labels)[1])
}

#' Area Under the Curve Loss (AUC) (computation function, any size)
#' 
#' This function computes the Area Under the Curve Loss (AUC) provided \code{preds} and \code{labels}, while handling multiclass problems (mean).
#' 
#' @param preds The \code{predictions}.
#' @param labels The \code{labels}.
#' 
#' @return The Area Under the Curve Loss.
#' 
#' @examples
#' library(data.table)
#' 
#' # Binary classification problem
#' my_preds <- dnorm(rnorm(n = 9*6*10))
#' my_labels <- as.numeric(runif(n = 9*6*10) >= 0.5)
#' df_auc(my_preds, my_labels)
#' 
#' # Multiclass classification problem
#' my_preds <- data.table(matrix(dnorm(rnorm(n = 9*6*10)), nrow = 9*10))
#' my_labels <- rep(c(1, 2, 3, 4, 5, 1, 0, 2, 3), 10)
#' df_auc(my_preds, my_labels)
#' 
#' @export

df_auc <- function(preds, labels) {
  if (!is.null(dim(preds))) {
    temp_auc <- 0
    if (is.data.frame(preds)) {
      for (i in 1:ncol(preds)) {
        x <- preds[[i]]
        temp_auc <- Laurae::FastROC(x, as.numeric(labels == (i - 1))) + temp_auc
      }
    } else {
      for (i in 1:ncol(preds)) {
        x <- preds[, i]
        temp_auc <- Laurae::FastROC(x, as.numeric(labels == (i - 1))) + temp_auc
      }
    }
    return(temp_auc / ncol(preds))
  } else {
    return(Laurae::FastROC(preds, labels))
  }
}
