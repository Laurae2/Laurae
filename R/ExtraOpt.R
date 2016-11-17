#' Cross-Entropy -based Hybrid Optimization
#'
#' This function allows to optimize for any input value: continuous, ordinal, discrete/categorical. Simplex-constrained-type optimization is not yet implemented (mutlivariate constraints which are not univariate constraints are not yet implemented). It tries to keep the discrete distribution, and as such, can be used to reduce dimensionality of supervised machine learning model (feature selection) while optimizing the performance. To get an overview of how to structure your functions to use (you need 3!!), check \code{.ExtraOpt_trainer}, \code{.ExtraOpt_estimate}, and \code{.ExtraOpt_prob}. For plotting, check \code{.ExtraOpt_plot} for an example.
#' 
#' @param f_train Type: function. The training function which returns at the end the loss. All arguments provided to \code{ExtraOpt} in \code{...} are provided to \code{f_train}. Defaults to \code{.ExtraOpt_trainer}, which is a sample xgboost trainer.
#' @param ... Type: any. Arguments to pass to \code{f_train}.
#' @param f_est Type: function. The estimator supervised machine learning function for the variables to optimize. It must return a list with \code{Model} as the model to use for \code{f_prob}, and the \code{Error} as the loss of the estimator model. Defaults to \code{.ExtraOpt_estimate}, which is a sample xgboost variable estimator.
#' @param f_prob Type: function. The predictor function for the supervised machine learning function. It takes the model from \code{f_est} and a prior vector as inputs, and returns the predicted loss from \code{f_est}. Defaults to \code{.ExtraOpt_prob}, which is a sample xgboost estimator prediction.
#' @param preInit Type: boolean. Whether a prior list is already computed to be used instead of the initiailzation. Set Ninit accordingly if you use a pre-initialized priors matrix. Defaults to \code{NULL}.
#' @param Ninit Type: integer. The initialization amount. It is best to use at least 2 times the number of initialization vs the number of variables. For instance, 50 features should require \code{Ninit = 100}, even if it does not guarantee a best result.
#' @param Nmax Type: integer. The maximum number of iterations alloted to optimize the variables provided against the loss. Once this amount of iterations is reached (excluding error code iterations), the function stops. Defaults to \code{200}.
#' @param Nimprove Type: integer. The maximum number of iterations alloted to optimize without improvements. Defaults to \code{10}.
#' @param elites Type: numeric. The percentage of iteration samples retained in the parameter estimator. The larger the \code{elites}, the lower the ability to get stuck at a local optima. However, a very low elite amount would get quickly stuck at a local optima and potentially overfit. After the initialization, a minimum of 5 sampled elites is mandatory. For instance, if \code{Ninit = 100}, then \code{elites >= 0.05}. It should do be higher than \code{1}. If the sampling results in a decimal-valued numeric, it will take the largest value. If the sampling results in a lower than \code{5} numeric, it will shrink back to \code{5}. Defaults to \code{0.90}.
#' @param max_elites Type: numeric. The maximum allowed number of elite samples. Setting this value low increases the convergence speed, at the expense of exploration. It is not recommended to increase it over \code{5000} as it will slow down severely the next prior optimization. When elites have the same loss, the elite which was computed the earliest takes precedence over all others identical-loss elites (even if their parameters are different). Defaults to \code{150}.
#' @param tested_elites Type: numeric. The number of elites tested at the same time when trying to find new values. A high value increases the space exploration at the expense of convergence speed. Minimum of \code{1} for small steps but fast convergence speed, supposing the initialization with good enough. Defaults to \code{5}.
#' @param elites_converge Type: numeric. The number of elites to use to assess convergence via \code{cThr} and \code{dThr}. The larger the \code{elites_converge}, the tighter the convergence requirements. It cannot be higher than the number of \code{tested_elites}. Defaults to \code{10}.
#' @param maximize Type: boolean. Whether to maximize or not to maximize (minimize). Defaults to \code{TRUE}.
#' @param best Type: numeric. The best value you can get from the loss you will accept to interrupt early the optimizer. Defaults to \code{NULL}.
#' @param cMean Type: numeric vector. The mean of continuous variables to feed to \code{f_train}.
#' @param cSD Type: numeric vector. The standard deviation of continuous variables to feed to \code{f_train}.
#' @param cOrdinal Type: boolean vector. Whether each continuous variable is ordinal or not.
#' @param cMin Type: numeric vector. The minimum of each continuous variable.
#' @param cMax Type: numeric vector. The maximum of each continuous variable.
#' @param cThr Type: numeric. The value at which if the maximum standard deviation of continuous variables of the elites is higher than \code{cThr}, the continuous variables are supposed having converged. Once converged, the algorithm will have only one try to generate a higher threshold while optimizing. If it fails, convergence interrupts the optimization. Applies also to the cross-entropy internal optimization. Defaults to \code{0.001}, which means the continuous variables will be supposed converged once there is no more maximum standard deviation of 0.001.
#' @param dProb Type: list of numeric vectors. A list containing for each discrete variable, a vector with the probability of the \code{i-1}-th element to appear.
#' @param dThr Type: numeric. The value at which if the probability of the worst occurring discrete value in discrete variables of the elites is higher or equal to \code{dThr}, the discrete variables are supposed having converged. Once converged, the algorithm will have only one try to generate a higher threshold while optimizing. If it fails, convergence interrupts the optimization. Applies also to the cross-entropy internal optimization, but as \code{1 - dThr}. Defaults to \code{1}, which means the discrete variables will be supposed converged once all discrete variables have the same probability of 1.
#' @param priorsC Type: matrix. The matrix of continuous priors. Even when filled, \code{cMean} and \code{cSD} are mandatory to be filled.
#' @param priorsD Type: matrix. The matrix of discrete priors. Even when filled, \code{dProb} is mandatory to be filled.
#' @param errorCode Type: vector of 2 numerics. When f_train is ill-conditioned or has an "error", you can use an error code to replace it by a dummy value which will be parsed afterwards for removal. You must adapt it to your own error code. For instance, the error code returned by \code{f_train} should be the \code{errorCode} value when no features are selected for training a supervised model. The error codes are removed from the priors. Defaults to \code{-9999}.
#' @param autoExpVar Type: boolean. Whether the local priors must be exported to the global environment. This is extremely useful for debugging, but also to catch the \code{priorsC} and \code{priorsD} matrices when \code{ExtraOpt}, \code{f_train}, \code{f_est}, or \code{f_prob} is error-ing without a possible recovery. You would then be able to feed the priors and re-run without having to run again the algorithm from scratch. Defaults to \code{FALSE}. The saved variable in the global environment is called "temporary_Laurae".
#' @param autoExpFile Type: character. Whether the local priors must be exported to as RDS files. This is extremely useful for debugging, but also to catch the \code{priorsC} and \code{priorsD} matrices when \code{ExtraOpt}, \code{f_train}, \code{f_est}, or \code{f_prob} is error-ing without a possible recovery. You would then be able to feed the priors and re-run without having to run again the algorithm from scratch. Defaults to \code{NULL}.
#' @param plot Type: function. Whether to call a function to plot data or not. Your plotting function should take as first argument \code{"priors"}, which as a matrix with as first column the \code{Loss}, followed then by continuous variables, and ends with discrete variables. Continuous variables start with \code{"C"} while discrete variables start with \code{"D"} in the column names. Defaults to \code{NULL}.
#' @param verbose Type: integer. Should \code{ExtraOpt} become chatty and report a lot? A value of \code{0} defines silent, while \code{1} chats a little bit (and \code{2} chats a lot). \code{3} is so chatty it will flood severely. Defaults to \code{1}.
#' @param debug Type: boolean. Whether an interactive console should be used to run line by line for debugging purposes. Defaults to \code{FALSE}.
#' 
#' @return A list with \code{best} for the best value found, \code{variables} for the variable values (split into \code{continuous} list and \code{discrete} list), \code{priors} for the list of iterations and their values, \code{elite_priors} for the laste elites used, \code{new_priors} for the last iterations issued from the elites, \code{iterations} for the number of iterations, and \code{thresh_stats} for the threshold statistics over batches.
#' 
#' @examples
#' \dontrun{
#' # Example of params:
#' - 50 random initializations
#' - 200 maximum tries
#' - 3 continuous variables in [0, 10]
#' --- with 2 continuous and 1 ordinal
#' --- with respective means (2, 4, 6)
#' --- and standard deviation (1, 2, 3)
#' - and 2 discrete features
#' - with respective prior probabilities {(0.8, 0.2), (0.7, 0.1, 0.2)}
#' - and loss error code (illegal priors) of -9999
#' 
#' ExtraOpt(Ninit = 50,
#'          nthreads = 1,
#'          eta = 0.1,
#'          early_stop = 10,
#'          X_train,
#'          X_test,
#'          Y_train,
#'          Y_test,
#'          Nmax = 200,
#'          cMean = c(2, 4, 6),
#'          cSD = c(1, 2, 3),
#'          cOrdinal = c(FALSE, FALSE, TRUE),
#'          cMin = c(0, 0, 0),
#'          cMax = c(10, 10, 10),
#'          dProb = list(v1 = c(0.8, 0.2), v2 = c(0.7, 0.1, 0.2)),
#'          priorsC = NULL,
#'          priorsD = NULL,
#'          autoExp = FALSE,
#'          errorCode = -9999)
#' }
#' 
#' @export

ExtraOpt <- function(f_train = .ExtraOpt_trainer, ..., f_est = .ExtraOpt_estimate, f_prob = .ExtraOpt_prob, preInit = NULL, Ninit = 50L, Nmax = 200, Nimprove = 10, elites = 0.90, max_elites = 150, tested_elites = 5, elites_converge = 10, maximize = TRUE, best = NULL, cMean = NULL, cSD = NULL, cOrdinal = NULL, cMin = NULL, cMax = NULL, cThr = 0.001, dProb = NULL, dThr = 0.999, priorsC = NULL, priorsD = NULL, errorCode = -9999, autoExpVar = FALSE, autoExpFile = NULL, verbose = 1, plot = NULL, debug = FALSE) {
  
  # Pass CRAN tests
  temporary_Laurae <- NULL
  
  if (debug) {browser()}
  
  # Initialization of numeric data: create N*length frame
  if (length(cMean) > 0) {
    
    # Check whether a prior continuous matrix was provided
    if (length(priorsC) == 0) {
      
      # Create matrix manually
      cLength <- length(cMean)
      continuous <- matrix(nrow = Ninit, ncol = cLength)
      
      # Generate Mean + Standard Deviation
      for (i in 1:length(cMean)) {
        continuous[, i] <- rnorm(Ninit, mean = cMean[i], sd = cSD[i])
      }
      
    } else {
      
      # Use premade matrix
      continuous <- as.matrix(priorsC)
      cLength <- ncol(continuous)
      
    }
    
    # Fix ordinality
    if (sum(cOrdinal) > 0) {
      for (i in which(cOrdinal)) {
        continuous[, i] <- round(continuous[, i])
      }
    }
    
    # Fix Minimum
    if (length(cMin) > 0) {
      for (i in 1:cLength) {
        continuous[continuous[, i] < cMin[i], i] <- cMin[i]
      }
    }
    
    # Fix Maximum
    if (length(cMax) > 0) {
      for (i in 1:cLength) {
        continuous[continuous[, i] > cMax[i], i] <- cMax[i]
      }
    }
    
    # Create simplex constraints
    if ((length(cMin) > 0) & (length(cMax) > 0)) {
      # Has Min/Max
      linCombos <- rbind(diag(cLength), -diag(cLength))
      linVects <- c(cMax, cMin)
      constrained <- TRUE
    } else if (length(cMin) > 0) {
      # Hax Min only
      linCombos <- diag(-cLength)
      linVects <- cMin
      constrained <- TRUE
    } else if (length(cMax) > 0) {
      # Has Max only
      linCombos <- diag(cLength)
      linVects <- cMax
      constrained <- TRUE
    }
    
  } else {
    
    # empty continuous
    cLength <- 0
    
  }
  
  if (verbose >= 2) {cat("[", format(Sys.time(), "%X"), "] Passed continuous tests.  \n", sep = "")}
  
  # Initialization of discrete data: create N*length frame
  if (length(dProb) > 0) {
    
    # Check whether a prior discrete matrix was provided
    if (length(priorsD) == 0) {
      
      # Create matrix manually
      dLength <- length(dProb)
      discrete <- matrix(nrow = Ninit, ncol = dLength)
      
      for (i in 1:dLength) {
        
        # Attempts indefinitely to create a frame with all possible discrete values, with a warning if unmatchable probabilities
        prob_vect <- dProb[[i]][dProb[[i]] > 0] # get positive probabilities
        extend <- length(prob_vect) # the required number of unique values
        
        # Warns user for a 0 probability
        if (extend < length(dProb[[i]])) {
          cat("Discrete variable ", i, " has unmatchable probability. Ignoring that unmatchable probability, keeping others.  \n", sep = "")
        }
        
        # Loop to create probabilities
        while (extend > length(unique(discrete[, i]))) {
          discrete[, i] <- (sample.int(n = extend, size = Ninit, replace = TRUE, prob = prob_vect) - 1)
        }

      }
      
    } else {
      
      # Use premade matrix
      discrete <- as.matrix(priorsD)
      dLength <- ncol(discrete)
      
    }
    
  } else {
    
    # empty discrete
    dLength <- 0
    
  }
  
  if (verbose >= 2) {cat("[", format(Sys.time(), "%X"), "] Passed discrete tests.  \n", sep = "")}
  
  # Prepare prior matrix
  priors <- data.frame(matrix(nrow = Ninit, ncol = (1 + cLength + dLength)))
  
  # Set names
  if ((cLength > 0) & (dLength > 0)) {
    # Do continuous + discrete case
    colnames(priors) <- c("Loss", paste("C", 1:cLength, sep = ""), paste("D", 1:dLength, sep = ""))
  } else if (cLength > 0) {
    # Do continuous case
    colnames(priors) <- c("Loss", paste("C", 1:cLength, sep = ""))
  } else if (dLength > 0) {
    # Do discrete case
    colnames(priors) <- c("Loss", paste("D", 1:dLength, sep = ""))
  } else {
    stop("No continuous & discrete values were provided.")
  }
  
  if (verbose >= 2) {cat("[", format(Sys.time(), "%X"), "] Passed priors tests.  \n", sep = "")}
  
  # Create prior initialization values
  if (length(preInit) == 0) {
    for (i in 1:Ninit) {
      
      if ((cLength > 0) & (dLength > 0)) {
        # Do continuous + discrete case
        x <- continuous[i, ]
        y <- discrete[i ,]
        d <- f_train(x = x, y = y, ...)
        priors[i, ] <- c(d, x, y)
      } else if (cLength > 0) {
        # Do continuous case
        x <- continuous[i, ]
        d <- f_train(x = x, ...)
        priors[i, ] <- c(d, x)
      } else if (dLength > 0) {
        # Do discrete case
        y <- discrete[i ,]
        d <- f_train(y = y, ...)
        priors[i, ] <- c(d, y)
      }
      
      if (verbose == 2) {cat("[", format(Sys.time(), "%X"), "] Iteration ", i, " loss = ", d, "  \n", sep = "")}
      if (verbose >= 3) {cat("[", format(Sys.time(), "%X"), "] Iteration ", i, " loss = ", d," - ", paste(colnames(priors), "=", priors[i, ], collapse = ", ", sep = ""), "  \n", sep = "")}
      
      if (autoExpVar) {temporary_Laurae <<- priors[1:i, ]}
      if (length(autoExpFile) > 0) {saveRDS(priors[1:i, ], autoExpFile, compress = TRUE)}
      
    }
  } else {
    priors <- preInit
    if (verbose == 2) {cat("[", format(Sys.time(), "%X"), "] Loaded pre-initialized priors matrix.  \n", sep = "")}
  }
  
  
  if (verbose >= 2) {cat("[", format(Sys.time(), "%X"), "] --- Computed all priors tests.  \n", sep = "")}
  
  # Clean priors from error code
  priors <- priors[!(priors[, 1] == errorCode), ]
  
  if (verbose >= 2) {cat("[", format(Sys.time(), "%X"), "] --- Cleaned all priors tests.  \n", sep = "")}
  
  # Prepare to go into loop
  converged <- FALSE
  no_imp <- 0
  new_best <- max(priors[, 1])
  last_best <- new_best
  elites_C <- numeric(0)
  elites_D <- numeric(0)
  elites_loss <- numeric(0)
  
  while((nrow(priors) < Nmax) & (converged == FALSE) & (no_imp <= Nimprove)) {
    
    if (verbose >= 1) {cat("[", format(Sys.time(), "%X"), "] Iteration ", nrow(priors), " best = ", ifelse(maximize, max(priors[, 1]), min(priors[, 1])), "  \n", sep = "")}
    
    # Get order of priors
    ranking <- order(priors[, 1], decreasing = maximize)
    
    # Set elite priors
    ranking <- ranking[1:max(5, min(max_elites, length(ranking) * elites))]
    priors_elite <- priors[ranking, ]
    
    # Attempt to get Threshold of standard deviation of continuous variables, and probability threshold of discrete variables
    elites_Csd <- 0
    elites_Dsd <- 0
    elites_mean <- numeric(0)
    elites_sd <- numeric(0)
    elites_tabulated <- list()
    
    if ((cLength > 0) & (dLength > 0)) {
      
      # Do continuous + discrete case
      for (i in 2:(1 + cLength)) {
        elites_mean[(i - 1)] <- mean(priors_elite[, i])
        elites_sd[(i - 1)] <- sd(priors_elite[, i])
        elites_sd[(i - 1)] <- ifelse(is.na(elites_sd[(i - 1)]), 0, elites_sd[(i - 1)])
        elites_Csd <- max(elites_Csd, elites_sd[(i - 1)])
        elites_sd[(i - 1)] <- ifelse(elites_sd[(i - 1)] == 0, cSD[i - 1], elites_sd[(i - 1)]) # attempt to create noise to unconverge
      }
      for (i in (2 + cLength):ncol(priors_elite)) {
        elites_tabulated[[(i - cLength - 1)]] <- tabulate(priors_elite[, i] + 1, nbins = length(dProb[[(i - cLength - 1)]]))
        elites_tabulated[[(i - cLength - 1)]] <- elites_tabulated[[(i - cLength - 1)]] / sum(elites_tabulated[[(i - cLength - 1)]])
        elites_Dsd <- max(elites_Dsd, max(elites_tabulated[[(i - cLength - 1)]]))
      }
      converged <- (elites_Csd < cThr) & (elites_Dsd >= dThr)
      elites_C <- c(elites_C, elites_Csd)
      elites_D <- c(elites_D, elites_Dsd)
      
    } else if (cLength > 0) {
      
      # Do continuous case
      for (i in 2:ncol(priors_elite)) {
        elites_mean[(i - 1)] <- mean(priors_elite[, i])
        elites_sd[(i - 1)] <- sd(priors_elite[, i])
        elites_sd[(i - 1)] <- ifelse(is.na(elites_sd[(i - 1)]), 0, elites_sd[(i - 1)])
        elites_Csd <- max(elites_Csd, elites_sd[(i - 1)])
        elites_sd[(i - 1)] <- ifelse(elites_sd[(i - 1)] == 0, cSD[i - 1], elites_sd[(i - 1)]) # attempt to create noise to unconverge
      }
      converged <- (elites_Csd < cThr)
      elites_C <- c(elites_C, elites_Csd)
      
    } else if (dLength > 0) {
      
      # Do discrete case
      for (i in 2:ncol(priors_elite)) {
        elites_tabulated[[(i - 1)]] <- tabulate(priors_elite[, i] + 1, nbins = length(dProb[[(i - 1)]]))
        elites_tabulated[[(i - 1)]] <- elites_tabulated[[(i - 1)]] / sum(elites_tabulated[[(i - 1)]])
        elites_Dsd <- max(elites_Dsd, max(elites_tabulated[[(i - 1)]]))
      }
      converged <- (elites_Dsd >= dThr)
      elites_D <- c(elites_D, elites_Dsd)
      
    }
    
    priors_elite <- priors[ranking[1:min(length(ranking), elites_converge)], ]
    elites_loss <- c(elites_loss, ifelse(maximize, max(priors[, 1]), min(priors[, 1])))
    
    if (verbose >= 1) {cat("[", format(Sys.time(), "%X"), "] Iteration ", nrow(priors), " loss=", max(priors[, 1]), ifelse(cLength > 0, paste(", cThr=", elites_Csd, sep = ""), ""), ifelse(dLength > 0, paste(", dThr=", elites_Dsd, sep = ""), ""), "  \n", sep = "")}
    if (verbose >= 3) {cat("[", format(Sys.time(), "%X"), "] Elites = ", length(ranking), ". Range = [", min(priors_elite[, 1]), ", ", max(priors_elite[, 1]), "]  \n", sep = "")}
    
    # Estimate priors using a model
    modeling <- f_est(priors_elite)
    if (verbose >= 3) {cat("[", format(Sys.time(), "%X"), "] Elite reconstructed loss = ", modeling$Error, "  \n", sep = "")}
    
    # Attempt to work on new priors
    new_priors <- matrix(nrow = tested_elites, ncol = ncol(priors))
    colnames(new_priors) <- colnames(priors)
    continuous <- matrix(nrow = tested_elites, ncol = cLength)
    discrete <- matrix(nrow = tested_elites, ncol = dLength)
    
    if ((cLength > 0) & (dLength > 0)) {
      
      # Do continuous + discrete case
      
      if (constrained) {
        
        # Constrained version
        for (i in 1:tested_elites) {
          set.seed(i)
          optimized <- CEoptim(f_prob,
                               f.arg = list(model = modeling$Model),
                               maximize = maximize,
                               continuous = list(mean = elites_mean,
                                                 sd = elites_sd,
                                                 sdThr = cThr,
                                                 conMat = linCombos,
                                                 conVec = linVects),
                               discrete = list(probs = elites_tabulated, probThr = (1 - dThr)),
                               N = ncol(new_priors - 1) * 10,
                               rho = 0.1,
                               iterThr = 100,
                               noImproveThr = 5,
                               verbose = FALSE)
          new_priors[i, ] <- c(optimized$optimum, optimized$optimizer$continuous, optimized$optimizer$discrete)
          continuous[i, ] <- optimized$optimizer$continuous
          discrete[i, ] <- optimized$optimizer$discrete
        }
        
      } else {
        
        # Unconstrained version
        for (i in 1:tested_elites) {
          set.seed(i)
          optimized <- CEoptim(f_prob,
                               f.arg = list(model = modeling$Model),
                               maximize = maximize,
                               continuous = list(mean = elites_mean,
                                                 sd = elites_sd,
                                                 sdThr = cThr),
                               discrete = list(probs = elites_tabulated, probThr = (1 - dThr)),
                               N = ncol(new_priors - 1) * 10,
                               rho = 0.1,
                               iterThr = 100,
                               noImproveThr = 5,
                               verbose = FALSE)
          new_priors[i, ] <- c(optimized$optimum, optimized$optimizer$continuous, optimized$optimizer$discrete)
          continuous[i, ] <- optimized$optimizer$continuous
          discrete[i, ] <- optimized$optimizer$discrete
        }
        
      }
      
    } else if (cLength > 0) {
      
      # Do continuous case
      
      if (constrained) {
        
        # Constrained version
        for (i in 1:tested_elites) {
          set.seed(i)
          optimized <- CEoptim(f_prob,
                               f.arg = list(model = modeling$Model),
                               maximize = maximize,
                               continuous = list(mean = elites_mean,
                                                 sd = elites_sd,
                                                 sdThr = cThr,
                                                 conMat = linCombos,
                                                 conVec = linVects),
                               N = ncol(new_priors - 1) * 10,
                               rho = 0.1,
                               iterThr = 100,
                               noImproveThr = 5,
                               verbose = FALSE)
          new_priors[i, ] <- c(optimized$optimum, optimized$optimizer$continuous)
          continuous[i, ] <- optimized$optimizer$continuous
        }
        
      } else {
        
        # Unconstrained version
        for (i in 1:tested_elites) {
          set.seed(i)
          optimized <- CEoptim(f_prob,
                               f.arg = list(model = modeling$Model),
                               maximize = maximize,
                               continuous = list(mean = elites_mean,
                                                 sd = elites_sd,
                                                 sdThr = cThr),
                               N = ncol(new_priors - 1) * 10,
                               rho = 0.1,
                               iterThr = 100,
                               noImproveThr = 5,
                               verbose = FALSE)
          new_priors[i, ] <- c(optimized$optimum, optimized$optimizer$continuous)
          continuous[i, ] <- optimized$optimizer$continuous
        }
        
      }
      
    } else if (dLength > 0) {
      
      # Do discrete case
      for (i in 1:tested_elites) {
        set.seed(i)
        optimized <- CEoptim(f_prob,
                             f.arg = list(model = modeling$Model),
                             maximize = maximize,
                             discrete = list(probs = elites_tabulated, probThr = (1 - dThr)),
                             N = ncol(new_priors - 1) * 10,
                             rho = 0.1,
                             iterThr = 100,
                             noImproveThr = 5,
                             verbose = FALSE)
        new_priors[i, ] <- c(optimized$optimum, optimized$optimizer$discrete)
        discrete[i, ] <- optimized$optimizer$discrete
      }
      
    }
    
    if (verbose >= 2) {cat("[", format(Sys.time(), "%X"), "] Elite maximum expectation: ", ifelse(maximize, max(new_priors[, 1]), min(new_priors[, 1])), "  \n", sep = "")}
    
    # Attempt to compute new priors
    
    for (i in 1:tested_elites) {
      
      if ((cLength > 0) & (dLength > 0)) {
        # Do continuous + discrete case
        x <- continuous[i, ]
        y <- discrete[i ,]
        d <- f_train(x = x, y = y, ...)
        new_priors[i, ] <- c(d, x, y)
      } else if (cLength > 0) {
        # Do continuous case
        x <- continuous[i, ]
        d <- f_train(x = x, ...)
        new_priors[i, ] <- c(d, x)
      } else if (dLength > 0) {
        # Do discrete case
        y <- discrete[i ,]
        d <- f_train(y = y, ...)
        new_priors[i, ] <- c(d, y)
      }
      
      if (verbose == 2) {cat("[", format(Sys.time(), "%X"), "] Iteration ", nrow(priors) + i, " loss = ", d, "  \n", sep = "")}
      if (verbose >= 3) {cat("[", format(Sys.time(), "%X"), "] Iteration ", nrow(priors) + i, " loss = ", d, " - ", paste(colnames(new_priors), "=", new_priors[i, ], collapse = ", ", sep = ""), "  \n", sep = "")}
      
      if (autoExpVar) {temporary_Laurae <<- priors}
      if (length(autoExpFile) > 0) {saveRDS(priors, autoExpFile, compress = TRUE)}
      
    }
    
    # Bind new priors to the original priors
    priors <- rbind(priors, new_priors)
    last_best <- max(priors[, 1])
    
    # Update convergence
    if (last_best == new_best) {
      no_imp <- no_imp + 1
    } else {
      no_imp <- 0
      new_best <- last_best
    }
    
    if (length(best) > 0) {
      converged <- (ifelse(maximize, (last_best < best), (last_best > best)))
    }
    
    # Do the user wants to plot something?
    if (length(plot) > 0) {
      try(plot(priors), silent = TRUE)
    }
    
    # Go back to the beginning...
    
  }
  
  # We must re-compute the elite priors to return them
  
  # Get order of priors
  ranking <- order(priors[, 1], decreasing = maximize)
  
  # Set elite priors
  ranking <- ranking[1:max(5, min(max_elites, length(ranking) * elites))]
  priors_elite <- priors[ranking, ]
  
  # Get best parameters & standard deviation thresholds
  variables <- list()
  thresh_stats <- data.frame(matrix(nrow = length(elites_loss), ncol = 0))
  thresh_stats$Batch <- 1:length(elites_loss)
  
  if ((cLength > 0) & (dLength > 0)) {
    # Do continuous + discrete case
    variables[["Continuous"]] <- priors[which.max(priors[, 1]), 2:(1 + cLength)]
    variables[["Discrete"]] <- priors[which.max(priors[, 1]), (2 + cLength):(ncol(priors))]
    thresh_stats$Cthr <- elites_C
    thresh_stats$Dthr <- elites_D
  } else if (cLength > 0) {
    # Do continuous case
    variables[["Continuous"]] <- priors[which.max(priors[, 1]), 2:ncol(priors)]
    thresh_stats$Cthr <- elites_C
  } else if (dLength > 0) {
    # Do discrete case
    variables[["Discrete"]] <- priors[which.max(priors[, 1]), 2:ncol(priors)]
    thresh_stats$Dthr <- elites_D
  }
  
  # We are at the end! Returning all values.
  return(list(best = ifelse(maximize, max(priors[, 1]), min(priors[, 1])),
              variables = variables,
              priors = priors,
              elite_priors = priors_elite,
              last_priors = new_priors,
              iterations = nrow(priors),
              thresh_stats = thresh_stats))
  
}
