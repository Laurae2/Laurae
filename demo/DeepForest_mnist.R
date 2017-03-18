# Load libraries
library(Laurae)
library(data.table)
library(Matrix)
library(plyr)
library(xgboost) # COMPILED FROM SOURCE ONLY

# Set working directory accordingly
readline("\nWhere did you download MNIST data? - Download MNIST CSV data here: https://pjreddie.com/projects/mnist-in-csv/\nMake sure you have one hour before running this, as computations are very long even on an Intel i7 quad core CPU.\nPress [Enter] to continue.")
cat("Select any MNIST file in the opening window.\n")
setwd(dirname(normalizePath(file.choose(), winslash = "/")))
train <- fread("mnist_train.csv")
test <- fread("mnist_test.csv")

label_train <- train$V1
label_test <- test$V1
train$V1 <- NULL
test$V1 <- NULL

# debug xgboost, mandatory
for (i in 1:784) {
  train[[i]] <- as.numeric(train[[i]])
  test[[i]] <- as.numeric(test[[i]])
}

# SUBSAMPLE DATA - 2000 SAMPLES ONLY - COMMENT TO NOT SUBSAMPLE
command_todo <- readline("\nDo you want to subsample data to 2000 samples?\nThe accuracy we will report are based on 2000 samples, therefore you should use 2000 samples if you can.\nThis is recommended.\n[NO + Enter] for NO (NOT RECOMMENDED), [Enter] for YES (RECOMMENDED): ")
if (command_todo != "N") {
  valid <- train[2001:60000, ]
  train <- train[1:2000, ]
  label_valid <- label_train[2001:60000]
  label_train <- label_train[1:2000]
}

# Create folds - set it to larger like 5 but it gets slower
folds <- kfold(label_train, k = as.numeric(readline("\nHow many folds for cross-validation? 3 is recommended.\n[Input number + Enter]: ")))

# Do Cascade Forest using xgboost Random Forest / Complete-Tree Random Forest behind the wheels
# 0.8991+
readline("\nWe will train a Cascade Forest and get 89.91%+ Accuracy. This is going to take a while.\nModel will be stored in /CascadeForest_1 of the MNIST folder you specified.\n[Enter] to continue. \n")
cat("\nWe should be stopping at Layer 11, and get this best accuracy: Layer 6, Average Forest: 0.899100\n\n")
dir.create("CascadeForest_1")
model <- CascadeForest(training_data = train,
                       validation_data = test,
                       training_labels = label_train,
                       validation_labels = label_test,
                       folds = folds,
                       boosting = FALSE,
                       nthread = 2, # More threads if you feel so
                       cascade_lr = 1,
                       training_start = NULL,
                       validation_start = NULL,
                       cascade_forests = c(rep(4, 2), 0),
                       cascade_trees = 200, # Set this to much higher like 1000 (cf official paper)
                       cascade_rf = 2, # If you changed cascade_forests, change this value accordingly
                       objective = "multi:softprob",
                       eval_metric = Laurae::df_acc,
                       multi_class = 10,
                       early_stopping = 4, # Keep it otherwise if you make it longer it will take forever to stop
                       maximize = TRUE,
                       verbose = TRUE,
                       low_memory = FALSE,
                       essentials = TRUE,
                       garbage = TRUE,
                       work_dir = "CascadeForest_1/")

# Now compare to xgboost
readline("\nWe will now compare to xgboost which gets 90.53% Accuracy. Take note of the reported Accuracy in a notepad before going ahead.\n[Enter] to continue. \n")
dtrain <- xgb.DMatrix(data = Laurae::DT2mat(train), label = label_train)
dtest <- xgb.DMatrix(data = Laurae::DT2mat(test), label = label_test)
gc()

# [250]	train-merror:0.000000	test-merror:0.094700
# 0.905300 accuracy
cat("\nWe should be stopping at iteration 250, and get this best accuracy: [250] train-merror:0.000000 test-merror:0.094700\n\n")
gc()
set.seed(11111)
model2 <- xgb.train(params = list(nthread = 4, # More threads if you feel so
                                  eta = 0.10,
                                  max_depth = 6,
                                  booster = "gbtree",
                                  tree_method = "hist",
                                  grow_policy = "depthwise"),
                    objective = "multi:softprob",
                    num_class = 10,
                    eval_metric = "merror",
                    nrounds = 1000000,
                    early_stopping_rounds = 50,
                    data = dtrain,
                    watchlist = list(train = dtrain, test = dtest),
                    verbose = 1)


# Try with Multi-Grained Scanning for gcForest
readline("\nWe will compare soon to Deep Forest (1 of 3). Take note of the reported Accuracy in a notepad before going ahead.\nFirst, we must preprocess the data to convert the column-wise format into a matrix format.\nProgress bars will report the advancement state.\n [Enter] to continue. \n")
new_train <- plyr::alply(train, 1, function(x) {matrix(as.numeric(x), nrow = 28, ncol = 28)}, .progress = "tk")
new_test <- plyr::alply(test, 1, function(x) {matrix(as.numeric(x), nrow = 28, ncol = 28)}, .progress = "tk")

# Run Multi-Grained Scanning
readline("\nWe will compare soon to Deep Forest (2 of 3).\nSecond, we must perform multi-grained scanning on the images, similarly to Convolutions in Neural Networks.\n[Enter] to continue. \n")
dir.create("MGSCanning")
new_model <- MGScanning(data = new_train,
                        labels = label_train,
                        folds = folds,
                        dimensions = 2,
                        depth = 10, # Default official implementation
                        stride = 2, # Set this to 1 if you want maximum performance, but it is VERY SLOW + adds many features
                        nthread = 2, # More threads if you feel so
                        n_forest = 2, # Following official implementation
                        n_trees = 30, # Following official implementation
                        random_forest = 1, # Following official implementation
                        seed = 0,
                        objective = "multi:softprob",
                        eval_metric = df_acc,
                        multi_class = 10,
                        garbage = TRUE,
                        work_dir = "MGScanning/")

readline("\nWe will compare soon to Deep Forest (3 of 3).\nThird, we predict the 'convolution' and bind them together with the original data.\n[Enter] to continue. \n")

# Predict on train data
new_train2 <- MGScanning_pred(model = new_model,
                              data = new_train,
                              folds = folds,
                              dimensions = 2,
                              multi_class = 10)

# Predict on test data
new_test2 <- MGScanning_pred(model = new_model,
                             data = new_test,
                             folds = NULL,
                             dimensions = 2,
                             multi_class = 10)

# Create new datasets
new_train2 <- Laurae::DTcbind(new_train2, train)
new_test2 <- Laurae::DTcbind(new_test2, test)

# Do Deep Forest / gcForest
# 0.9146+
readline("\nWe can now train a Deep Forest model earning 91.46%+ Accuracy.\nModel will be stored in /CascadeForest_2 of the MNIST folder you specified.\n[Enter] to continue. \n")
cat("\nWe should be stopping at Layer 5, and get this best accuracy: Layer 5, Average Forest: 0.914600\n\n")
dir.create("CascadeForest_2")
model <- CascadeForest(training_data = new_train2,
                       validation_data = new_test2,
                       training_labels = label_train,
                       validation_labels = label_test,
                       folds = folds,
                       boosting = FALSE,
                       nthread = 1, # More threads if you feel so
                       cascade_lr = 1,
                       training_start = NULL,
                       validation_start = NULL,
                       cascade_forests = c(rep(4, 2), 0),
                       cascade_trees = 200, # Set this to much higher like 1000 (cf official paper)
                       cascade_rf = 2, # If you changed cascade_forests, change this value accordingly
                       objective = "multi:softprob",
                       eval_metric = Laurae::df_acc,
                       multi_class = 10,
                       early_stopping = 2,
                       maximize = TRUE,
                       verbose = TRUE,
                       low_memory = FALSE,
                       essentials = TRUE,
                       garbage = TRUE,
                       work_dir = "CascadeForest_2/")


# Try with xgboost as final booster instead of Cascade Forest
readline("\nHowever, a xgboost trained first using Multi-Grained Scanning earns 92.80%+ Accuracy.\nWe will train such model, take note of the previous accuracy before going on.\n[Enter] to continue. \n")
dtrain2 <- xgb.DMatrix(data = Laurae::DT2mat(new_train2), label = label_train)
dtest2 <- xgb.DMatrix(data = Laurae::DT2mat(new_test2), label = label_test)
gc()

# [223]	train-merror:0.000000	test-merror:0.072000 
# 0.928000 accuracy, not converged
cat("\nWe should be stopping at iteration 243, and get this best accuracy: [243] train-merror:0.000000 test-merror:0.072000\n\n")
gc()
set.seed(11111)
model2 <- xgb.train(params = list(nthread = 1, # More threads if you feel so
                                  eta = 0.10,
                                  max_depth = 6,
                                  booster = "gbtree",
                                  tree_method = "hist",
                                  grow_policy = "depthwise"),
                    objective = "multi:softprob",
                    num_class = 10,
                    eval_metric = "merror",
                    nrounds = 1000000,
                    early_stopping_rounds = 50,
                    data = dtrain2,
                    watchlist = list(train = dtrain2, test = dtest2),
                    verbose = 1)

readline("\nWe are done with this CPU intensive tutorial!\n[Enter] to exit. \n")
