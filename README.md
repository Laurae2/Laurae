# Installing this package? (Unproper installation)

**Proper version is at the end of this page.**

If you already installed this package in the past, or you want to install this package super fast because you want the functions, run in R:

```
library(devtools)
install_github("Laurae2/Laurae")
```

Running in a Virtual Machine? Use the following alternative:

```
library(devtools)
install_git("git://github.com/Laurae2/Laurae.git")

```

# Laurae
Advanced Toolbox for Data Science for R by Laurae

# What you need?

If I am not missing stuff (please make a pull request if something is missing that must be added):

| Package | Requires compilation? | Which functions? |
| --- | :---: | --- |
| LightGBM | YES | lgbm.train, lgbm.predict, lgbm.cv, lgbm.fi, lgbm.metric, lgbm.fi.plot |
| xgboost | YES (?) | xgb.ncv, xgb.opt.depth |
| data.table | YES | read_sparse_csv, lightgbm.train, lightgbm.predict, lightgbm.cv, lgbm.fi, lgbm.fi.plot, DTcbind, DTsubsample, setDF, DTfillNA |
| outliers | No | rule_single, rule_double |
| R.utils | No | rule_single, rule_double |
| Matrix | No | read_sparse_csv |
| recommenderlab | No | read_sparse_csv (only when using NAs as sparse) |
| Rtsne | No | tsne_grid |
| tabplot | No | tableplot_jpg |
| stringi | No | lightgbm.cv |
| ggplot2 | No | lgbm.fi.plot |
| None so far | No | kfold, nkfold, lgbm.find |

# Installing dependencies?

* For LightGBM, please use: `git clone --recursive https://github.com/Microsoft/LightGBM` for the repository (as of 10/20/2016, it has now a correct early_stopping implementation)
* For xgboost, refer to my documentation for installing in MinGW: https://github.com/dmlc/xgboost/tree/master/R-package - If you encounter strange issues in Windows (like permission denied, etc.), please read: https://medium.com/@Laurae2/compiling-xgboost-in-windows-for-r-d0cb826786a5. Make sure you are using MinGW.
* data.table: to get fwrite, run in your R console `install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")`
* tabplot: please use: `install.packages("https://cran.r-project.org/src/contrib/Archive/tabplot/tabplot_0.12.tar.gz", repos=NULL, type="source")`. The 0.13 version is "junk" since they added standard deviation which makes unreadable tableplots when it is too high, even if standard deviation is disabled.

# Strange errors on first run

Sometimes you will get strange errors (like a corrupted documentation database) on the first load ever on the package. Restart R to get rid of this issue. It does not show up anymore afterwards.

# Printed text is missing after interrupting LightGBM / xgboost

Write in your R console `sink()` until you get an error.

# A lot of functions that worked are giving errors.

Write in your R console `sink()` until you get an error.

# What is inside?

| Utility | Function Name(s) |
| --- | --- |
| Supervised Learning | xgboost: xgb.ncv, xgb.opt.depth <br> LightGBM: lgbm.train, lgbm.predict, lgbm.cv, lgbm.metric, lgbm.fi, lgbm.fi.plot, lgbm.find <br> Rules: rule_single, rule_double <br> Base: kfold, nkfold |
| Unsupervised Learning | t-SNE: tsne_grid |
| Visualizations | tableplots: tableplot_jpg |
| Extreme low-memory manipulation | data.table: setDF, DTcbind, DTsubsample, DTfillNA <br> CSV sparse: read_sparse_csv |

| Function Name | Type | What is it for |
| --- | --- | --- |
| tsne_grid | Dimensionality Reduction + Grid Search | Allows to grid search a seed and a perplexity interval using t-SNE, while returning the best t-SNE model along with the best iteration found, all in a fully verbose fashion. |
| read_sparse_csv | Iterated numeric sparse matrix reading | R always imports CSV as dense. This function allows to read very large CSVs in chunks by variables (or a specific subset of variables), outputting a sparse matrix with typically lower RAM usage than a dense matrix if sparsity is high enough, all in a fully verbose fashion. Sparsity can be defined as 0 or NA, while saving as RDS is available in the loading streak. |
| tableplot_jpg | Batch tableplot output to JPEG | Allows to create a tableplot which is immediately turned into JPEG in batch per variable, against a label. It allows to preview features in a more understandable fashion than eyeballing numeric values. |
| xgb.ncv | Repeated xgboost Cross-Validation | Allows to run a repeated xgboost cross-validation with fully verbosity of aggregate summaries, computation time, and ETA of computation, with fixed seed and a sink to store xgboost verbose data, and also out-of-fold predictions and external data prediction. |
| rule_single | Outlying Univariate Continuous Association Rule Finder | Allows to use an outlying univariate continuous association rule finder on data and predicts immediately. Intermediate outlying scores can be stored. High verbosity of outputs during computation.
| rule_double | Outlying Bivariate Linear Continuous Association Rule Finder | Allows to use an outlying bivariate linear continuous association rule finder on data and predicts immediately. Intermediate outlying scores cannot be stored. If a bivariate combination is ill-conditioned (sum of correlation matrix = 4), that bivariate combination is skipped to avoid a solver matrix inversion crash/freeze/interruption when trying to compute Mahalanobis distance dimensionality reduction. High verbosity of outputs during computation. Potential TO-DO: give the user the possibility to use their own dimensionality reduction function (like a truncated PCA 1-axis). |
| xgb.opt.depth | xgboost Depth Optimizer | Allows to optimize xgboost's depth parameter using simple heuristics. The learner function is customizable to fit any other model requiring to work by integer steps. Hence, it is adaptable to work on continuous 1-D features, with a large safety net you define yourself by coercing the integer to your own range. |
| lgbm.train | LightGBM trainer | Trains a LightGBM model. Full verbosity control, with logging to file possible. Allows to predict out of the box during the training on the validation set and a test set. |
| lgbm.predict | LightGBM predictor | Predicts from a LightGBM model. Use the model working directory if you lost the model variable (which is not needed to predict - you only need the correct model working directory and the model name). |
| lgbm.cv | LightGBM CV trainer | Cross-Validates a LightGBM model, returns out of fold predictions, ensembled average test predictions (if provided a test set), and cross-validated feature importance. Full verbosity control, with logging to file possible, with predictions given back as return. Subsampling is optimized to maximum to lower memory usage peaks. |
| lgbm.fi | LightGBM Feaure Importance | Computes the feature importance (Gain, Frequence) of a LightGBM model with Sum / Relative Ratio / Absolute Ratio scales. |
| lgbm.fi.plot | LightGBM Feaure Importance Plot | Pretty plots a LightGBM feature importance table from a trained model, or from a cross-validated model. Use the model for auto-plotting. Try to use different scales to see more appropriately differences in feature importance. You can also use the multipresence parameter to cross-validate features. |
| lgbm.metric | LightGBM Training Metrics | Computes the training metrics of a logged LightGBM model and finds the best iteration. |
| lgbm.find | LightGBM Path Helper | Helps you usign a GUI to find and write the correct path for input to LightGBM functions. |
| setDF | Low memory DT coercion to DF | (Already available in data.table) Coerces a data.table to data.frame using the least possible memory. Actually, it uses about 0 extra memory. |
| DTcbind | Low memory DT cbind | Column bind two data.tables using the least possible memory. With extreme settings, it uses only one column extra of memory, and the peak is reached when hitting the largest RAM intensive column (which is not much when you have 1,000+ columns). Compared to cbind, this reduce peak memory usage by 3X, and sometimes by more. |
| DTsubsample | Low memory DT subsampling | Subsample a data.table using the least possible memory. It should not do lower memory usage than direct subsampling. Sometimes, you can get a slight efficiency of up to 5%. |
| DTfillNA | Low memory DT Missing Value filling | Fills the missing values of a data.table using the least possible memory. Compared to direct usages (DT[is.na(DT)] <- value), this function consumes up to 3X less (and typically 2X less). You can even create a new data.table or overwrite the original one. Also, this function works on data.frame, and can even overwrite the original data.frame. |
| kfold | k-fold Cross-Validation | Creates folds for cross-validation. |
| nkfold | n-repeated k-fold Cross-Validation | Creates folds for repeated cross-validation. |


# TO-DO:

* Add a super fast matrix to data.table converter
* Refactor LightGBM code
* Better handling of LightGBM arguments
* Better handling of LightGBM files
* Fuse Laurae2/sparsity 's SVMLight converter/reader and Laurae2/Laurae
* Add SVMLight format (via dgCMatrix) as input for LightGBM (actually, it supports already SVMLight, you just need to create the appropriate files beforehand)

# To add:

* xgboost grid search
* xgboost unbalanced large dataset learning
* large sparse matrix loader for categorical data
* Categorical to Numeric converter: h2o's autoencoder, mxnet's autoencoder, t-SNE, Generalized Low Rank Models, largeVis, FeatureHashing - along with testing performance using xgboost
* Logloss brute force calibration
* Prediction Analyzer (analyze any type of model predictions, currently only binary)
* Automated Feature Creator (create automatically features using linear (^1), quadratic (^2), cubic (^3), quartic (^4), mean, and standard deviation of different random features as inputs) - all with a GUI to interrupt without losing data with full verbosity of search
* Automated Feature Analyzer (analyze created features and test against randomness of improval) - all with verbosity
* Leave-one-out encoding (encodes any categorical using a continuous variable such as the label or a feature)
* AND MANY MORE...

# Extra contributors:

@fakyras for the base R code for LightGBM.

# Installing this package? (Proper installation)

If you need the modeling packages, you are going to need LightGBM and xgboost compiled. Also, xgboost requires to be installed afterwards as a R package. Using drat or CRAN version is not guaranteed to work with my package.

Linux users can skip xgboost (https://github.com/dmlc/xgboost/tree/master/R-package) and LightGBM (https://github.com/Microsoft/LightGBM/wiki/Installation-Guide) installation steps, as they are straightforward (compile source).

Windows users need MinGW (architecture x86_64) and Visual Studio 2015 Community (or any working version, starting from 2013). Prepare at least 10 GB.

## xgboost (~1 GB in Windows)

This applies to **Windows only**. Linux users can just compile "out of the box" xgboost with the gcc tool chain and install easily the package in R.

Check first if you have RTools. If not, download a proper version here: https://cran.r-project.org/bin/windows/Rtools/

Check also whether you installed Git Bash or not. If not, install Git Bash (https://git-for-windows.github.io/).

Make sure you installed MinGW (mandatory) for x86_64 architecture.

Run in R: `system('gcc -v')`

* If you don't see MinGW, then edit the PATH variable appropriately so MinGW is FIRST.
* If you see MinGW, open Git Bash and run:

```
mkdir C:/xgboost
cd C:/xgboost
git clone --recursive https://github.com/dmlc/xgboost
cd xgboost
git submodule init
git submodule update
alias make='mingw32-make'
cd dmlc-core
make
cd ../rabit
make lib/librabit_empty.a
cd ..
cp make/mingw64_min.mk config.mk
make
```

This should compile xgboost perfectly out of the box on Windows. If you get an error at the last "make", it means you are not using MinGW or you messed up something in the steps.

Now, fire up an R session and run this:

```
setwd('C:/xgboost/xgboost/R-package')
library(devtools)
install()
```

If you get a "permission denied" error, go to C:\xgboost\xgboost\R-package, right-click on the “src” folder, select “Properties”:

* Under the “Security” tab, click “Edit”
* Click “Full control” to all group or user names (click on each group, click Full control for each)
* Click OK twice
* Right-click on the “src” folder, select “Properties”
* Under the “Security” tab, click “Advanced”
* Check “Replace all child object permission entries with inheritable permission entries from this object” (it is the last box at the bottom left of the opened tab).
* Click OK twice
* Run again `install()` in the R console

And you should have now xgboost compiled in Windows.

Check quickly that xgboost works:

```
library(xgboost)
set.seed(11111)
n=100
ncov=4
z=matrix(replicate(n,rnorm(ncov)),nrow=n)
alpha=c(-1,0.5,-0.25,-0.1)
za=z%*%alpha
p=exp(za)/(1+exp(za))
t=rbinom(n,1,p)
xgb.train(list(objective="binary:logitraw"), xgb.DMatrix(data=z,label=t), nrounds=10)
```

## LightGBM installation (~10 GB in Windows)

This applies to **Windows only**. Linux users can just compile "out of the box" LightGBM with the gcc tool chain

LightGBM use Visual Studio (2013 or higher) to build in Windows. If you do not have Visual Studio, follow this: download Visual Studio 2015 Community. It is free. When installing Visual Studio Community, use the default installation method. Otherwise, you might have random errors on the UI if you try a minimal installation. Prepare at least 8GB of free drive space.

Once you are done installing Visual Studio 2015 Community, reboot your computer.

Now, or if you skipped the installation step, clone LightGBM repository by doing in Git Bash:

```
cd C:/xgboost
git clone --recursive https://github.com/Microsoft/LightGBM
```

Now the steps:

* Under C:/xgboost/LightGBM/windows, double click LightGBM.sln to open it in Visual Studio.
* Accept any warning pop up about project versioning issues (Upgrade VC++ Compiler and Libraries --> OK).
* Wait one minute for the loading.
* On the Solution Explorer, click "Solution 'LightGBM' (1 project)"
* On the bottom right tab (Properties), change the "Active config" to "Release|x64" (default is "Debug_mpi|x64")
* Compile the solution by pressing Ctrl+Shift+B (or click Build > Build Solution).
* Should everything be correct, you now have LightGBM compiled under C:\xgboost\LightGBM\windows\x64\Release

If you get an error while building (Windows SDK version blabla), then you will need the correct SDK for your OS. Start Visual Studio from scratch, click "New Project", select "Visual C++" and click "Install Visual C++ 2015 Tools for Windows Desktop". Then, attempt to build LightGBM.

If Visual Studio fails to load the "project", delete LightGBM folder and clone LightGBM repository again in Git Bash. If it still does not compile in Visual Studio, try adjusting the PATH to include the appropriate Windows SDK path. Restart Visual Studio and try compiling again.

Once you compiled it (and after you installed everything else you need, like the Laurae package), create a folder named "test" in "C:/" (or any appropriate folder you have), and try to run the following in R (you will get two prompts: the first for the "temporary" directory you created, and the second for the LightGBM executable to select):

```
setwd(choosedir(caption = "Select the temporary folder"))
library(Laurae)
library(stringi)

DT <- data.table(Split1 = c(rep(0, 50), rep(1, 50)), Split2 = rep(c(rep(0, 25), rep(0.5, 25)), 2))
DT$Split5 <- rep(c(rep(0, 5), rep(0.05, 5), rep(0, 10), rep(0.05, 5)), 4)
label <- as.numeric((DT$Split2 == 0) & (DT$Split1 == 0) & (DT$Split3 == 0) & (DT$Split4 == 0) | ((DT$Split2 == 0.5) & (DT$Split1 == 1) & (DT$Split3 == 0.25) & (DT$Split4 == 0.1) & (DT$Split5 == 0)) | ((DT$Split1 == 0) & (DT$Split2 == 0.5)))

trained <- lgbm.train(y_train = label,
                      x_train = DT,
                      bias_train = NA,
                      application = "binary",
                      num_iterations = 1,
                      early_stopping_rounds = 1,
                      learning_rate = 1,
                      num_leaves = 16,
                      min_data_in_leaf = 1,
                      min_sum_hessian_in_leaf = 1,
                      tree_learner = "serial",
                      num_threads = 1,
                      lgbm_path = lgbm.find(),
                      workingdir = getwd(),
                      validation = FALSE,
                      files_exist = FALSE,
                      verbose = TRUE,
                      is_training_metric = TRUE,
                      save_binary = TRUE,
                      metric = "binary_logloss")
```

## data.table

To make LightGBM run as fast as possible, improvements for Input/Output is necessary. For this, you will need the development version of data.table. To download it, run in your R console:

```
install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")
```

The speed up can reach over 1,000x for pure I/O.

## tabplot

To have "more readable" tableplots for visualizations, you will need to install an old version of the tabplot package. You can do this by running in your R console:

```
install.packages("https://cran.r-project.org/src/contrib/Archive/tabplot/tabplot_0.12.tar.gz", repos=NULL, type="source")
```

## Other packages

You can install the other packages by running in your R console:

```
install.packages(c("stringi", "outliers", "R.utils", "Matrix", "recommenderlab", "Rtsne", "caret"))
```

## Laurae

You can now install the Laurae package and use the fully fledged version of it.

```
library(devtools)
install_github("Laurae2/Laurae")
```

Running in a Virtual Machine? Use the following alternative:

```
library(devtools)
install_git("git://github.com/Laurae2/Laurae.git")
```

Getting a package error while running install_github/install_git which is not "could not connect to server"? Make sure you have the package outlined in the error, which is required by devtools.
