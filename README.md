# Laurae

Advanced High Performance Data Science Toolbox for R by Laurae

![What is data science](https://cloud.githubusercontent.com/assets/9083669/20948670/8a05baec-bc15-11e6-9c7a-33419038d252.png)

# What can I do with it?

Mostly... in a nutshell:

* Supervised Learning: xgboost, LightGBM, rule-based, feature engineering assistant, interactive xgb feature importance, repeated cross-validation, symbolic loss function derivation
* Unsupervised Learning: auto t-SNE
* Automated Reporting for Machine Learning: linear regression, unbiased xgboost regression/classification
* Interactive Analysis: interactive loss function symbolic derivation, interactive "I'm Feeling Lucky" ggplot
* Optimization: Cross-Entropy optimization combined with Elite optimization
* data.table improvements: up to 3X memory efficiency without even a minor cost in CPU time
* Plot massive amounts of data without being slow: tableplots tableplots tableplots tableplots tableplots
* SVMLight I/O: C++ implementation of SVMLight reading/saving for dgCMatrix (sparse column-compressed format)

**Supervised Learning:**

* Use LightGBM in R (first wrapper available in R for LightGBM) tuned for maximum I/O without using in-memory dataset moves (which is both a good and bad thing! - 10GB of data takes 4 mins of travel in a HDD) and use feature importance with smart and readable plots
* Use a repeated cross-validated xgboost (Extreme Gradient Boosting)
* Get pretty interactive feature importance tables of xgboost ready-to-use for markdown documents
* Throw supervised rules using outliers anywhere you feel it appropriate (univariate, bivariate)
* Create cross-validated and repeated cross-validated folds for supervised learning with more options for creating them (like batch creation - those ones can be fed into my LightGBM R wrapper for extensive analysis of feature behavior)
* Feature Engineering Assistant (mostly non-linear version) using automated decision trees
* Dictionary of loss functions and ready to input into xgboost (currently: Absolute Error, Squared Error, Cubic Error, Loglikelihood Error, Poisson Error, Kullback-Leibler Error)
* Symbolic Derivaton for custom loss functions (finding gradient/hessian painlessly)

**Unsupervised Learning:**

* Auto-tune t-SNE (t-Distributed Stochastic Neighbor Embedding), but it comes already with premade hyperparameters tuned for minimal reproduction loss!

**Automated Reporting for Machine Learning:**

* Generate an in-depth automated report for linear regression with interactive elements.
* Generate an in-depth automated report for xgboost regression/classification with interactive elements, with unbiased feature importance computations

**Interactive Analysis:**

* Discover and optimize gradient and hessian functions interactively in real-time
* Plot up to 1 dependent variable, 2 independent variables, 2 conditioning variables, and 1 weighting variable for Exploratory Data Analysis using ggplot, in real-time

**Optimization:**

* Do feature selection & hyperparameter optimization using Cross-Entropy optimization & Elite optimization
* Do the same optimization but with any variable (continuous, ordinal, discrete) for any function using fully personalized callbacks (which is both a great thing and a hassle for the user) and a personalized training backend (by default it uses xgboost as the predictor for next steps, you can modify it by another (un)supervised machine learning model!)
* Symbolic Derivaton for custom loss functions (finding gradient/hessian painlessly)

**Improvements & Extras:**

* Improve data.table memory efficiency by up to 3X while keeping a large part of its performance (best of both worlds? isn't that insane?)
* Improve Cross-Entropy optimization by providing a more powerful frontend (at the expense of the user's necessary knowledge) in order to converge better on feature selection & but slower on hyperparameter optimization of black boxes
* Load sparse data directly as dgCMatrix (sparse matrix)
* Plot massive amount of data in an easily readable picture

**Sparsity SVMLight converter benchmark:**

* Benchmark to convert a dgCMatrix with 2,500,000 rows and 8,500 columns (1.1GB in memory) => 5 minutes
* I think it needs minimum hours if not days for the other existing converters for such size.
* Currently not merged on this repository: see https://github.com/Laurae2/sparsity !

**Nice pictures:**

* LightGBM Feature Importance:

![LightGBM Feature Importance](https://cloud.githubusercontent.com/assets/9083669/20763173/3e7a54a2-b729-11e6-86d5-48966ce2cd92.png)

* xgboost Interactive Feature Importance:

![xgboost Interactive Feature Importance](https://cloud.githubusercontent.com/assets/9083669/20934442/6fdc1902-bbdb-11e6-8eb4-9f382e9cd97e.png)

* Automated Reporting with pretty tables:

![Automated Reporting with pretty tables](https://cloud.githubusercontent.com/assets/9083669/21076083/d23e6d94-bf22-11e6-9b40-c718b4d5691e.png)

* Interactive Symbolic Derivation:

![Interactive Symbolic Derivation](https://cloud.githubusercontent.com/assets/9083669/21138299/b050ce68-c12d-11e6-9302-c32286ad1729.png)

* Interactive EDA using ggplot:

![Interactive EDA using ggplot](https://cloud.githubusercontent.com/assets/9083669/21138494/9efcf910-c12e-11e6-9e02-0a4bacd0e957.png)

* Feature Engineering Assistant:

![Feature Engineering Assistant](https://cloud.githubusercontent.com/assets/9083669/20763260/896627ac-b729-11e6-93e4-2c4baef6f67f.png)

* Feature Engineering Assistant Pretty Print:

![Feature Engineering Assistant Pretty Print](https://cloud.githubusercontent.com/assets/9083669/20763274/9c6bf57a-b729-11e6-86ef-cd7157b81d70.png)

# Installing this package? (Unproper installation)

**Proper version is at the end of this page.**

If you already installed this package in the past, or you want to install this package super fast because you want the functions, run in R:

```r
library(devtools)
install_github("Laurae2/Laurae")
```

Running in a Virtual Machine and/or have no proxy redirection from R? Use the following alternative:

```r
library(devtools)
install_git("git://github.com/Laurae2/Laurae.git")
```

Need all R dependencies in one shot?:

```r
install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")
install.packages("https://cran.r-project.org/src/contrib/Archive/tabplot/tabplot_1.1.tar.gz", repos=NULL, type="source")
install.packages(c("rpart", "rpart.plot", "tabplot", "ggplot2", "plotluck", "grid", "gridExtra", "lattice", "car", "CEoptim", "DT", "formattable", "rmarkdown", "shiny", "shinydashboard", "Matrix", "matrixStats", "R.utils", "Rtsne", "recommenderlab", "Rcpp", "RcppArmadillo", "Deriv", "outliers", "stringi"))
install_github("Laurae2/sparsity")
```

Getting Failed with error: `'there is no package called 'sparsity''` ? Run `install_github("Laurae2/sparsity")` or `install_git("git://github.com/Laurae2/sparsity.git")` if you wish to hide this error or if you want to use the super fast column-compressed sparse matrix (dgCMatrix) -> SVMLight converter in R.

# What you need?

If I am not missing stuff (please make a pull request if something is missing that must be added):

| Package | Requires compilation? | Which functions? |
| --- | :---: | --- |
| LightGBM | YES (from PR 33) | lgbm.train, lgbm.predict, lgbm.cv, lgbm.cv.prep, lgbm.fi, lgbm.metric, lgbm.fi.plot |
| xgboost | YES (from PR 1855) | xgb.ncv, xgb.opt.depth, report.xgb |
| data.table | YES (MUST) | read_sparse_csv, lgbm.train, lgbm.predict, lgbm.cv, lgbm.cv.prep, lgbm.fi, lgbm.fi.plot, DTcbind, DTrbind, DTsubsample, setDF, DTfillNA, report.lm, report.xgb, interactive.SymbolicLoss, interactive.eda_ggplot |
| Laurae2/sparsity | YES | lgbm.train, lgbm.predict, lgbm.cv, lgbm.cv.prep |
| rpart | No | FeatureLookup |
| rpart.plot | No | FeatureLookup |
| tabplot | No | tableplot_jpg, interactive.eda_ggplot |
| ggplot2 | No | lgbm.fi.plot, report.lm, report.xgb,, interactive.eda_ggplot |
| plotluck | No | , interactive.eda_ggplot |
| grid | No | report.lm, report.xgb |
| gridExtra | No | report.lm, report.xgb |
| lattice | No | report.lm, report.xgb |
| car | No | .ExtraOpt_plot |
| CEoptim | No | ExtraOpt |
| DT | No | xgb.importance.interactive, report.lm, report.xgb |
| formattable | No | report.lm, report.xgb |
| rmarkdown | No | report.lm, report.xgb |
| shiny | No | interactive.SymbolicLoss, interactive.eda_ggplot |
| shinydashboard | No | interactive.SymbolicLoss, interactive.eda_ggplot |
| Matrix | No | read_sparse_csv |
| matrixStats | No | report.lm, report.xgb |
| R.utils | No | rule_single, rule_double, report.lm, report.xgb |
| Rtsne | No | tsne_grid |
| recommenderlab | No | read_sparse_csv (only when using NAs as sparse) |
| Rcpp | No | sparsity (package) |
| RcppArmadillo | No | report.lm |
| Deriv | No | SymbolicLoss, interactive.SymbolicLoss |
| outliers | No | rule_single, rule_double |
| stringi | No | lightgbm.cv |
| None so far | No | kfold, nkfold, lgbm.find |

LightGBM PR 33: https://github.com/Microsoft/LightGBM/tree/9895116d9e71a91b6722ca7ef1139c946fb608bf

# Installing dependencies?

* For LightGBM (use PR 33 please), please do NOT use: `git clone --recursive https://github.com/Microsoft/LightGBM` for the repository. Use my stable version which is aligned with Laurae package via `git clone --recursive https://github.com/Laurae2/LightGBM`. Then follow the installation steps (https://github.com/Microsoft/LightGBM/wiki/Installation-Guide).
* For xgboost, refer to my documentation for installing in MinGW: https://github.com/dmlc/xgboost/tree/master/R-package - If you encounter strange issues in Windows (like permission denied, etc.), please read: https://medium.com/@Laurae2/compiling-xgboost-in-windows-for-r-d0cb826786a5. Make sure you are using MinGW.
* data.table: to get fwrite, run in your R console `install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")`
* sparsity: You must use Laurae's sparsity package (SVMLight I/O conversion) which can be found here: https://github.com/Laurae2/sparsity/blob/master/README.md - compilation simply requires writing `devtools:::install_github("Laurae2/sparsity")` (and having Rtools in Windows).
* tabplot: please use: `install.packages("https://cran.r-project.org/src/contrib/Archive/tabplot/tabplot_1.1.tar.gz", repos=NULL, type="source")`. The 1.3 version is "junk" since they added standard deviation which makes unreadable tableplots when it is too high, even if standard deviation is disabled.

# Strange errors on first run

Sometimes you will get strange errors (like a corrupted documentation database) on the first load ever on the package. Restart R to get rid of this issue. It does not show up anymore afterwards.

# Printed text is missing after interrupting LightGBM / xgboost

Write in your R console `sink()` until you get an error.

# A lot of functions that worked are giving errors.

Write in your R console `sink()` until you get an error.

# What is inside?

| Utility | Function Name(s) |
| --- | --- |
| Supervised Learning | xgboost: xgb.ncv, xgb.opt.depth, xgb.importance.interactive <br> LightGBM: lgbm.train, lgbm.predict, lgbm.cv, lgbm.metric, lgbm.fi, lgbm.fi.plot, lgbm.find <br> Rules: rule_single, rule_double <br> Base: kfold, nkfold <br> Helpers: SymbolicLoss, FeatureLookup, ExtraOpt |
| Unsupervised Learning | t-SNE: tsne_grid |
| Automated Reporting | report.lm, report.xgb |
| Interactive Analysis | interactive.SymbolicLoss, interactive.eda_ggplot |
| Visualizations | tableplots: tableplot_jpg |
| Extreme low-memory manipulation | data.table: setDF, DTcbind, DTrbind, DTsubsample, DTfillNA <br> CSV sparse: read_sparse_csv |

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
| lgbm.cv.prep | LightGBM CV preparation helper | Prepares the data for using lgbm.cv. All required data files are output, so you can run lgbm.cv with files_exist = TRUE without the need of other data preparation (which can be long sometimes). Supports SVMLight format. |
| lgbm.fi | LightGBM Feaure Importance | Computes the feature importance (Gain, Frequence) of a LightGBM model with Sum / Relative Ratio / Absolute Ratio scales. |
| lgbm.fi.plot | LightGBM Feaure Importance Plot | Pretty plots a LightGBM feature importance table from a trained model, or from a cross-validated model. Use the model for auto-plotting. Try to use different scales to see more appropriately differences in feature importance. You can also use the multipresence parameter to cross-validate features. |
| lgbm.metric | LightGBM Training Metrics | Computes the training metrics of a logged LightGBM model and finds the best iteration. |
| lgbm.find | LightGBM Path Helper | Helps you usign a GUI to find and write the correct path for input to LightGBM functions. |
| setDF | Low memory DT coercion to DF | (Already available in data.table) Coerces a data.table to data.frame using the least possible memory. Actually, it uses about 0 extra memory. |
| DTcbind | Low memory DT cbind | Column bind two data.tables using the least possible memory. With extreme settings, it uses only one column extra of memory, and the peak is reached when hitting the largest RAM intensive column (which is not much when you have 1,000+ columns). Compared to cbind, this reduce peak memory usage by 3X, and sometimes by more. |
| DTrbind | Low memory DT rbind | Row bind two data.tables using the least possible memory. With extreme settings, it uses only one column extra of memory, and the peak is reached when hitting the largest RAM intensive column (which is not much when you have 1,000+ columns). Compared to rbind, this reduce peak memory usage by 3X, and sometimes by more. |
| DTsubsample | Low memory DT subsampling | Subsample a data.table using the least possible memory. It should not do lower memory usage than direct subsampling. Sometimes, you can get a slight efficiency of up to 5%. |
| DTfillNA | Low memory DT Missing Value filling | Fills the missing values of a data.table using the least possible memory. Compared to direct usages (DT[is.na(DT)] <- value), this function consumes up to 3X less (and typically 2X less). You can even create a new data.table or overwrite the original one. Also, this function works on data.frame, and can even overwrite the original data.frame. |
| kfold | k-fold Cross-Validation | Creates folds for cross-validation. |
| nkfold | n-repeated k-fold Cross-Validation | Creates folds for repeated cross-validation. |
| ExtraOpt | Cross-Entropy -based Hybrid Optimization | Combines Cross-Entropy optimization and Elite optimization in order to optimize mixed types of variable (continuous, ordinal, discrete). The frontend is fully featured and requires the usage of callbacks in order to be usable. Example callbacks are provided. A demo trainer, a demo estimator, a demo predictor, and a demo plotter are provided as reference callbacks to customize. The optimization backend is fully customizable, allowing you to switch the optimizer (default is xgboost) to any other (un)supervised machine learning model! |
| FeatureLookup | Non-linear Feature Engineering Assistant | Allows to run a cross-validated decision tree using your own specified depth, amount of surrogates, and best potential lookups in order to to create new features based on the resulting decision tree at your own will. |
| SymbolicLoss | Symbolic Derivation of Loss Functions | Attemps to compute the exact 1st and 2nd derivatives of the loss function provided, along of a reference function if you provide one. The functions returned are ready to be used. Graphics are also added to help the user. |
| xgb.importance.interactive | Interactive xgboost Feature Importance | Allows to print an interactive xgboost feature importance table, ready to be used in markdown documents and HTML documents to be shared. |
| report.lm | Automated HTML Reporting for Linear Regression | Automatically creates a report for linear regression (C++ backend). Allows data normalization, NA cleaning, rank deficiency checking, pretty printed machine learning performance statistics (R, R^2, MAE, MSE, RMSE, MAPE), pretty printed feature multiplicative coefficients, plotting statistics, analysis of variance (ANOVA), adjusted R^2, degrees of freedom computation... |
| report.xgb | Automated HTML Reporting for Linear Regression | Automatically creates a report for linear regression (C++ backend). Allows data normalization, NA cleaning, rank deficiency checking, pretty printed machine learning performance statistics (R, R^2, MAE, MSE, RMSE, MAPE, AUC, Logloss, optimistic Kappa, optimistic F1 Score, optimistic MCC, optimistic TPR, optimistic TNR, optimistic FPR, optimistic FNR), pretty printed feature (unbiased/biased) importance, plotting statistics, plotting of machine learning performance statistic evolution vs probability... |
| interactive.SymbolicLoss | Interactive Dashboard for Derivation of Loss Functions | Creates an interactive dashboard which allows you to work on up to 4 loss functions with their gradient and hessian, which are typically used in numerical optimization tasks. Resists to errors (keeps running even when you input errors). |
| interactive.eda_ggplot | Interactive Dashforboard for Exploratory Data Analysis using ggplot2 | Creates an interactive dashboard which allows you to work on the data set you want (from the global environment) by plotting up to 3 variables simultaneously, using a smart detection of variables to choose the best appropriate plot. Resists to errors (keeps running even when you input errors). |

# TO-DO:

* Add a super fast matrix to data.table converter
* Refactor LightGBM code
* Better handling of LightGBM arguments
* Better handling of LightGBM files
* Fuse Laurae2/sparsity 's SVMLight converter/reader and Laurae2/Laurae
* Add Differential Evolution algorithm for feature selection and hyperparameter simultaneous optimization (add another backend via another interface as it typically takes a lot of time for both)
* (Attempt to) Add automated non-linear feature creation using decision trees

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

```bash
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

```r
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

```r
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

LightGBM use Visual Studio (2013 or higher) to build in Windows. If you do not have Visual Studio, follow this: download Visual Studio 2015 Community. It is free. When installing Visual Studio Community, use the default installation method. Otherwise, you might have random errors on the UI if you try a minimal installation. Prepare at least 8GB of free drive space. Install it with the Visual C++ additions (custom install, select the first box which has 3 subboxes - it should say you will install the Windows SDK blablabla - ignore the update failure error at the end).

Once you are done installing Visual Studio 2015 Community, reboot your computer.

Now, or if you skipped the installation step, clone the latest (CLEARLY UNRECOMMENDED) LightGBM repository by doing in Git Bash:

```r
cd C:/xgboost
git clone --recursive https://github.com/Microsoft/LightGBM
```

If you want the stable (RECOMMENDED) version aligned to Laurae package, use `git clone --recursive https://github.com/Laurae2/LightGBM` instead. You have 99%+ guarantee to have a non-working version if you use the fully bleeding edge devel version of LightGBM with this package (well, most of the things work but it is refusing to train on data most of the times, even via direct command line).

Now the steps:

* Under C:/xgboost/LightGBM/windows, double click LightGBM.sln to open it in Visual Studio.
* Accept any warning pop up about project versioning issues (Upgrade VC++ Compiler and Libraries --> OK).
* Wait one minute for the loading.
* On the Solution Explorer, click "Solution 'LightGBM' (1 project)"
* On the bottom right tab (Properties), change the "Active config" to "Release|x64" (default is "Debug_mpi|x64")
* Compile the solution by pressing Ctrl+Shift+B (or click Build > Build Solution).
* Should everything be correct, you now have LightGBM compiled under C:\xgboost\LightGBM\windows\x64\Release

If you get an error while building (Windows SDK version blabla), then you will need the correct SDK for your OS. Start Visual Studio from scratch, click "New Project", select "Visual C++" and click "Install Visual C++ 2015 Tools for Windows Desktop". Then, attempt to build LightGBM.

If Visual Studio fails to load the "project", delete LightGBM folder and clone LightGBM repository again in Git Bash. If it still does not compile in Visual Studio, try adjusting the PATH to include the appropriate Windows SDK path. Restart Visual Studio and try compiling again. Another way: uninstall Visual Studio (using the installer), reboot, and reinstall using Custom install (and select all Visual C++ things, it must be the first box with 3 subboxes to check - which will tell you it will install the SDK etc.). Then, you should be able to compile it perfectly.

Once you compiled it (and after you installed everything else you need, like the Laurae package), create a folder named "test" in "C:/" (or any appropriate folder you have), and try to run the following in R (you will get two prompts: the first for the "temporary" directory you created, and the second for the LightGBM executable to select):

```r
# Make sure you have data.table in case
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

```r
install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")
```

The speed up can reach over 1,000x for pure I/O.

## tabplot

To have "more readable" tableplots for visualizations, you will need to install an old version of the tabplot package. You can do this by running in your R console:

```r
install.packages("https://cran.r-project.org/src/contrib/Archive/tabplot/tabplot_1.1.tar.gz", repos=NULL, type="source")
```

## Other packages

You can install the other packages by running in your R console:

```r
install.packages(c("rpart", "rpart.plot", "tabplot", "ggplot2", "plotluck", "grid", "gridExtra", "lattice", "car", "CEoptim", "DT", "formattable", "rmarkdown", "shiny", "shinydashboard", "Matrix", "matrixStats", "R.utils", "Rtsne", "recommenderlab", "Rcpp", "RcppArmadillo", "Deriv", "outliers", "stringi"))
install_github("Laurae2/sparsity")
```

## Laurae

You can now install the Laurae package and use the fully fledged version of it.

```r
library(devtools)
install_github("Laurae2/Laurae")
```

Running in a Virtual Machine and/or have no proxy redirection from R? Use the following alternative:

```r
library(devtools)
install_git("git://github.com/Laurae2/Laurae.git")
```

Getting a package error while running install_github/install_git which is not "could not connect to server"? Make sure you have the package outlined in the error, which is required by devtools.
