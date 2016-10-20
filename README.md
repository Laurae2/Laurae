# Installing this package?
To install this package, run in R:
```
library(devtools)
install_github("Laurae2/Laurae")
```

# Laurae
Advanced Toolbox for Data Science for R by Laurae

# What you need?

If I am not missing stuff (please make a pull request if something is missing that must be added):

| Package | Requires compilation? | Which functions? |
| --- | :---: | --- |
| LightGBM | YES | lightgbm.train, lightgbm.predict, lightgbm.cv |
| xgboost | YES (?) | xgb.ncv, xgb.opt.depth |
| outliers | No | rule_single, rule_double |
| R.utils | No | rule_single, rule_double |
| data.table | YES | read_sparse_csv, lightgbm.train, lightgbm.predict, lightgbm.cv, DTcbind, DTsubsample |
| Matrix | No | read_sparse_csv |
| recommenderlab | No | read_sparse_csv (only when using NAs as sparse) |
| Rtsne | No | tsne_grid |
| tabplot | No | tableplot_jpg |
| caret | No | xgb.ncv |
| stringi | No | lightgbm.cv |

# Installing dependencies?

* For LightGBM, please use: `git clone --recursive https://github.com/wxchan/LightGBM` for the repository (as of 10/20/2016, this one has a correct early_stopping implementation.
* For xgboost, refer to my documentation for installing in MinGW: https://github.com/dmlc/xgboost/tree/master/R-package - If you encounter strange issues in Windows (like permission denied, etc.), please read: https://medium.com/@Laurae2/compiling-xgboost-in-windows-for-r-d0cb826786a5. Make sure you are using MinGW.
* data.table: to get fwrite, run in your R console `install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")`
* tabplot: please use: `install.packages("https://cran.r-project.org/src/contrib/Archive/tabplot/tabplot_0.12.tar.gz", repos=NULL, type="source")`. The 0.13 version is "junk" since they added standard deviation which makes unreadable tableplots when it is too high, even if standard deviation is disabled.

# Strange errors on first run

Sometimes you will get strange errors (like a corrupted documentation database) on the first load ever on the package. Restart R to get rid of this issue. It does not show up anymore afterwards.

# What is inside?

| Utility | Function Name(s) |
| --- | --- |
| Supervised Learning | xgboost: xgb.ncv, xgb.opt.depth <br> LightGBM: lgbm.train, lgbm.predict, lgbm.cv <br> Rules: rule_single, rule_double |
| Unsupervised Learning | t-SNE: tsne_grid |
| Visualizations | tableplots: tableplot_jpg |
| Extreme low-memory manipulation | data.table: setDF, DTcbind, DTsubsample <br> CSV sparse: read_sparse_csv |

| Function Name | Type | What is it for |
| --- | --- | --- |
| tsne_grid | Dimensionality Reduction + Grid Search | Allows to grid search a seed and a perplexity interval using t-SNE, while returning the best t-SNE model along with the best iteration found, all in a fully verbose fashion. |
| read_sparse_csv | Iterated numeric sparse matrix reading | R always imports CSV as dense. This function allows to read very large CSVs in chunks by variables (or a specific subset of variables), outputting a sparse matrix with typically lower RAM usage than a dense matrix if sparsity is high enough, all in a fully verbose fashion. Sparsity can be defined as 0 or NA, while saving as RDS is available in the loading streak. |
| tableplot_jpg | Batch tableplot output to JPEG | Allows to create a tableplot which is immediately turned into JPEG in batch per variable, against a label. It allows to preview features in a more understandable fashion than eyeballing numeric values. |
| xgb.ncv | Repeated xgboost Cross-Validation | Allows to run a repeated xgboost cross-validation with fully verbosity of aggregate summaries, computation time, and ETA of computation, with fixed seed and a sink to store xgboost verbose data, and also out-of-fold predictions and external data prediction. |
| rule_single | Outlying Univariate Continuous Association Rule Finder | Allows to use an outlying univariate continuous association rule finder on data and predicts immediately. Intermediate outlying scores can be stored. High verbosity of outputs during computation.
| rule_double | Outlying Bivariate Linear Continuous Association Rule Finder | Allows to use an outlying bivariate linear continuous association rule finder on data and predicts immediately. Intermediate outlying scores cannot be stored. If a bivariate combination is ill-conditioned (sum of correlation matrix = 4), that bivariate combination is skipped to avoid a solver matrix inversion crash/freeze/interruption when trying to compute Mahalanobis distance dimensionality reduction. High verbosity of outputs during computation. Potential TO-DO: give the user the possibility to use their own dimensionality reduction function (like a truncated PCA 1-axis). |
| xgb.opt.depth | xgboost Depth Optimizer | Allows to optimize xgboost's depth parameter using simple heuristics. The learner function is customizable to fit any other model requiring to work by integer steps. Hence, it is adaptable to work on continuous 1-D features, with a large safety net you define yourself by coercing the integer to your own range. |
| lightgbm.train | LightGBM trainer | Trains a LightGBM model. Full verbosity control, with logging to file possible. |
| lightgbm.predict | LightGBM predictor | Predicts from a LightGBM model. Use the model working directory if you lost the model variable (which is not needed to predict - you only need the correct model working directory and the model name). |
| lightgbm.cv | LightGBM CV trainer | Cross-Validates a LightGBM model. Full verbosity control, with logging to file possible, with predictions given back as return. Subsampling is optimized to maximum to lower memory usage peaks. |
| setDF | Low memory DT coercion to DF | (Already available in data.table) Coerces a data.table to data.frame using the least possible memory. Actually, it uses about 0 extra memory. |
| DTcbind | Low memory DT cbind | Column bind two data.tables using the least possible memory. With extreme settings, it uses only one column extra of memory, and the peak is reached when hitting the largest RAM intensive column (which is not much when you have 1,000+ columns). Compared to cbind, this reduce peak memory usage by 3X, and sometimes by more. |
| DTsubsample | Low memory DT subsampling | Subsample a data.table using the least possible memory. With extreme settings, it uses only the original data.table + 2 rows of memory. Compared to direct subsampling, this reduce peak memory usage by 2X, and sometimes by more. |

# TO-DO:

* Refactor LightGBM code
* Better handling of LightGBM arguments
* Better handling of LightGBM files
* Fuse Laurae2/sparsity 's SVMLight converter/reader and Laurae2/Laurae

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
