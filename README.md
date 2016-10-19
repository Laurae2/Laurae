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

| Package | Which functions? |
| --- | --- |
| xgboost (0.60) | xgb.ncv, xgb.opt.depth |
| outliers | rule_single, rule_double |
| R.utils | rule_single, rule_double |
| data.table | read_sparse_csv |
| Matrix | read_sparse_csv |
| recommenderlab | read_sparse_csv (only when using NAs as sparse) |
| Rtsne | tsne_grid |
| tabplot | tableplot_jpg |
| caret | xgb.ncv |
| LightGBM (to compile) | lightgbm.train, lightgbm.predict, lightgbm.cv, lightgbm.predict.cv |

# What is inside?

| Function Name | Type | What is it for |
| --- | --- | --- |
| tsne_grid | Dimensionality Reduction + Grid Search | Allows to grid search a seed and a perplexity interval using t-SNE, while returning the best t-SNE model along with the best iteration found, all in a fully verbose fashion. |
| read_sparse_csv | Iterated numeric sparse matrix reading | R always imports CSV as dense. This function allows to read very large CSVs in chunks by variables (or a specific subset of variables), outputting a sparse matrix with typically lower RAM usage than a dense matrix if sparsity is high enough, all in a fully verbose fashion. Sparsity can be defined as 0 or NA, while saving as RDS is available in the loading streak. |
| tableplot_jpg | Batch tableplot output to JPEG | Allows to create a tableplot which is immediately turned into JPEG in batch per variable, against a label. It allows to preview features in a more understandable fashion than eyeballing numeric values. |
| xgb.ncv | Repeated xgboost Cross-Validation | Allows to run a repeated xgboost cross-validation with fully verbosity of aggregate summaries, computation time, and ETA of computation, with fixed seed and a sink to store xgboost verbose data, and also out-of-fold predictions and external data prediction. |
| rule_single | Outlying Univariate Continuous Association Rule Finder | Allows to use an outlying univariate continuous association rule finder on data and predicts immediately. Intermediate outlying scores can be stored. High verbosity of outputs during computation.
| rule_double | Outlying Bivariate Linear Continuous Association Rule Finder | Allows to use an outlying bivariate linear continuous association rule finder on data and predicts immediately. Intermediate outlying scores cannot be stored. If a bivariate combination is ill-conditioned (sum of correlation matrix = 4), that bivariate combination is skipped to avoid a solver matrix inversion crash/freeze/interruption when trying to compute Mahalanobis distance dimensionality reduction. High verbosity of outputs during computation. Potential TO-DO: give the user the possibility to use their own dimensionality reduction function (like a truncated PCA 1-axis). |
| xgb.opt.depth | xgboost Depth Optimizer | Allows to optimize xgboost's depth parameter using simple heuristics. The learner function is customizable to fit any other model requiring to work by integer steps. Hence, it is adaptable to work on continuous 1-D features, with a large safety net you define yourself by coercing the integer to your own range. |
| lightgbm.train | LightGBM trainer | Trains a LightGBM model. |
| lightgbm.predict | LightGBM predictor | Predicts from a LightGBM model. |
| lightgbm.cv | LightGBM CV trainer | Cross-Validates a LightGBM model. |
| lightgbm.cv.predict | LightGBM CV predictor | Predicts from a Cross-Validated LightGBM model. |

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
