#' Partial Dependency, plotting function
#'
#' This function is a helper to help plotting partial dependency plots using a provided grid via the specified backend.
#' 
#' For selecting the plotting backend, it depends on what you are trying to plot (from what you are working from). If you are using single observations (\code{partial_dep.obs}), you are provided the following backends:
#' 
#' \describe{
#'   \item{"tableplot"}{Tableplot is the best when it comes to plotting any type of large data. It is the default and the most appropriate for 99\% of cases, even if you have million of data points it will be blazing fast. Use this unless you have a rationale reason to use something else.}
#'   \item{"car"}{Car is the best when it comes to analyzing in depth the output in a scatter plot matrix, but it is extremely slow. Not recommended for more than 10k observations and 5 columns.}
#'   \item{"lattice"}{Lattice is used with parallel plots. Not recommended for more than 50k observations and 5 columns.}
#'   \item{"ggplot"}{ggplot is used for scatter plot matrix and correlation measurement. Not recommended for more than 20k observations and 5 columns.}
#'   \item{"plotly"}{Combines ggplot with Plotly for interactive graphics. Not recommended for more than 1k observations and 5 columns.}
#'   \item{"base"}{Base ships with R and thus is simple, but is slow. Not recommended for more than 50k observations and 5 columns.}
#' }
#' 
#' If you are using multiple observations (\code{partial_dep.obs_all}), you are provided the following backends:
#' 
#' \describe{
#'   \item{c("tableplot")}{Tableplot is used to output plot, but it is clearly not the recommended way of doing it. It is the default in case you have too many points.}
#'   \item{c("ggplot", "boxplot")}{ggplot is used to draw boxplots to check for distribution in boxplots, grouped by feature.}
#'   \item{c("ggplot", "point")}{ggplot is used to draw points and check for evolution, grouped both by feature and evolution.}
#'   \item{c("ggplot", "line")}{ggplot is used to draw points and lines and check for evolution, grouped both by feature and evolution.}
#'   \item{c("ggplot", "line2")}{ggplot is used to draw points and lines and check for evolution, grouped both by feature and evolution, interactively, protected against "2 point only" error.}
#'   \item{c("plotly", "boxplot")}{plotly + ggplot is used to draw boxplots to check for distribution in boxplots, grouped by feature, interactively.}
#'   \item{c("plotly", "point")}{plotly + ggplot is used to draw points and check for evolution, grouped both by feature and evolution, interactively.}
#'   \item{c("plotly", "line")}{plotly + ggplot is used to draw points and line and check for evolution, grouped both by feature and evolution, interactively.}
#'   \item{c("plotly", "line2")}{plotly + ggplot is used to draw points and line and check for evolution, grouped both by feature and evolution, interactively, protected against "2 point only" error.}
#' }
#' 
#' @param grid_data Type: data.table. A \code{partial_dep} \code{grid_exp} output.
#' @param backend Type: logical. What type of backend for plotting to use. Check details for detailed description. Defaults to \code{tableplot}.
#' @param label_name Type: character. The label column name in \code{grid_data} when using \code{tableplot} or \code{lattice}.
#' @param comparator_name Type: character. The comparator column name in \code{grid_data} when using \code{tableplot} or \code{lattice}.
#' @param ... other arguments to pass to to the plotting backend function.
#' 
#' @return A plot with the requested backend.
#' 
#' @examples
#' \dontrun{
#' # Train you supervised machine learning model
#' # ...
#' 
#' # Prepare partial dependence content
#' my_grid <- partial_dep.obs(...)
#' 
#' # Plot partial dependence content
#' partial_dep.plot(my_grid$grid_exp, backend = "tableplot")
#' }
#'
#' 
#' @export

partial_dep.plot <- function(grid_data, backend = "tableplot", label_name = "Target", comparator_name = "Evolution", ...) {
  
  # Check whether we have the name "Feature" in the variable (coming from partial_dep.obs_all)
  if (colnames(grid_data)[1] == "Feature") {
    
    # We are plotting many observations
    if (backend[1] == "tableplot") {
      eval(parse(text = paste0("tableplot(grid_data, sortCol = ", which(colnames(grid_data) == label_name), ", ...)"))) # no other workaround for dynamic arguments
    } else if (backend[1] == "ggplot") {
      
      if (backend[2] == "boxplot") {
        ggplot(data = grid_data, mapping = aes_string(x = "Feature", y = label_name, fill = "Feature")) + geom_boxplot() + theme_bw()
      } else if (backend[2] == "point") {
        ggplot(data = grid_data, mapping = aes_string(x = label_name, y = "Value", color = "Feature")) + geom_point() + facet_grid(as.formula(paste0("Feature ~ ", comparator_name)), scales = "free") + theme_bw()
      } else if (backend[2] == "line") {
        ggplot(data = grid_data, mapping = aes_string(x = label_name, y = "Value", color = "Feature")) + geom_point() + stat_smooth_func(geom = "text", method = "lm", hjust = 0, parse = TRUE) + stat_smooth(method = "lm") + facet_grid(as.formula(paste0("Feature ~ ", comparator_name)), scales = "free") + theme_bw()
      } else if (backend[2] == "line2") {
        ggplot(data = grid_data, mapping = aes_string(x = label_name, y = "Value", color = "Feature")) + geom_point() + stat_smooth(method = "glm") + facet_grid(as.formula(paste0("Feature ~ ", comparator_name)), scales = "free") + theme_bw()
      } else {
        warning(paste0("Unknown backend provided: ", backend))
      }
      
    } else if (backend[1] == "plotly") {
      
      if (backend[2] == "boxplot") {
        ggplotly(ggplot(data = grid_data, mapping = aes_string(x = "Feature", y = label_name, fill = "Feature")) + geom_boxplot() + theme_bw())
      } else if (backend[2] == "point") {
        ggplotly(ggplot(data = grid_data, mapping = aes_string(x = label_name, y = "Value", color = "Feature")) + geom_point() + facet_grid(as.formula(paste0("Feature ~ ", comparator_name)), scales = "free") + theme_bw())
      } else if (backend[2] == "line") {
        ggplotly(ggplot(data = grid_data, mapping = aes_string(x = label_name, y = "Value", color = "Feature")) + geom_point() + stat_smooth_func.plotly(geom = "text", method = "lm", hjust = 0, parse = TRUE) + stat_smooth(method = "lm") + facet_grid(as.formula(paste0("Feature ~ ", comparator_name)), scales = "free") + theme_bw())
      } else if (backend[2] == "line2") {
        ggplotly(ggplot(data = grid_data, mapping = aes_string(x = label_name, y = "Value", color = "Feature")) + geom_point() + stat_smooth(method = "glm") + facet_grid(as.formula(paste0("Feature ~ ", comparator_name)), scales = "free") + theme_bw())
      } else {
        warning(paste0("Unknown backend provided: ", backend))
      }
      
    } else {
      warning(paste0("Unknown backend provided: ", backend))
    }
    
  } else {
    
    if (backend == "tableplot") {
      eval(parse(text = paste0("tableplot(grid_data, sortCol = ", which(colnames(grid_data) == label_name), ", ...)"))) # no other workaround for dynamic arguments
    } else if (backend == "car") {
      car::scatterplotMatrix(grid_data, ...)
    } else if (backend == "lattice") {
      lattice::parallelplot(grid_data, ...)
    } else if (backend == "ggplot") {
      GGally::ggpairs(grid_data, mapping = ggplot2::aes_string(color = label_name, group = comparator_name), upper = list(continuous = "blank", combo = "blank", discrete = "blank", na = "blank"), lower = list(continuous = "smooth", combo = "facethist", discrete = "facetbar", na = "na"), diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"), ...) + ggplot2::theme_bw()
    } else if (backend == "plotly") {
      plotly::ggplotly(GGally::ggpairs(grid_data, mapping = ggplot2::aes_string(color = label_name, group = comparator_name), upper = list(continuous = "blank", combo = "blank", discrete = "blank", na = "blank"), lower = list(continuous = "smooth", combo = "facethist", discrete = "facetbar", na = "na"), diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"), ...) + ggplot2::theme_bw())
    } else if (backend == "base") {
      plot(grid_data, ...)
    } else {
      warning(paste0("Unknown backend provided: ", backend))
    }
    
  }
  
}