#' Interactive Dashboard for Exploratory Data Analysis (Plotly)
#'
#' This function runs an interactive dashboard which allows to explore data via 3djs and Plotly. Unlike its counterpart \code{interactive.eda_ggplot}, using 3djs is not efficient for large data (10K+ observations) and automated smart plotting is not available. You should use this function over \code{interactive.eda_3djs}, which performs poorly compared to its plotly counterpart. When specific variables are mandatory, the interactive dashboard will warn you with a star preceding the element which is mandatory to be used.
#' 
#' Plotting is done using \code{plotly}, which must be loaded before running this function. You must import data as \code{data.table} if you want maximum performance, although you will probably not notice any difference unless you are playing with millions of rows. For kernel estimation, you need the \code{MASS} package. There are issues if you open this in Internet Explorer 9 or under.
#' 
#' The colors (header: \code{f_back}) allowed are the following: 
#' 
#' \describe{
#'   \item{blue}{blue color}
#'   \item{black}{black color}
#'   \item{purple}{purple color}
#'   \item{green}{green color}
#'   \item{red}{red color}
#'   \item{yellow}{yellow color}
#' }
#' 
#' @param data Type: name reference to a data.table (preferred) or data.frame. The data you want to explore. Using a data.table increases the processing speed.
#' @param type Type: character. The type of plot to preselect (but it does nothing itself). You may choose between \code{"scatter"}, \code{"bar"}, \code{"pie"}, \code{"histogram"}, \code{"histogram2d"}, \code{"box"}, \code{"contour"}, \code{"heatmap"}, \code{"polar"}, \code{"scatter3d"}, and \code{"surface"}. Defaults to \code{"scatter"}.
#' @param plot_width Type: numeric. The width for the plot. Defaults to \code{1500}, which fits nicely Full HD screens (1920 vertical pixels).
#' @param plot_height Type: numeric. The height for the plot. Defaults to \code{820}, which fits nicely Full HD screens (1080 vertical pixels).
#' @param f_back Type: character. A background color character for the header. Defaults to \code{"red"}.
#' @param side_width Type: numeric. The width of the sidebar containing variable names. Defaults to \code{300}.
#' 
#' @return Nothing
#' 
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(MASS)
#' library(plotly)
#' library(datasets)
#' hair_eye <- as.data.frame(HairEyeColor)
#' interactive.eda_plotly(data = hair_eye,
#'                        type = "scatter",
#'                        plot_width = 600,
#'                        plot_height = 420,
#'                        f_back = "red",
#'                        side_width = 300)
#' }
#' 
#' @export

interactive.eda_plotly <- function(data,
                                   type = "scatter",
                                   plot_width = 1500,
                                   plot_height = 820,
                                   f_back = "red",
                                   side_width = 300) {
  
  if (interactive()) {
    
    ui <- dashboardPage(
      skin = f_back,
      header = dashboardHeader(title = "Laurae's Data Explorer (Plotly) Dashboard", titleWidth = 500),
      sidebar = dashboardSidebar(
        sidebarMenu(
          actionButton("run_me", "Create 3djs (Plotly) Plot", icon("refresh")),
          selectInput("data", "Data (Global Environment):", choices = ls(envir = .GlobalEnv)[sapply(ls(.GlobalEnv), function(x) class(get(x))[length(class(get(x)))]) == 'data.frame'], selected = data),
          selectInput("plot_var", "Plot Type:", choices = c("scatter", "bar", "pie", "histogram", "histogram2d", "box", "contour", "heatmap", "polar", "scatter3d", "surface", "wtf"), selected = type),
          conditionalPanel(condition = "input.plot_var == 'scatter' || input.plot_var == 'bar' || input.plot_var == 'pie' || input.plot_var == 'histogram' || input.plot_var == 'histogram2d' || input.plot_var == 'box' || input.plot_var == 'contour' || input.plot_var == 'heatmap' || input.plot_var == 'scatter3d' || input.plot_var == 'surface'",
                           selectInput("dep_var", "Dependent Variable:", choices = colnames(data), selected = colnames(data)[1])),
          conditionalPanel(condition = "input.plot_var == 'scatter' || input.plot_var == 'histogram' || input.plot_var == 'histogram2d' || input.plot_var == 'box' || input.plot_var == 'contour' || input.plot_var == 'heatmap' || input.plot_var == 'scatter3d' || input.plot_var == 'surface'",
                           selectInput("indep_var", "Independent Variable:", choices = c("None", colnames(data)[-1]), selected = "None")),
          conditionalPanel(condition = "input.plot_var == 'bar' || input.plot_var == 'pie' || input.plot_var == 'histogram2d' || input.plot_var == 'contour' || input.plot_var == 'heatmap' || input.plot_var == 'scatter3d' || input.plot_var == 'surface'",
                           selectInput("freq_var", "Value Variable:", choices = c("None", colnames(data)[-1]), selected = "None")),
          conditionalPanel(condition = "input.plot_var == 'polar'",
                           selectInput("radius_var", "Radius Variable:", choices = c("None", colnames(data)), selected = "None")),
          conditionalPanel(condition = "input.plot_var == 'polar'",
                           selectInput("angle_var", "Angular Variable:", choices = c("None", colnames(data)), selected = "None")),
          conditionalPanel(condition = "input.plot_var == 'scatter' || input.plot_var == 'bar' || input.plot_var == 'histogram' || input.plot_var == 'box' || input.plot_var == 'polar' || input.plot_var == 'scatter3d'",
                           selectInput("color_var", "Color Variable:", choices = c("None", colnames(data)[-1]), selected = "None")),
          conditionalPanel(condition = "input.plot_var == 'scatter' || input.plot_var == 'polar' || input.plot_var == 'scatter3d'",
                           selectInput("size_var", "Size Variable:", choices = c("None", colnames(data)[-1]), selected = "None")),
          conditionalPanel(condition = "input.plot_var == 'bar' || input.plot_var == 'pie' || input.plot_var == 'surface'",
                           selectInput("grouping_var", "Grouping Mode:", choices = c("frequency", "sum", "mean", "sd"), selected = "frequency")),
          conditionalPanel(condition = "input.plot_var == 'scatter' || input.plot_var == 'polar' || input.plot_var == 'scatter3d'",
                           selectInput("mode_var", "Drawing Mode:", choices = c("lines", "markers", "text", "lines+markers", "lines+text", "markers+text", "lines+markers+text", "none"), selected = "markers")),
          conditionalPanel(condition = "input.plot_var == 'scatter' || input.plot_var == 'box' || input.plot_var == 'polar' || input.plot_var == 'scatter3d'",
                           selectInput("symbol_var", "Symbol Mode:", choices = c("None", "circle" , "circle-open" , "circle-dot" , "circle-open-dot" , "square" , "square-open" , "square-dot" , "square-open-dot" , "diamond" , "diamond-open" , "diamond-dot" , "diamond-open-dot" , "cross" , "cross-open" , "cross-dot" , "cross-open-dot" , "x" , "x-open" , "x-dot" , "x-open-dot" , "triangle-up" , "triangle-up-open" , "triangle-up-dot" , "triangle-up-open-dot" , "triangle-down" , "triangle-down-open" , "triangle-down-dot" , "triangle-down-open-dot" , "triangle-left" , "triangle-left-open" , "triangle-left-dot" , "triangle-left-open-dot" , "triangle-right" , "triangle-right-open" , "triangle-right-dot" , "triangle-right-open-dot" , "triangle-ne" , "triangle-ne-open" , "triangle-ne-dot" , "triangle-ne-open-dot" , "triangle-se" , "triangle-se-open" , "triangle-se-dot" , "triangle-se-open-dot" , "triangle-sw" , "triangle-sw-open" , "triangle-sw-dot" , "triangle-sw-open-dot" , "triangle-nw" , "triangle-nw-open" , "triangle-nw-dot" , "triangle-nw-open-dot" , "pentagon" , "pentagon-open" , "pentagon-dot" , "pentagon-open-dot" , "hexagon" , "hexagon-open" , "hexagon-dot" , "hexagon-open-dot" , "hexagon2" , "hexagon2-open" , "hexagon2-dot" , "hexagon2-open-dot" , "octagon" , "octagon-open" , "octagon-dot" , "octagon-open-dot" , "star" , "star-open" , "star-dot" , "star-open-dot" , "hexagram" , "hexagram-open" , "hexagram-dot" , "hexagram-open-dot" , "star-triangle-up" , "star-triangle-up-open" , "star-triangle-up-dot" , "star-triangle-up-open-dot" , "star-triangle-down" , "star-triangle-down-open" , "star-triangle-down-dot" , "star-triangle-down-open-dot" , "star-square" , "star-square-open" , "star-square-dot" , "star-square-open-dot" , "star-diamond" , "star-diamond-open" , "star-diamond-dot" , "star-diamond-open-dot" , "diamond-tall" , "diamond-tall-open" , "diamond-tall-dot" , "diamond-tall-open-dot" , "diamond-wide" , "diamond-wide-open" , "diamond-wide-dot" , "diamond-wide-open-dot" , "hourglass" , "hourglass-open" , "bowtie" , "bowtie-open" , "circle-cross" , "circle-cross-open" , "circle-x" , "circle-x-open" , "square-cross" , "square-cross-open" , "square-x" , "square-x-open" , "diamond-cross" , "diamond-cross-open" , "diamond-x" , "diamond-x-open" , "cross-thin" , "cross-thin-open" , "x-thin" , "x-thin-open" , "asterisk" , "asterisk-open" , "hash" , "hash-open" , "hash-dot" , "hash-open-dot" , "y-up" , "y-up-open" , "y-down" , "y-down-open" , "y-left" , "y-left-open" , "y-right" , "y-right-open" , "line-ew" , "line-ew-open" , "line-ns" , "line-ns-open" , "line-ne" , "line-ne-open" , "line-nw" , "line-nw-open"), selected = "circle")),
          conditionalPanel(condition = "input.plot_var == 'bar' || input.plot_var == 'box'",
                           selectInput("ordinal_var", "Grouping:", choices = c("Group", "Degroup"), selected = "group")),
          conditionalPanel(condition = "input.plot_var == 'bar' || input.plot_var == 'histogram' || input.plot_var == 'box'",
                           selectInput("orient_var", "Orientation:", choices = c("v", "h"), selected = "v")),
          conditionalPanel(condition = "input.plot_var == 'histogram' || input.plot_var == 'histogram2d'",
                           numericInput("nbinsx_var", "Bins X (0 = auto): ", value = 0, min = 0)),
          conditionalPanel(condition = "input.plot_var == 'histogram' || input.plot_var == 'histogram2d'",
                           numericInput("nbinsy_var", "Bins Y (0 = auto): ", value = 0, min = 0)),
          conditionalPanel(condition = "input.plot_var == 'contour'",
                           numericInput("ncontour_var", "Contour Levels (0 = auto) [BROKEN]: ", value = 0, min = 0)),
          conditionalPanel(condition = "input.plot_var == 'box'",
                           sliderInput("jitter_var", "Jitter: ", min = 0, max = 1, value = 0)),
          conditionalPanel(condition = "input.plot_var == 'box'",
                           sliderInput("pointpos_var", "Point Position: ", min = -2, max = 2, value = 0)),
          conditionalPanel(condition = "input.plot_var == 'pie'",
                           sliderInput("donut_var", "Pie Donut: ", min = 0, max = 1, value = 0)),
          conditionalPanel(condition = "input.plot_var == 'pie'",
                           sliderInput("pull_var", "Pie Pull: ", min = 0, max = 1, value = 0)),
          conditionalPanel(condition = "input.plot_var == 'surface'",
                           selectInput("kernel_var", "Kernel Estimator (XY):", choices = c("None", "Rule of Thumb Estimator", "Solve the Equation Estimator", "Direct Plug-in Estimator"), selected = "None")),
          conditionalPanel(condition = "input.plot_var == 'surface'",
                           sliderInput("nkernel_var", "Kernel Points (XY, quadratic complexity): ", min = 0, max = 5000, value = 100)),
          conditionalPanel(condition = "input.plot_var == 'bar'",
                           selectInput("annotate_var", "Annotate Bars:", choices = c("Yes", "No"), selected = "Yes")),
          conditionalPanel(condition = "input.plot_var == 'histogram' || input.plot_var == 'histogram2d'",
                           selectInput("histfunc_var", "Histogram Function:", choices = c("count", "sum", "avg", "min", "max"), selected = "count")),
          conditionalPanel(condition = "input.plot_var == 'histogram' || input.plot_var == 'histogram2d'",
                           selectInput("histnorm_var", "Histogram Normalization:", choices = c("None", "percent", "probability", "density", "probability_density"), selected = "None")),
          conditionalPanel(condition = "input.plot_var == 'histogram2d' || input.plot_var == 'heatmap'",
                           selectInput("zsmooth_var", "Z Smooth Algorithm:", choices = c("fast", "best", "FALSE"), selected = "FALSE")),
          conditionalPanel(condition = "input.plot_var == 'heatmap'",
                           selectInput("gaps_var", "Connect Gaps (Matrix Completion):", choices = c("TRUE", "FALSE"), selected = "FALSE")),
          conditionalPanel(condition = "input.plot_var == 'box'",
                           selectInput("boxmean_var", "Box Mean:", choices = c("TRUE", "sd", "FALSE"), selected = "FALSE")),
          conditionalPanel(condition = "input.plot_var == 'box'",
                           sliderInput("whisker_var", "Whisker Width: ", min = 0, max = 1, value = 0.5)),
          conditionalPanel(condition = "input.plot_var == 'box'",
                           selectInput("boxpoints_var", "Box Points:", choices = c("all", "outliers", "suspectedoutliers", "FALSE"), selected = "outliers")),
          conditionalPanel(condition = "input.plot_var == 'polar'",
                           sliderInput("bgfilter_var", "Background Filter: ", min = 0, max = 100, value = 100, round = TRUE)),
          conditionalPanel(condition = "input.plot_var == 'pie'",
                           selectInput("piepos_var", "Pie Text Position:", choices = c("inside", "outside", "auto", "none"), selected = "auto")),
          conditionalPanel(condition = "input.plot_var == 'pie'",
                           selectInput("textinfo_var", "Text Information:", choices = c("none", "label", "text", "value", "percent", "label+text", "label+value", "label+percent", "text+value", "text+percent", "value+percent", "label+text+value", "label+value+percent", "label+text+value+percent"), selected = "percent")),
          conditionalPanel(condition = "input.plot_var == 'scatter' || input.plot_var == 'bar' || input.plot_var == 'pie' || input.plot_var == 'histogram' || input.plot_var == 'histogram2d' || input.plot_var == 'box' || input.plot_var == 'contour' || input.plot_var == 'heatmap' || input.plot_var == 'polar' || input.plot_var == 'scatter3d' || input.plot_var == 'surface'",
                           selectInput("palette_var", "Color Palette:", choices = c("None", "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"), selected = "None")),
          sliderInput("opacity_var", "Opacity: ", min = 0, max = 1, value = 1),
          numericInput("width", "Plot Width:", value = plot_width),
          numericInput("height", "Plot Height:", value = plot_height)
        ),
        width = side_width
      ),
      body = dashboardBody(
        fluidRow(
          box(plotlyOutput("plotted", width = paste0(plot_width, "px"), height = paste0(plot_height, "px")), width = 12)
        )
        
      )
      
    )
    
    server <- function(input, output, session) {
      
      returnScatter <- function(dep, indep, color, size, mode, symbol, palette, opacity, width, height, data) {
        
        to_keep <- c(dep, indep, color, size)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data, x = ~", indep, ", y = ~", dep, ifelse(color == "None", "", paste0(", color = ~", color)), ifelse(size == "None", "", paste0(", size = ~", size)), ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", opacity = ", opacity, ", mode = '", mode, "'", ifelse(symbol == "None", "", paste0(", marker = list(symbol = '", symbol, "')")), ", width = ", width, ", height = ", height, ", type = 'scatter') %>% layout(title = 'Scatter plot of ", dep, " vs ", indep, ifelse(color == "None", "", paste0(" by ", color)), "') %>% toWebGL()")))
        return(my_plot)
        
      }
      
      returnBar <- function(dep, freq, color, grouping, palette, orient, ordinal, annotate, opacity, width, height, data) {
        
        to_keep <- c(dep, freq, color)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        if ((freq == "None") | (grouping == "frequency")) {
          #my_data <- plyr::count(df = my_data, vars = colnames(my_data))
          #my_data <- eval(parse(text = paste0("dplyr::count(my_data, ", dep, ifelse(color == "None", "", paste0(", ", color)), ")")))
          my_data <- eval(parse(text = paste0("my_data[, list(", "freq = .N), by = list(", dep, ifelse(color == "None", "", paste0(", ", color)), ")]")))
          my_data <- eval(parse(text = paste0("my_data[order(", dep, ifelse(color == "None", "", paste0(", ", color)), ")]")))
        } else {
          my_data <- eval(parse(text = paste0("my_data[, list(", ifelse(grouping == "frequency", "freq = .N", paste0(freq, "_", grouping, " = ", grouping, "(", freq, ", na.rm = TRUE)")), "), by = list(", dep, ifelse(color == "None", "", paste0(", ", color)), ")]")))
          my_data <- eval(parse(text = paste0("my_data[order(", dep, ifelse(color == "None", "", paste0(", ", color)), ")]")))
        }
        if (ordinal == "Degroup") {
          my_data[[dep]] <- as.factor(my_data[[dep]])
          if (color != "None") {
            my_data[[color]] <- as.factor(my_data[[color]])
          }
        }
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data, x = ~", dep, ", y = ~", ifelse((freq == "None") | (grouping == "frequency"), "freq", paste0(freq, "_", grouping)), ifelse(color == "None", "", paste0(", color = ~", color)), ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", orientation = '", orient, "', opacity = ", opacity, ", width = ", width, ", height = ", height, ", type = 'bar')", ifelse(annotate == "Yes", paste0(" %>% layout(annotations = list(x = my_data[['", dep, "']], y = my_data[['", ifelse((freq == "None") | (grouping == "frequency"), "freq", paste0(freq, "_", grouping)), "']], text = my_data[['", ifelse((freq == "None") | (grouping == "frequency"), "freq", paste0(freq, "_", grouping)), "']], xanchor = 'center', yanchor = 'bottom', showarrow = ", ifelse(ordinal == "Degroup", "FALSE", "TRUE"), "))"), ""), " %>% layout(title = 'Bar plot of ", ifelse(freq == "None", "", paste0(freq, " vs ")), dep, ifelse(color == "None", "", paste0(" by ", color)), "')")))
        return(my_plot)
        
      }
      
      returnPie <- function(dep, freq, grouping, palette, donut, pull, pietext, textinfo, opacity, width, height, data) {
        
        to_keep <- c(dep, freq)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        if ((freq == "None") | (grouping == "frequency")) {
          my_data <- eval(parse(text = paste0("my_data[, list(", "freq = .N), by = list(", dep, ")]")))
          my_data <- eval(parse(text = paste0("my_data[order(", dep, ")]")))
        } else {
          my_data <- eval(parse(text = paste0("my_data[, list(", ifelse(grouping == "frequency", "freq = .N", paste0(freq, "_", grouping, " = ", grouping, "(", freq, ", na.rm = TRUE)")), "), by = list(", dep, ")]")))
          my_data <- eval(parse(text = paste0("my_data[order(", dep, ")]")))
        }
        my_data[[dep]] <- as.factor(my_data[[dep]])
        levels(my_data[[dep]]) <- paste0(dep, " = ", levels(my_data[[dep]]))
        if (palette != "None") {
          temp_colors <- col2rgb(brewer.pal(length(unique(my_data[[dep]])), palette))
          temp_colored <- character(0)
          for (i in 1:min(c(nrow(temp_colors)), length(unique(my_data[[dep]])))) {
            temp_colored[i] <- paste0("'rgb(", temp_colors[1, i], ", ", temp_colors[2, i], ", ", temp_colors[3, i], ")'")
          }
          temp_colored <- paste0("c(", paste(temp_colored, collapse = ", ") ,")")
        }
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data, labels = ~", dep, ", values = ~", ifelse((freq == "None") | (grouping == "frequency"), "freq", paste0(freq, "_", grouping)), ", hole = ", donut, ", pull = ", pull, ifelse(palette == "None", "", paste0(", marker = list(colors = ", temp_colored, ")")), ", textposition = '", pietext, "', textinfo = '", textinfo, "', opacity = ", opacity, ", width = ", width, ", height = ", height, ", type = 'pie') %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% layout(title = 'Pie plot of ", ifelse(freq == "None", "", paste0(freq, " vs ")), dep, " grouped by ", grouping, "')")))
        return(my_plot)
        
      }
      
      returnHistogram <- function(dep, indep, color, nbinsx, nbinsy, histfunc, histnorm, palette, orient, opacity, width, height, data) {
        
        to_keep <- c(dep, indep, color)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data, x = ~", dep, ifelse(indep == "None", "", paste0(", y = ~", indep)), ifelse(color == "None", "", paste0(", color = ~", color)), ifelse(nbinsx > 0, paste0(", nbinsx = ", nbinsx, ", autobinx = FALSE"), ""), ifelse((nbinsy > 0) & (indep != "None"), paste0(", nbinsy = ", nbinsy, ", autobiny = FALSE"), ""), ", histfunc = '", histfunc, "', histnorm = ", ifelse(histnorm == "None", "''", paste0("'", histnorm, "'")), ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", orientation = '", orient, "', opacity = ", opacity, ", width = ", width, ", height = ", height, ", type = 'histogram') %>% layout(title = 'Histogram plot of ", ifelse(indep == "None", "", paste0(indep, " vs ")), dep, ifelse(color == "None", "", paste0(" by ", color)), "')")))
        return(my_plot)
        
      }
      
      returnHistogram2d <- function(dep, indep, freq, nbinsx, nbinsy, histfunc, histnorm, zsmooth, palette, opacity, width, height, data) {
        
        to_keep <- c(dep, indep, freq)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data, x = ~", dep, ", y = ~", indep, ifelse(freq == "None", "", paste0(", z = ~", freq)), ifelse(nbinsx > 0, paste0(", nbinsx = ", nbinsx, ", autobinx = FALSE"), ""), ifelse(nbinsy > 0, paste0(", nbinsy = ", nbinsy, ", autobiny = FALSE"), ""), ", histfunc = '", histfunc, "', histnorm = ", ifelse(histnorm == "None", "''", paste0("'", histnorm, "'")), ", zsmooth = ", ifelse(zsmooth == "FALSE", "FALSE", paste0("'", zsmooth, "'")), ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", opacity = ", opacity, ", width = ", width, ", height = ", height, ", type = 'histogram2d') %>% layout(title = 'Bar plot of ", dep, " vs ", indep, ifelse(freq == "None", "", paste0(" by ", freq)), " grouped by ", histfunc, " as ", ifelse(histnorm == "None", "frequency", histnorm), "')")))
        return(my_plot)
        
      }
      
      returnBox <- function(dep, indep, color, jitter, pointpos, boxmean, whisker, boxpoints, marker, palette, ordinal, orient, opacity, width, height, data) {
        
        to_keep <- c(dep, indep, color)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        if (ordinal == "Degroup") {
          my_data[[color]] <- as.factor(my_data[[color]])
        }
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data", ifelse(indep == "None", "", paste0(", x = ~", indep)), ", y = ~", dep, ifelse(color == "None", "", paste0(", color = ~", color)), ", jitter = ", jitter, ", pointpos = ", pointpos, ", boxmean = ", ifelse(boxmean == "sd", "'sd'", boxmean), ", whiskerwidth = ", whisker, ", boxpoints = ", ifelse(boxpoints == "FALSE", "FALSE", paste0("'", boxpoints, "'")), ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", orientation = '", orient, "', opacity = ", opacity, ifelse(marker == "None", "", paste0(", marker = list(symbol = '", marker, "')")), ", width = ", width, ", height = ", height, ", type = 'box')", ifelse(ordinal == "Degroup", " %>% layout(boxmode = 'group')", ""), " %>% layout(title = 'Box plot of ", ifelse(indep == "None", "", paste0(indep, " vs ")), dep, ifelse(color == "None", "", paste0(" by ", color)), "')")))
        return(my_plot)
        
      }
      
      returnContour <- function(dep, indep, freq, ncontour, palette, opacity, width, height, data) {
        
        to_keep <- c(dep, indep, freq)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data, x = ~", dep, ", y = ~", indep, ", z = ~", freq, ", ncontours = ", ncontour, ", autocontour = ", ifelse(ncontour == 0, "TRUE", "FALSE"), ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", opacity = ", opacity, ", width = ", width, ", height = ", height, ", type = 'contour') %>% layout(title = 'Contour plot of ", dep, " vs ", indep, " by ", freq, "')")))
        return(my_plot)
        
      }
      
      returnHeatmap <- function(dep, indep, freq, zsmooth, gaps, palette, opacity, width, height, data) {
        
        to_keep <- c(dep, indep, freq)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data, x = ~", dep, ", y = ~", indep, ", z = ~", freq, ", zsmooth = ", ifelse(zsmooth == "FALSE", "FALSE", paste0("'", zsmooth, "'")), ", connectgaps = ", gaps, ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", opacity = ", opacity, ", width = ", width, ", height = ", height, ", type = 'heatmap') %>% layout(title = 'Heatmap plot of ", dep, " vs ", indep, " by ", freq, "')")))
        return(my_plot)
        
      }
      
      returnPolar <- function(radius, angular, color, size, mode, symbol, palette, opacity, bgfilter, width, height, data) {
        
        to_keep <- c(radius, angular, color, size)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        if (color != "None") {
          my_data[[color]] <- as.factor(my_data[[color]])
        }
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data, r = ~", radius, ", t = ~", angular, ifelse(color == "None", "", paste0(", color = ~", color)), ifelse(size == "None", "", paste0(", size = ~", size)), ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", opacity = ", opacity, ", mode = '", mode, "'", ifelse(symbol == "None", "", paste0(", marker = list(symbol = '", symbol, "')")), ", width = ", width, ", height = ", height, ", type = 'scatter') %>% layout(angularaxis = list(range = c(", min(my_data[[angular]], na.rm = TRUE), ", ", max(my_data[[angular]], na.rm = TRUE), "))) %>% layout(plot_bgcolor = '", toRGB(paste0("grey", bgfilter)), "', title = 'Polar plot of ", radius, " vs ", angular, ifelse(color == "None", "", paste0(", by ", color)), "')")))
        return(my_plot)
        
      }
      
      returnScatter3d <- function(dep, indep, freq, color, size, mode, symbol, palette, opacity, width, height, data) {
        
        to_keep <- c(dep, indep, freq, color, size)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        my_plot <- eval(parse(text = paste0("plot_ly(data = my_data, x = ~", dep, ", y = ~", indep, ", z = ~", freq, ifelse(color == "None", "", paste0(", color = ~", color)), ifelse(size == "None", "", paste0(", size = ~", size)), ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", opacity = ", opacity, ", mode = '", mode, "'", ifelse(symbol == "None", "", paste0(", marker = list(symbol = '", symbol, "')")), ", width = ", width, ", height = ", height, ", type = 'scatter3d') %>% layout(title = '3D Scatter plot of (", dep, ", ", indep, ", ", freq, ")", ifelse(color == "None", "", paste0(" by ", color)), "') %>% toWebGL()")))
        return(my_plot)
        
      }
      
      returnSurface <- function(dep, indep, freq, grouping, kernel, nkernel, palette, opacity, width, height, data) {
        
        to_keep <- c(dep, indep, freq)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        setDT(data)
        my_data <- data[, to_keep, with = FALSE]
        my_data <- my_data[complete.cases(my_data), ]
        if (kernel == "None") {
          if ((freq == "None") | (grouping == "frequency")) {
            my_data <- eval(parse(text = paste0("my_data[, list(", "freq = .N), by = list(", dep, ", ", indep, ")]")))
            my_data <- eval(parse(text = paste0("my_data[order(", dep, ", ", indep, ")]")))
          } else {
            my_data <- eval(parse(text = paste0("my_data[, list(", ifelse(grouping == "frequency", "freq = .N", paste0(freq, "_", grouping, " = ", grouping, "(", freq, ", na.rm = TRUE)")), "), by = list(", dep, ", ", indep, ")]")))
            my_data <- eval(parse(text = paste0("my_data[order(", dep, ", ", indep, ")]")))
          }
          my_data <- eval(parse(text = paste0("dcast(my_data, ", indep, " ~ ", dep, ", fun.aggregate = mean, value.var = ", ifelse((freq == "None") | (grouping == "frequency"), "'freq'", paste0("'", freq, "_", grouping, "'")), ")")))
          my_x <- colnames(my_data)
          my_x_range <- range(as.numeric(my_x), na.rm = TRUE)
          my_y <- row.names(my_data)
          my_y_range <- range(as.numeric(my_y), na.rm = TRUE)
          my_z <- as.matrix(my_data[, 2:ncol(my_data), with = FALSE])
          my_z_range <- range(my_z, na.rm = TRUE)
        } else {
          if (kernel == "Rule of Thumb Estimator") {
            my_data <- kernel2d_est(x = my_data[[dep]], y = my_data[[indep]], n = nkernel)
          } else if (kernel == "Solve the Equation Estimator") {
            my_data <- kernel2d_est(x = my_data[[dep]], y = my_data[[indep]], h = c(width.SJ(x = my_data[[dep]], nb = 1000, method = "ste"), width.SJ(x = my_data[[dep]], nb = 1000, method = "ste")), n = nkernel)
          } else {
            my_data <- kernel2d_est(x = my_data[[dep]], y = my_data[[indep]], h = c(width.SJ(x = my_data[[dep]], nb = 1000, method = "dpi"), width.SJ(x = my_data[[dep]], nb = 1000, method = "dpi")), n = nkernel)
          }
          my_x <- my_data$x
          my_x_range <- range(my_x, na.rm = TRUE)
          my_y <- my_data$y
          my_y_range <- range(my_y, na.rm = TRUE)
          my_z <- my_data$z
          my_z_range <- range(my_z, na.rm = TRUE)
        }
        my_plot <- eval(parse(text = paste0("plot_ly(x = my_x, y = my_y, z = my_z", ifelse(palette == "None", "", paste0(", colors = '", palette, "'")), ", opacity = ", opacity, ", width = ", width, ", height = ", height, ", type = 'surface')", " %>% layout(xaxis = list(range = c(", my_x_range[1], ", ", my_x_range[2], ")), yaxis = list(range = c(", my_y_range[1], ", ", my_y_range[2], ")), scene = list(xaxis = list(title = '", dep, "'), yaxis = list(title = '", indep, "'), zaxis = list(title = '", ifelse((freq == "None") | (grouping == "frequency"), "freq", paste0(freq, "_", grouping)), "')), title = 'Surface plot of (", dep, ", ", indep, ", ", freq, ", grouped by ", ifelse(kernel == "None", grouping, "Kernel Bandwidth Estimation"), ")')")))
        return(my_plot)
        
      }
      
      observeEvent(input$width, {
        assign("width", input$width, envir = parent.env(environment()))
      })
      
      observeEvent(input$height, {
        assign("height", input$height, envir = parent.env(environment()))
      })
      
      observeEvent(input$dep_var, {
        updateSelectInput(session, "indep_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]), selected = input$indep_var)
        updateSelectInput(session, "freq_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]), selected = input$freq_var)
        updateSelectInput(session, "color_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]), selected = input$color_var)
        updateSelectInput(session, "size_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]), selected = input$size_var)
      })
      
      observeEvent(input$data, {
        assign("data", get(input$data, envir = .GlobalEnv), envir = parent.env(environment()))
        updateSelectInput(session, "dep_var", choices =  colnames(data), selected = colnames(data)[1])
        updateSelectInput(session, "indep_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "freq_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "color_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "size_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "radius_var", choices = c("None", colnames(data)))
        updateSelectInput(session, "angle_var", choices = c("None", colnames(data)))
      })
      
      observeEvent(input$plot_var, {
        
        if (input$plot_var == "scatter") {
          updateSelectInput(session, "dep_var", "(*) Dependent Variable:")
          updateSelectInput(session, "indep_var", "(*) Independent Variable:")
        } else if (input$plot_var == "bar") {
          updateSelectInput(session, "dep_var", "(*) Dependent Variable:")
          updateSelectInput(session, "freq_var", "Value Variable:")
        } else if (input$plot_var == "pie") {
          updateSelectInput(session, "dep_var", "(*) Dependent Variable:")
          updateSelectInput(session, "freq_var", "Value Variable:")
        } else if (input$plot_var == "histogram") {
          updateSelectInput(session, "dep_var", "(*) Dependent Variable:")
          updateSelectInput(session, "indep_var", "Independent Variable:")
        } else if (input$plot_var == "histogram2d") {
          updateSelectInput(session, "dep_var", "(*) Dependent Variable:")
          updateSelectInput(session, "indep_var", "(*) Independent Variable:")
          updateSelectInput(session, "freq_var", "Value Variable:")
        } else if (input$plot_var == "box") {
          updateSelectInput(session, "dep_var", "(*) Dependent Variable:")
          updateSelectInput(session, "indep_var", "Independent Variable:")
        } else if (input$plot_var == "contour") {
          updateSelectInput(session, "dep_var", "(*) Dependent Variable:")
          updateSelectInput(session, "indep_var", "(*) Independent Variable:")
          updateSelectInput(session, "freq_var", "(*) Value Variable:")
        } else if (input$plot_var == "heatmap") {
          updateSelectInput(session, "dep_var", "(*) Dependent Variable:")
          updateSelectInput(session, "indep_var", "(*) Independent Variable:")
          updateSelectInput(session, "freq_var", "(*) Value Variable:")
        } else if (input$plot_var == "scatter3d") {
          updateSelectInput(session, "dep_var", "(X) Dependent Variable:")
          updateSelectInput(session, "indep_var", "(Y) Independent Variable:")
          updateSelectInput(session, "freq_var", "(Z) Value Variable:")
        } else if (input$plot_var == "surface") {
          updateSelectInput(session, "dep_var", "(X) Dependent Variable:")
          updateSelectInput(session, "indep_var", "(Y) Independent Variable:")
          updateSelectInput(session, "freq_var", "(Z) Value Variable:")
        }
        
      })
      
      observeEvent({input$run_me}, {
        withProgress(message = "Plotting data", detail = "Plotting data...", value = 0, {
          if (input$plot_var == "scatter") {
            temp_out <- returnScatter(input$dep_var, input$indep_var, input$color_var, input$size_var, input$mode_var, input$symbol_var, input$palette_var, input$opacity_var, input$width, input$height, data)
          } else if (input$plot_var == "bar") {
            temp_out <- returnBar(input$dep_var, input$freq_var, input$color_var, input$grouping_var, input$palette_var, input$orient_var, input$ordinal_var, input$annotate_var, input$opacity_var, input$width, input$height, data)
          } else if (input$plot_var == "pie") {
            temp_out <- returnPie(input$dep_var, input$freq_var, input$grouping_var, input$palette_var, input$donut_var, input$pull_var, input$piepos_var, input$textinfo_var, input$opacity_var, input$width, input$height, data)
          } else if (input$plot_var == "histogram") {
            temp_out <- returnHistogram(input$dep_var, input$indep_var, input$color_var, input$nbinsx_var, input$nbinsy_var, input$histfunc_var, input$histnorm_var, input$palette_var, input$orient_var, input$opacity_var, input$width, input$height, data)
          } else if (input$plot_var == "histogram2d") {
            temp_out <- returnHistogram2d(input$dep_var, input$indep_var, input$freq_var, input$nbinsx_var, input$nbinsy_var, input$histfunc_var, input$histnorm_var, input$zsmooth_var, input$palette_var, input$opacity_var, input$width, input$height, data)
          } else if (input$plot_var == "box") {
            temp_out <- returnBox(input$dep_var, input$indep_var, input$color_var, input$jitter_var, input$pointpos_var, input$boxmean_var, input$whisker_var, input$boxpoints_var, input$symbol_var, input$palette_var, input$ordinal_var, input$orient_var, input$opacity_var, input$width, input$height, data)
          } else if (input$plot_var == "contour") {
            temp_out <- returnContour(input$dep_var, input$indep_var, input$freq_var, input$ncontour_var, input$palette_var, input$opacity_var, input$width, input$height, data)
          } else if (input$plot_var == "heatmap") {
            temp_out <- returnHeatmap(input$dep_var, input$indep_var, input$freq_var, input$zsmooth_var, input$gaps_var, input$palette_var, input$opacity_var, input$width, input$height, data)
          } else if (input$plot_var == "polar") {
            temp_out <- returnPolar(input$radius_var, input$angle_var, input$color_var, input$size_var, input$mode_var, input$symbol_var, input$palette_var, input$opacity_var, input$bgfilter_var, input$width, input$height, data)
          } else if (input$plot_var == "scatter3d") {
            temp_out <- returnScatter3d(input$dep_var, input$indep_var, input$freq_var, input$color_var, input$size_var, input$mode_var, input$symbol_var, input$palette_var, input$opacity_var, input$width, input$height, data)
          } else if (input$plot_var == "surface") {
            temp_out <- returnSurface(input$dep_var, input$indep_var, input$freq_var, input$grouping_var, input$kernel_var, input$nkernel_var, input$palette_var, input$opacity_var, input$width, input$height, data)
          }
          
          output$plotted <- renderPlotly(temp_out)
        })
      })
      
    }
    
    shinyApp(ui, server)
    
  }
  
}
