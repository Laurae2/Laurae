#' Interactive Dashboard for Exploratory Data Analysis (d3js)
#'
#' This function runs an interactive dashboard which allows to explore data via 3djs and NVD3. Unlike its counterpart \code{interactive.eda_ggplot}, using 3djs is not efficient for large data (10K+ observations) and automated smart plotting is not available. In addition, the filtering method is currently broken. This function is not recommended if you want d3js plot and you should use \code{interactive.eda_plotly}.
#' 
#' Plotting is done using \code{rCharts}, which must be loaded before running this function. There are issues if you open this in Internet Explorer 9 or under.
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
#' @param data Type: data.table (preferred) or data.frame. The data you want to explore. Using a data.table increases the processing speed.
#' @param type Type: character. The type of plot requested to d3js nvds. You may choose between \code{"lineChart"}, \code{"scatterChart"}, \code{"stackedAreaChart"}, \code{"discreteBarChart"}, \code{"multiBarChart"}, \code{"multiBarHorizontalChart"}, \code{"linePlusBarChart"}, \code{"cumulativeLineChart"}, \code{"lineWithFocusChart"}, \code{"pieChart"}, \code{"bulletChart"}. Defaults to \code{"scatterChart"}.
#' @param dep_var Type: character (currently overriden). The dependent variable to plot. Defaults to \code{colnames(data)[1]}, a reference to the first column of \code{data}.
#' @param indep_var Type: character (currently overriden). The independent variable to plot. Defaults to \code{"None"}.
#' @param group_var Type: character (currently overriden). The grouping variable to plot. Defaults to \code{"None"}.
#' @param filter_var Type: character (feature currently broken). The filtering variable to plot. Defaults to \code{"None"}.
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
#' library(rCharts)
#' library(datasets)
#' hair_eye <- as.data.frame(HairEyeColor)
#' interactive.eda_d3js(data = hair_eye,
#'                      type = "multiBarChart",
#'                      dep_var = "Hair", #ignored
#'                      indep_var = "Freq", #ignored
#'                      group_var = "Sex", #ignored
#'                      plot_width = 600,
#'                      plot_height = 420,
#'                      f_back = "red",
#'                      side_width = 300)
#' }
#' 
#' @export

interactive.eda_d3js <- function(data,
                                 type = "scatterChart",
                                 dep_var = colnames(data)[1],
                                 indep_var = "None",
                                 group_var = "None",
                                 filter_var = "None",
                                 plot_width = 1500,
                                 plot_height = 820,
                                 f_back = "red",
                                 side_width = 300) {
  
  if (interactive()) {
    
    ui <- dashboardPage(
      skin = f_back,
      header = dashboardHeader(title = "Laurae's Data Explorer (3djs) Dashboard", titleWidth = 500),
      sidebar = dashboardSidebar(
        sidebarMenu(
          verbatimTextOutput("rule_name"),
          actionButton("run_me", "Create 3djs Plot", icon("refresh")),
          selectInput("data", "Data (Global Environment):", choices = ls(envir = .GlobalEnv)[sapply(ls(.GlobalEnv), function(x) class(get(x))[length(class(get(x)))]) == 'data.frame'], selected = data),
          selectInput("plot_var", "Plot Type:", choices = c("lineChart", "scatterChart", "stackedAreaChart", "discreteBarChart", "multiBarChart", "multiBarHorizontalChart", "linePlusBarChart", "cumulativeLineChart", "lineWithFocusChart", "pieChart", "bulletChart"), selected = type),
          selectInput("dep_var", "Label Variable:", choices = colnames(data), selected = dep_var),
          selectInput("indep_var", "Independent Variable:", choices = c("None", colnames(data)[-1]), selected = indep_var),
          selectInput("group_var", "Grouping Variable:", choices = c("None", colnames(data)[-1]), selected = group_var),
          selectInput("filter_var", "Filtering Variable (broken =( ):", choices = c("None", colnames(data)[-1]), selected = filter_var),
          numericInput("width", "Plot Width:", value = plot_width),
          numericInput("height", "Plot Height:", value = plot_height)
        ),
        width = side_width
      ),
      body = dashboardBody(
        fluidRow(
          box(showOutput("plotted", lib = "nvd3"), width = 12)
        )
        
      )
      
    )
    
    server <- function(input, output, session) {
      
      returnplot <- function(dep, indep, group, filter, plot_type, width, height, data) {
        to_keep <- c(dep, indep, group, filter)
        to_keep <- to_keep[grep("None", to_keep, invert = TRUE)]
        if (length(to_keep) == 1) {
          my_data <- eval(parse(text = paste0("data.table(", to_keep, " = data[['", to_keep, "']])")))
        } else {
          my_data <- data[, to_keep]
        }
        NA_mat <- complete.cases(my_data)
        NA_sum <- sum(!NA_mat)
        if (NA_sum > 0) {
          my_data <- my_data[NA_mat, ]
        }
        my_plot <- eval(parse(text = paste0("nPlot(", ifelse(indep == "None", "", indep), " ~ ", dep, ifelse(group == "None", "", paste0(", group = '", group, "'")), ", data = my_data, type = '", plot_type, "')")))
        if (filter != "None") {
          eval(parse(text = paste0("my_plot$addFilters('", filter, "')")))
        }
        my_plot$set(width = width, height = height - ifelse(filter != "None", 250, 0))
        return(list(plot = my_plot, NAs = NA_sum))
      }
      
      observeEvent(input$dep_var, {
        updateSelectInput(session, "indep_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]), selected = input$indep_var)
        updateSelectInput(session, "group_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]), selected = input$group_var)
        updateSelectInput(session, "filter_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]), selected = input$filter_var)
      })
      
      observeEvent(input$data, {
        assign("data", get(input$data, envir = .GlobalEnv), envir = parent.env(environment()))
        updateSelectInput(session, "dep_var", choices =  colnames(data), selected = colnames(data)[1])
        updateSelectInput(session, "indep_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "group_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "filter_var", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        output$plotted <- renderPlot({returnplot(input$dep_var, input$indep_var, input$group_var, input$filter_var, input$plot_var, input$width, input$height, data)})
      })
      
      output$rule_name <- renderText({"3djs plots via NVD3 from rCharts."})
      
      observeEvent({input$run_me}, {
        withProgress(message = "Plotting data", detail = "Preparing data...", value = 0, {
          temp_out <- returnplot(input$dep_var, input$indep_var, input$group_var, input$filter_var, input$plot_var, input$width, input$height, data)
          output$plotted <- renderChart2({temp_out[["plot"]]})
          output$rule_name <- renderText({paste0("3djs plots via NVD3 from rCharts.\n", temp_out[["NAs"]], " rows with missing values.")})
        })
      })
      
    }
    
    shinyApp(ui, server)
    
  }
  
}
