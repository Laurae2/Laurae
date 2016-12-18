#' Interactive Dashboard for Exploratory Data Analysis (ggplot)
#'
#' This function runs an interactive dashboard which allows to explore data in a WYSIWYG (What You See Is What You Get). It uses automated variable detection along with smart automated plotting, which eases the task for data exploration.
#' 
#' Plotting is done using \code{plotluck}, which must be loaded along \code{ggplot2}.
#' 
#' The colors (\code{p_back}) allowed are the following: 
#' 
#' \describe{
#'   \item{red}{red color}
#'   \item{yellow}{yellow color}
#'   \item{aqua}{aqua color}
#'   \item{blue}{blue color}
#'   \item{light-blue}{light-blue color}
#'   \item{green}{green color}
#'   \item{navy}{navy color}
#'   \item{teal}{teal color}
#'   \item{olive}{olive color}
#'   \item{lime}{lime color}
#'   \item{orange}{orange color}
#'   \item{fuchsia}{fuchsia color}
#'   \item{purple}{purple color}
#'   \item{maroon}{maroon color}
#'   \item{black}{black color}
#' }
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
#' @param data Type: data.table (preferred) or data.frame. The data you want to explore. Using a data.table allows to avoid copying in-memory when switching datasets.
#' @param p_back Type: character. A background color character for the plot frame. Defaults to \code{"red"}.
#' @param f_back Type: character. A background color character for the header. Defaults to \code{"red"}.
#' @param side_width Type: numeric. The width of the sidebar containing variable names. Defaults to \code{300}.
#' @param max_height Type: numeric. The maximum height for the plots. Defaults to \code{820}, which fits nicely Full HD screens (820 vertical pixels).
#' 
#' @return Nothing
#' 
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(ggplot2)
#' library(plotluck)
#' library(datasets)
#' data(faithful)
#' interactive.eda_ggplot(data = faithful,
#'                        p_back = "red",
#'                        f_back = "red",
#'                        side_width = 300,
#'                        max_height = 820)
#' }
#' 
#' @export

interactive.eda_ggplot <- function(data,
                                   p_back = "red",
                                   f_back = "red",
                                   side_width = 300,
                                   max_height = 820) {
  
  if (interactive()) {
    
    ui <- dashboardPage(
      skin = f_back,
      header = dashboardHeader(title = "Laurae's Data Explorer (ggplot) Dashboard", titleWidth = 500),
      sidebar = dashboardSidebar(
        sidebarMenu(
          verbatimTextOutput("rule_name"),
          selectInput("data", "Data (Global Environment):", choices = ls(envir = .GlobalEnv)[sapply(ls(.GlobalEnv), function(x) class(get(x))[length(class(get(x)))]) == 'data.frame'], selected = data),
          selectInput("dep_var", "Label Variable:", choices = colnames(data), selected = colnames(data)[1]),
          selectInput("indep_var1", "Independent Variable 1:", choices = c("None", colnames(data)[-1]), selected = "None"),
          selectInput("indep_var2", "Independent Variable 2:", choices = c("None", colnames(data)[-1]), selected = "None"),
          selectInput("cond_var1", "Conditioning Variable 1:", choices = c("None", colnames(data)[-1]), selected = "None"),
          selectInput("cond_var2", "Conditioning Variable 2:", choices = c("None", colnames(data)[-1]), selected = "None"),
          selectInput("weight_var", "Weighting Variable:", choices = c("None", colnames(data)[-1]), selected = "None")
        ),
        width = side_width
      ),
      body = dashboardBody(
        fluidRow(
          box(plotOutput("plotted", height = max_height), width = 12, background = p_back)
        )
        
      )
      
    )
    
    server <- function(input, output, session) {
      
      returnplot <- function(dep, indep, cond, weight, data) {
        eval(parse(text = paste0("plotluck(data = data, ", dep, ifelse((indep[1] == "None") & (indep[2] == "None"), "~1", ifelse(indep[2] == "None", paste0("~", indep[1]), ifelse(indep[1] == "None", paste0("~", indep[2]), paste0("~", paste(indep, collapse = "+"))))), ifelse((cond[1] == "None") & (cond[2] == "None"), "", ifelse(cond[2] == "None", paste0("|", cond[1]), ifelse(cond[1] == "None", paste0("|", cond[2]), paste0("|", paste(cond, collapse = "+"))))), ifelse(weight == "None", "", paste0(", weights = ", weight)), ", opts = plotluck.options(sample.max.rows = 99999999)) + theme_bw()")))
      }
      
      observeEvent(input$dep_var, {
        updateSelectInput(session, "indep_var1", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "indep_var2", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "cond_var1", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "cond_var2", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
      })
      
      observeEvent(input$data, {
        assign("data", get(input$data, envir = .GlobalEnv), envir = parent.env(environment()))
        updateSelectInput(session, "dep_var", choices =  colnames(data), selected = colnames(data)[1])
        updateSelectInput(session, "indep_var1", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "indep_var2", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "cond_var1", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        updateSelectInput(session, "cond_var2", choices = c("None", colnames(data)[!colnames(data) %in% input$dep_var]))
        output$plotted <- renderPlot({returnplot(input$dep_var, c(input$indep_var1, input$indep_var2), c(input$cond_var1, input$cond_var2), input$weight_var, data)})
      })
      
      observeEvent({input$dep_var; input$indep_var1; input$indep_var2; input$cond_var1; input$cond_var2; input$weight_var}, {
        withProgress(message = "Plotting data", detail = "Preparing data...", value = 0, {
          output$plotted <- renderPlot({returnplot(input$dep_var, c(input$indep_var1, input$indep_var2), c(input$cond_var1, input$cond_var2), input$weight_var, data)})
        })
      })
      
      output$rule_name <- renderText({"Select ONLY at most 3 variables!  \nWeight is excluded from the count."})
      
    }
    
    shinyApp(ui, server)
    
  }
  
}
