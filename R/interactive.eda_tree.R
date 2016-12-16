#' Interactive Dashboard for the Non-Linear Feature Engineering Assistant
#'
#' This function is a massive helper in feature engineering, supposing your variables are already conditioned well enough for 2-way or deeper interactions and you are looking for non-linear relationships. It uses a decision tree (Classification and Regression Trees), and supports factors, integer, and numeric variables.
#' 
#' To use this function properly, you require to set the \code{max_depth} to a very small value (like \code{3}). This ensures interpretability.
#' 
#' Moreover, if you have a sparse frame (with lot of missing values), it is important to keep an eye at \code{surrogate_type} and \code{surrogate_style} as they will dictate whether a split point will be made depending on the missing values. Default values are made to handle them appropriately. However, if your intent is to penalize missing values (for instance if missing values are anomalies), changing their values respectively to \code{0} and \code{1} is recommended.
#' 
#' The colors (\code{tree_back}, \code{gain_back}, \code{rules_back}, \code{details_back}) allowed are the following: 
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
#' @param data Type: name reference to a data.frame (preferred) or data.table. Your data, preferably a data.frame but it "should" also work perfectly with data.table.
#' @param label Type: character. The name of the label feature in the data. Defaults to \code{"!!!!! SELECT ME !!!!!"}
#' @param ban Type: vector of characters or of numerics The names (or column numbers) of variables to be banned from the decision tree. Defaults to \code{NULL}, which means no variables are banned (all variables are potentially used for the decision tree). Defaults to \code{NULL}.
#' @param antiban Type: boolean. Whether banned variable selection should be inverted, which means if \code{"yes"}, the \code{ban} transforms into a selection (which bans all other variables not "banned" initially). Defaults to \code{"yes"}.
#' @param type Type: character. The type of problem to solve. Either classification (\code{"class"}), regression (\code{"anova"}), count (\code{"poisson"}), or survival (\code{"exp"}). Defaults to \code{"auto"}, which will attempt to find the base type (classification / regression) of model to create using simple heuristics.
#' @param split Type: character. If a classification task has been requested (\code{type = "class"}), then the split must be either set to \code{"gini"} (for Gini index) or \code{"information"} (for Information Gain) as the splitting rule. Defaults to \code{"information"} as it is less biased than \code{"gini"} when it comes to cardinalities.
#' @param folds Type: integer or character. The folds to use for cross-validation. If you intend to keep the same folds over and over, it is preferrable to provide your own fold character (use a variable name). A numeric vector matching the length of \code{label} is also valid.
#' @param seed Type: integer. The random seed applied to the decision tree and the fold generation (if required).
#' @param verbose Type: boolean. Whether to print debug information about the model. For each node, a maximum of \code{competing_splits + surrogate_search} rows will be printed. Defaults to \code{TRUE}.
#' @param plots Type: boolean. Whether to plot debug information about the model. If using knitr / Rmarkdown, you will have two plots printed: the complexity plot, and the decision tree. Without knitr / Rmarkdown, make sure you look at both. Defaults to \code{TRUE}.
#' @param min_split Type: integer. The minimum number of observations in a node to allow a split to be made. If this number is not reached in a node, the node is kept but any other potential splits are cancelled. Keep it large to avoid overfitting. Defaults to \code{max(20, nrow(data) / 1000)}, which is the maximum between 20 and the 0.1\% of the number of observations.
#' @param max_depth Type: numeric. The maximum depth of the decision tree. Do not set to large values if the intent is for analysis. Defaults to \code{3}. Any value greater than \code{30} will cause issues on 32-bit operating systems due to C code.
#' @param min_bucket Type: integer. The minimum number of observations in a leaf. If this number is not reached in a leaf, the leaf is destroyed. Defaults to \code{round(min_split/3)}, which means by defaults at least 7 to approximately 0.033\% of the number of observations.
#' @param min_improve Type: numeric. The minimum fitting improvement to create a node (complexity parameter in Classification and Regression Trees). For regression, the requirement for a leaf to be created and kept is an R-squared increase by at least \code{min_improve}. For classification, the purity (issued from Gini or Information Gain) must increase by at least \code{min_improve}.
#' @param competing_splits Type: numeric. The number of best splitting rules retained per split. When using \code{verbose = TRUE}, each node will have \code{competing_splits} rules printed, if they are adequate enough (instead of only one splitting rule). This allows the user to lookup for more details. Defaults to \code{4}.
#' @param surrogate_search Type: numeric. The number of surrogate splits to look for. A greater number means more surrogates will be looked for, but increased computation time is required. They are also printed when \code{verbose = TRUE}. Defaults to \code{5}.
#' @param surrogate_type Type: numeric. Controls the surrogate creation, with three possible values. If set to \code{0}, any surrogates with missing values are not used for the tree. If set to \code{1}, when all surrogates are with missing values, they are not used the tree. If set to \code{2}, when all surrogates are not used, the majority rule is used (Breiman tree). Sparse frames should preferably use \code{2}. It is recommended to use \code{2} as it handles better missing values, which is the default. Set to \code{0} if you need to ignore as much as possible missing values.
#' @param surrogate_style Type: numeric. Controls the selection of the best surrogate, with two values. If set to \code{1}, any missing values in the surrogate is removed to compute the correctness of the surrogate. If set to \code{0}, it ignores any missing values and takes into account all observations to compute the correctness of the surrogate. Defaults to \code{0}. Set to \code{1} if you need to ignore as much as possible missing values.
#' @param tree_back Type: character. A background color character for the tree plot. Defaults to \code{"red"}.
#' @param gain_back Type: character. A background color character for the gain plot. Defaults to \code{"red"}.
#' @param rules_back Type: character. A background color character for the rules. Defaults to \code{"red"}.
#' @param details_back Type: character. A background color character for the details Defaults to \code{"red"}.
#' @param f_back Type: character. A background color character for the header. Defaults to \code{"red"}.
#' @param side_width Type: numeric. The width of the sidebar containing variable names. Defaults to \code{300}.
#' @param tree_height Type: numeric. The maximum height for the tree plot. Defaults to \code{580}, which fits nicely Full HD screens (580 vertical pixels).
#' @param gain_height Type: numeric. The maximum height for the gain plot. Defaults to \code{200}, which fits nicely Full HD screens (200 vertical pixels).
#' 
#' @return The fitted \code{rpart} model.
#' 
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' library(rpart)
#' library(rpart.plot)
#' library(partykit)
#' library(datasets)
#' data(faithful)
#' interactive.eda_tree(data = "faithful",
#'                      label = "!!!!! SELECT ME !!!!!",
#'                      ban = NULL,
#'                      antiban = "Yes",
#'                      type = "auto",
#'                      split = "information",
#'                      folds = 5,
#'                      seed = 0,
#'                      verbose = TRUE,
#'                      plots = TRUE,
#'                      max_depth = 4,
#'                      min_split = max(20, nrow(data)/1000),
#'                      min_bucket = round(min_split/3),
#'                      min_improve = 0.01,
#'                      competing_splits = 2,
#'                      surrogate_search = 5,
#'                      surrogate_type = 2,
#'                      surrogate_style = 0,
#'                      tree_back = "red",
#'                      gain_back = "red",
#'                      rules_back = "red",
#'                      details_back = "red",
#'                      f_back = "red",
#'                      side_width = 300,
#'                      tree_height = 580,
#'                      gain_height = 200)
#' }

interactive.eda_tree <- function(data,
                                 label = "!!!!! SELECT ME !!!!!",
                                 ban = NULL,
                                 antiban = "Yes",
                                 type = "auto",
                                 split = "information",
                                 folds = 5,
                                 seed = 0,
                                 verbose = TRUE,
                                 plots = TRUE,
                                 max_depth = 4,
                                 min_split = max(20, nrow(data)/1000),
                                 min_bucket = round(min_split/3),
                                 min_improve = 0.01,
                                 competing_splits = 2,
                                 surrogate_search = 5,
                                 surrogate_type = 2,
                                 surrogate_style = 0,
                                 tree_back = "red",
                                 gain_back = "red",
                                 rules_back = "red",
                                 details_back = "red",
                                 f_back = "red",
                                 side_width = 300,
                                 tree_height = 580,
                                 gain_height = 200) {
  
  if (interactive()) {
    
    ui <- dashboardPage(
      skin = "red",
      header = dashboardHeader(title = "Laurae's Non-Linear Feature Engineering Dashboard", titleWidth = 500),
      sidebar = dashboardSidebar(
        sidebarMenu(
          actionButton("run_me", "Run Feature Engineering Assistant", icon("refresh")),
          selectInput("data", "Data (Global Environment):", choices = ls(envir = .GlobalEnv)[sapply(ls(.GlobalEnv), function(x) class(get(x))) == 'data.frame'], selected = data),
          selectInput("label", "Label Variable:", choices = c(label, colnames(data)), selected = label),
          selectInput("ban", "Feature Selection:", choices = colnames(data), selected = ban, multiple = TRUE),
          selectInput("antiban", "Ban Selection:", choices = c("Yes", "No"), selected = antiban),
          selectInput("type", "Type:", choices = c("auto", "class", "anova", "poisson", "exp"), selected = type),
          selectInput("split", "Split (Classification):", choices = c("gini", "information"), selected = split),
          textInput("folds", "Folds (text for variable):", value = as.character(folds)),
          numericInput("max_depth", "Maximum Depth:", value = max_depth),
          numericInput("min_split", "Minimum Observations for a Node:", value = min_split),
          numericInput("min_bucket", "Minimum Observations for a Leaf:", value = min_bucket),
          numericInput("min_improve", "Minimum Improvement for a Node:", value = min_improve),
          numericInput("competing_splits", "Competing Splits Report:", value = competing_splits),
          numericInput("surrogate_search", "Number of Surrogates:", value = surrogate_search),
          numericInput("surrogate_type", "Surrogate Type (0, 1, 2):", value = surrogate_type),
          numericInput("surrogate_style", "Surrogate Style (0, 1):", value = surrogate_style),
          numericInput("seed", "Seed:", value = seed)
        ),
        width = side_width
      ),
      body = dashboardBody(
        fluidRow(
          if (plots) {box(plotOutput("plot1", height = tree_height), width = 12, background = tree_back)} else {NULL},
          if (plots) {box(plotOutput("plot2", height = gain_height), width = 12, background = gain_back)} else {NULL},
          if (verbose) {box(tableOutput("plot4"), width = 12, background = rules_back)} else {NULL},
          if (verbose) {box(verbatimTextOutput("plot3"), width = 12, background = details_back)} else {NULL}
        )
        
      )
      
    )
    
    server <- function(input, output, session) {
      
      rule_pred <- function(model, ...) {
        if(!inherits(model, "party")) model <- as.party(model)
        #rules <- partykit:::.list.rules.party(model)
        rules <- GetPartyRules(model)
        rules <- data.table(ruleID = 1:length(rules), Rule = rules)
        return(unique(rules))
      }
      
      observeEvent(input$label, {
        updateSelectInput(session, "ban", choices = colnames(data)[!colnames(data) %in% input$label])
        updateSelectInput(session, "label", choices = colnames(data), selected = input$label)
      })
      
      observeEvent(input$data, {
        assign("data", get(input$data, envir = .GlobalEnv), envir = parent.env(environment()))
        updateSelectInput(session, "label", choices =  c("!!!!! SELECT ME !!!!!", colnames(data)), selected = "!!!!! SELECT ME !!!!!")
        updateSelectInput(session, "ban", choices = colnames(data))
        updateNumericInput(session, "min_split", value = max(20, nrow(data) / 1000))
        updateNumericInput(session, "min_bucket", value = round(max(20, nrow(data) / 1000) / 3))
      })
      
      temp_model <- observeEvent(input$run_me, {
        isolate({
          temp_model <- FeatureLookup(data = data,
                                      label = data[[input$label]],
                                      ban = input$ban,
                                      antiban = ifelse(input$antiban == "No", FALSE, TRUE),
                                      type = input$type,
                                      split = input$split,
                                      folds = if(is.na(as.numeric(input$folds))) {get(input$folds, envir = .GlobalEnv)} else {as.numeric(input$folds)},
                                      seed = input$seed,
                                      verbose = FALSE,
                                      plots = FALSE,
                                      max_depth = input$max_depth,
                                      min_split = input$min_split,
                                      min_bucket = input$min_bucket,
                                      min_improve = input$min_improve,
                                      competing_splits = input$competing_splits,
                                      surrogate_search = input$surrogate_search,
                                      surrogate_type = input$surrogate_type,
                                      surrogate_style = input$surrogate_style)
        })
        
        if (plots) {
          
          output$plot1 <- renderPlot({
            rpart.plot(temp_model, main = "Decision Tree")
          })
          
          output$plot2 <- renderPlot({
            plotcp(temp_model)
          })
          
        }
        
        if (verbose) {
          
          output$plot3 <- renderPrint({
            summary(temp_model)
          })
          
          output$plot4 <- renderTable({
            rule_pred(temp_model)
          })
          
        }
        
      })
      
    }
    
    shinyApp(ui, server)
    
  }
  
}
