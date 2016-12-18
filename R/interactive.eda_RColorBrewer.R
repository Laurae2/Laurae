#' Interactive Dashboard for Finding the Perfect Color Brewer Palette
#'
#' This interactive dashboard allows the user to find the perfect palette to use from Color Brewer palettes. In addition, an unlimited amount of values is provided, instead of the practical limitations in the original Color Brewer palette. You can also request RGB values, and copy and paste immediately results from text (a vector ready to be pasted in R is provided back as text, along with the evolution of the colors over the n-th color interpolation).
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
#' @param f_back Type: character. A background color character for the header. Defaults to \code{"red"}.
#' @param side_width Type: numeric. The width of the sidebar containing variable names. Defaults to \code{300}.
#' @param plot_height Type: numeric. The height for the plot. Defaults to \code{360}, which fits nicely Full HD screens (1080 vertical pixels).
#' 
#' @return A bandwidth on a scale suitable for the \code{width} argument of \code{density}.
#' 
#' @examples
#' \dontrun{
#' interactive.eda_RColorBrewer()
#' }
#' 
#' @export

interactive.eda_RColorBrewer <- function(f_back = "red",
                                         side_width = 300,
                                         plot_height = 360) {
  
  if (interactive()) {
    
    ui <- dashboardPage(
      skin = f_back,
      header = dashboardHeader(title = "Laurae's RColorBrewer Palette Dashboard", titleWidth = 500),
      sidebar = dashboardSidebar(
        sidebarMenu(
          selectInput("palette", "Color Palette:", choices = c("None", "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"), selected = "None"),
          numericInput("colors", "Amount of Colors:", value = 10),
          selectInput("quoting", "Quotation:", choices = c("Quote (\')", "Double Quote (\")"), selected = "None"),
          selectInput("rgb", "RGB Hexadecimal or Decimal:", choices = c("Hexadecimal (#RRGGBB)", "Decimal (rgb(RRR,GGG,BBB))"), selected = "Hexadecimal (#RRGGBB)")
        ),
        width = side_width
      ),
      body = dashboardBody(
        fluidRow(
          box(plotOutput("plot1", height = plot_height), width = 12),
          box(verbatimTextOutput("plot2"), width = 12),
          box(tableOutput("plot3"), width = 6),
          box(plotOutput("plot4", height = plot_height), width = 6)
        )
        
      )
      
    )
    
    server <- function(input, output, session) {
      
      observeEvent({input$palette; input$colors; input$rgb}, {
        
        if ((input$palette != "None") & (input$colors > 0)) {
          
          new_palette <- brewer.pal_extended(input$colors, input$palette)
          
          temp_dt <- data.table(t(rbind(paste("Color", 1:length(new_palette)), col2rgb(new_palette))))
          colnames(temp_dt) <- c("Color", "Red", "Green", "Blue")
          temp_dt[["Red"]] <- as.integer(temp_dt[["Red"]])
          temp_dt[["Green"]] <- as.integer(temp_dt[["Green"]])
          temp_dt[["Blue"]] <- as.integer(temp_dt[["Blue"]])
          
          output$plot1 <- renderPlot({
            pie(rep(1, length(new_palette)), col = new_palette, main = paste0("Palette: '", input$palette, "', ", input$colors, " colors"))
          })
          
          output$plot2 <- renderPrint({
            if (input$quoting == "Quote (\')") {
              cat("c('", paste(if(input$rgb == "Hexadecimal (#RRGGBB)") {new_palette} else {apply(col2rgb(new_palette), 2, function(x) {paste0("rgb(", x[1], ", ", x[2], ", ", x[3], ")")})}, collapse = "', '"), "')", sep = "")
            } else {
              cat('c("', paste(if(input$rgb == "Hexadecimal (#RRGGBB)") {new_palette} else {apply(col2rgb(new_palette), 2, function(x) {paste0("rgb(", x[1], ", ", x[2], ", ", x[3], ")")})}, collapse = '", "'), '")', sep = "")
            }
          })
          
          output$plot3 <- renderTable({
            temp_dt
          }, rownames = FALSE, colnames = TRUE)
          
          output$plot4 <- renderPlot({
            curve(return(temp_dt[x]$Red), from = 1, to = nrow(temp_dt), n = nrow(temp_dt), type = "o", col = "red", main = paste0("Evolution of '", input$palette, "'\nPalette of ", input$colors, " colors"), xlab = "Color Number", ylab = "Decimal Value")
            curve(return(temp_dt[x]$Green), from = 1, to = nrow(temp_dt), n = nrow(temp_dt), type = "o", col = "green3", add = TRUE)
            curve(return(temp_dt[x]$Blue), from = 1, to = nrow(temp_dt), n = nrow(temp_dt), type = "o", col = "blue", add = TRUE)
          })
          
        } else {
          
          output$plot1 <- renderPlot({
            pie(c(Sky = 78, "Sunny side of pyramid" = 17, "Shady side of pyramid" = 5), init.angle = 315, col = c("deepskyblue", "yellow", "yellow3"), border = FALSE)
          })
          
        }
        
      })
      
    }
    
    shinyApp(ui, server)
    
  }
  
}
