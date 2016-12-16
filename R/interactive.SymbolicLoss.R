#' Interactive Dashboard for Symbolic Gradient/Hessian Loss Behavior Exploration
#'
#' This function runs an interactive dashboard which computes the 1st and 2nd symbolic derivatives of the loss function (gradient/hessian) provided, for up to 4 functions simultaneously without interrupting if you input a bad function.
#' 
#' This function cannot handle any type of input. It cannot handle sums or loops in the function code. It handles the following, in the alphabetic order:
#' 
#' \describe{
#'   \item{\*}{Multiplication}
#'   \item{/}{Division}
#'   \item{^}{Power}
#'   \item{abs}{Absolute value function}
#'   \item{acos}{Arcosine function}
#'   \item{acosh}{Hyperbolic Arcosine function}
#'   \item{asin}{Arsine function}
#'   \item{asinh}{Hyperbolic Arcsine function}
#'   \item{atan}{Arctangent function}
#'   \item{atan2}{Arctangent angle function between the x-axis and the vector from the origin (x,y), atan=y/x if x>0 and y>0}
#'   \item{atanh}{Hyperbolic Arctangent function}
#'   \item{besselI}{Modified Bessel function of the first kind}
#'   \item{besselJ}{Bessel function of the first kind}
#'   \item{besselK}{Modified Bessel function of the second kind}
#'   \item{besselY}{Sphereical Bessel function}
#'   \item{beta}{Beta function (Eulerian integral of the first kind)}
#'   \item{cos}{Cosine function}
#'   \item{cosh}{Hyperbolic cosine function}
#'   \item{cospi}{Cosine function with argument multiplicand pi}
#'   \item{dbinom}{Density binomial function}
#'   \item{digamma}{First derivative of the logarithm of the gamma function}
#'   \item{dnorm}{Density normal function}
#'   \item{exp}{Exponential function}
#'   \item{expm1}{Exponential function minus 1}
#'   \item{gamma}{Gamma function (Mellin transform of the negative exponential function)}
#'   \item{lbeta}{Natural logarithm of the beta function}
#'   \item{lgamma}{Natural logarithm of the absolute value of the gamma function}
#'   \item{log}{Natural (e) logarithm function}
#'   \item{log10}{Common (10) logarithm function}
#'   \item{log1p}{Natural (e) logarithm function with 1 added to the argument}
#'   \item{log2}{Binary (2) logarithm function}
#'   \item{logb}{Logarithm function of base b (base)}
#'   \item{pnorm}{Normal distribution function}
#'   \item{psigamma}{Polygamma function (degree specified by deriv)}
#'   \item{rep.int}{Replicate "times" elements of vectors and lists}
#'   \item{rep_len}{Replicate "length.out" elements of vectors and lists}
#'   \item{sign}{Sign function}
#'   \item{sin}{Sine function}
#'   \item{sinh}{Hyperbolic sine function}
#'   \item{sinpi}{Sine function with argument multiplicand pi}
#'   \item{sqrt}{Square root function}
#'   \item{tan}{Tangent function}
#'   \item{tanh}{Hyperbolic tangent function}
#'   \item{tanpi}{Tangent function with argument multiplicand pi}
#'   \item{trigamma}{Second derivative of the logarithm of the gamma function}
#' }
#' 
#' The colors (non header: \code{f1_back}, \code{f2_back}, \code{f3_back}, \code{f4_back}) allowed are the following: 
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
#' @param f1 Type: character. A string describing the first loss function, with potentially multiple arguments. Requires at least \code{(x, y)} arguments. Defaults to \code{"(x, y) {(x - y) ^ 2}"}.
#' @param f2 Type: character. A string describing the second loss function, with potentially multiple arguments. Requires at least \code{(x, y)} arguments. Defaults to \code{"(x, y) {(y * log(x) + (1 - y) * log(1 - x))}"}.
#' @param f3 Type: character. A string describing the third loss function, with potentially multiple arguments. Requires at least \code{(x, y)} arguments. Defaults to \code{"(x, y) {(x - y * log(x)) + (y * log(y) - y)}"}.
#' @param f4 Type: character. A string describing the fourth loss function, with potentially multiple arguments. Requires at least \code{(x, y)} arguments. Defaults to \code{"(x, y) {(y - x) * log(y / x)}"}.
#' @param f1_init Type: numeric vector of length 4. A vector containing sequentially the minimum, the maximum, the number of points, and the y value for the plots of the first loss function. Defaults to \code{c(-20, 20, 50, 0),}.
#' @param f2_init Type: numeric vector of length 4. A vector containing sequentially the minimum, the maximum, the number of points, and the y value for the plots of the second loss function. Defaults to \code{c(0.01, 0.99, 50, 1)}.
#' @param f3_init Type: numeric vector of length 4. A vector containing sequentially the minimum, the maximum, the number of points, and the y value for the plots of the third loss function. Defaults to \code{c(0, 100, 100, 20)}.
#' @param f4_init Type: numeric vector of length 4. A vector containing sequentially the minimum, the maximum, the number of points, and the y value for the plots of the fourth loss function. Defaults to \code{c(0, 100, 100, 20)}.
#' @param f1_back Type: character. A background color character for the first function. Defaults to \code{"yellow"}.
#' @param f2_back Type: character. A background color character for the second function. Defaults to \code{"aqua"}.
#' @param f3_back Type: character. A background color character for the third function. Defaults to \code{"olive"}.
#' @param f4_back Type: character. A background color character for the fourth function. Defaults to \code{"purple"}.
#' @param f_back Type: character. A background color character for the header. Defaults to \code{"red"}.
#' @param type Type: character. The type of plot to use for plots. \code{"p"} for points, \code{"l"} for lines, \code{"b"} for points+line, \code{"c"} for line without points, \code{"o"} for overplotted (points+line overlapping), \code{"h"} for high-density vertical lines (histogram-like), \code{"s"} for optimistic stair steps, \code{"S"} for pessimistic stair steps, \code{"n"} to plot nothing. Defaults to \code{"o"} for overplotted.
#' @param max_height Type: numeric. The maximum height for the plots. Defaults to \code{580}, which fits nicely Full HD screens (1080 vertical pixels).
#' 
#' @return Nothing
#' 
#' @examples
#' \dontrun{
#' interactive.SymbolicLoss(f1 = "(x, y) {(x - y) ^ 2}",
#'                          f2 = "(x, y) {(y * log(x) + (1 - y) * log(1 - x))}",
#'                          f3 = "(x, y) {(x - y * log(x)) + (y * log(y) - y)}",
#'                          f4 = "(x, y) {(y - x) * log(y / x)}",
#'                          f1_init = c(-20, 20, 50, 0),
#'                          f2_init = c(0.01, 0.99, 50, 1),
#'                          f3_init = c(0, 100, 100, 20),
#'                          f4_init = c(0, 100, 100, 20),
#'                          f1_back = "yellow",
#'                          f2_back = "aqua",
#'                          f3_back = "olive",
#'                          f4_back = "purple",
#'                          f_back = "red",
#'                          type = "o",
#'                          max_height = 580)
#' }
#' 
#' @export

interactive.SymbolicLoss <- function(f1 = "(x, y) {(x - y) ^ 2}",
                                     f2 = "(x, y) {(y * log(x) + (1 - y) * log(1 - x))}",
                                     f3 = "(x, y) {(x - y * log(x)) + (y * log(y) - y)}",
                                     f4 = "(x, y) {(y - x) * log(y / x)}",
                                     f1_init = c(-20, 20, 50, 0),
                                     f2_init = c(0.01, 0.99, 50, 1),
                                     f3_init = c(0, 100, 100, 20),
                                     f4_init = c(0, 100, 100, 20),
                                     f1_back = "yellow",
                                     f2_back = "aqua",
                                     f3_back = "olive",
                                     f4_back = "purple",
                                     f_back = "red",
                                     type = "o",
                                     max_height = 580) {
  
  if (interactive()) {
    
    ui <- dashboardPage(
      skin = f_back,
      header = dashboardHeader(
        dropdownMenu(type = "messages",
                     messageItem(
                       from = "How do I use this?",
                       message = "Just fill in the text boxes.",
                       icon = icon("question"),
                       time = format(Sys.time(), "%H:%M")
                     ),
                     messageItem(
                       from = "Computer",
                       message = "What time is it?",
                       icon = icon("life-ring"),
                       time = Sys.time()
                     )
        ),
        title = "Laurae's Loss Objective Dashboard", titleWidth = 600),
      sidebar = dashboardSidebar(disable = TRUE),
      body = dashboardBody(
        
        fluidRow(
          box(
            textInput("range1", "Plot 1's xMin, xMax, xValues, Y:", paste(f1_init, collapse = ", ")),
            width = 3, background = f1_back
          ),
          box(
            textInput("range2", "Plot 2's xMin, xMax, xValues, Y:", paste(f2_init, collapse = ", ")),
            width = 3, background = f2_back
          ),
          box(
            textInput("range3", "Plot 3's xMin, xMax, xValues, Y:", paste(f3_init, collapse = ", ")),
            width = 3, background = f3_back
          ),
          box(
            textInput("range4", "Plot 4's xMin, xMax, xValues, Y:", paste(f4_init, collapse = ", ")),
            width = 3, background = f4_back
          )
        ),
        
        fluidRow(
          box(
            textInput("formula1", "Loss Formula 1:", f1),
            width = 3, background = f1_back
          ),
          box(
            textInput("formula2", "Loss Formula 2:", f2),
            width = 3, background = f2_back
          ),
          box(
            textInput("formula3", "Loss Formula 3:", f3),
            width = 3, background = f3_back
          ),
          box(
            textInput("formula4", "Loss Formula 4:", f4),
            width = 3, background = f4_back
          )
        ),
        
        fluidRow(
          box(plotOutput("plot1", height = max_height), width = 3, background = f1_back),
          box(plotOutput("plot2", height = max_height), width = 3, background = f2_back),
          box(plotOutput("plot3", height = max_height), width = 3, background = f3_back),
          box(plotOutput("plot4", height = max_height), width = 3, background = f4_back)
        )
        
      )
      
    )
    
    server <- function(input, output) {
      
      getname <- function(main, dx) {
        ifelse(dx == 0, paste0("Original ", main), ifelse(dx == 1, paste0(main, " Gradient"), ifelse(dx == 2, paste0(main, " Hessian"), paste0(main, "s ", dx, "th Derivative"))))
      }
      
      tryplot <- function(x, y, f, type = "o", main = "Plot", xlab = "x", ylab = "Loss", dx = 0) {
        if (dx > 0) {
          f <- Deriv(f, "x", nderiv = dx)
        }
        if (length(try(plot(x = x, y = try(f(x, y)), type = type, main = paste0(getname(main, dx), ": Success"), xlab = xlab, ylab = ylab, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5))) == 1) {plot(x = 0, y = 0, main = paste0(getname(main, dx), ": Constant/Error"), xlab = xlab, ylab = ylab, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)}
      }
      
      returnplot <- function(varname, ranges, id) {
        eval(parse(text = paste0("f <- function", varname)))
        x_plot <- c(ranges[1], ranges[1] + ((ranges[2] - ranges[1]) / ranges[3]) * 1:ranges[3])
        par(mfrow = c(3, 1))
        tryplot(x = x_plot, y = ranges[4], f = f, type = type, main = paste0("Loss f", id), xlab = "x", ylab = "Loss", dx = 0)
        tryplot(x = x_plot, y = ranges[4], f = f, type = type, main = paste0("Loss f", id), xlab = "x", ylab = "Loss", dx = 1)
        tryplot(x = x_plot, y = ranges[4], f = f, type = type, main = paste0("Loss f", id), xlab = "x", ylab = "Loss", dx = 2)
      }
      
      observeEvent({input$range1; input$formula1}, {
        withProgress(message = "Computing Loss Function 1", detail = "Preparing data...", value = 0, {
          output$plot1 <- renderPlot({returnplot(input$formula1, as.numeric(strsplit(input$range1, ", ")[[1]]), 1)})
        })
      })
      
      observeEvent({input$range2; input$formula2}, {
        withProgress(message = "Computing Loss Function 2", detail = "Preparing data...", value = 0, {
          output$plot2 <- renderPlot({returnplot(input$formula2, as.numeric(strsplit(input$range2, ", ")[[1]]), 2)})
        })
      })
      
      observeEvent({input$range3; input$formula3}, {
        withProgress(message = "Computing Loss Function 3", detail = "Preparing data...", value = 0, {
          output$plot3 <- renderPlot({returnplot(input$formula3, as.numeric(strsplit(input$range3, ", ")[[1]]), 3)})
        })
      })
      
      observeEvent({input$range4; input$formula4}, {
        withProgress(message = "Computing Loss Function 4", detail = "Preparing data...", value = 0, {
          output$plot4 <- renderPlot({returnplot(input$formula4, as.numeric(strsplit(input$range4, ", ")[[1]]), 4)})
        })
      })
      
      
    }
    
    shinyApp(ui, server)
    
  }
  
}

