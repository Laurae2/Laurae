library(data.table)
library(shiny)
library(miniUI)
library(ggplot2)
library(plotly)

# arguments <- commandArgs(trailingOnly = TRUE)
# file_where <- arguments[1]
# my_ip <- arguments[2]
# port <- as.numeric(arguments[3])
# browser <- as.logical(arguments[4])

if (file.exists(file_where)) {
  mini_data <- fread(file_where, stringsAsFactors = TRUE)
} else {
  mini_data <- NULL
}

metric_print <- NULL
metric_imp <- NULL

ui <- miniPage(
  gadgetTitleBar("Xgboard watcher", left = NULL, right = miniTitleBarButton("done", "Done", primary = TRUE)),
  miniTabstripPanel(
    miniTabPanel("Params", icon = icon("sliders"),
                 miniContentPanel(
                   checkboxInput("block", "Block Refresh", value = FALSE),
                   sliderInput("digits", "Digits", 1, 15, 7, sep = ""),
                   radioButtons("autoselect", "About metrics:", choices = c("Auto Select" = 1, "NO Auto Select" = 2)),
                   selectInput("chosen1", "Metric 1:", choices = "None", selected = "None"),
                   selectInput("chosen2", "Metric 2:", choices = "None", selected = "None"),
                   selectInput("palette", "Color Palette:", choices = c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"), selected = "Set1"),
                   actionButton("now", "Refresh Now", icon = icon("signal")),
                   actionButton("restarter", "Reload Xgboard", icon = icon("recycle")),
                   plotOutput("color_plot")
                 )
    ),
    miniTabPanel("Time", icon = icon("area-chart"),
                 miniContentPanel(
                   plotlyOutput("timing", height = "100%")
                 )
    ),
    miniTabPanel("Val 1", icon = icon("line-chart"),
                 miniContentPanel(
                   plotlyOutput("metric1", height = "100%")
                 )
    ),
    miniTabPanel("Val 2", icon = icon("line-chart"),
                 miniContentPanel(
                   plotlyOutput("metric2", height = "100%")
                 )
    ),
    miniTabPanel("Imp 1", icon = icon("bar-chart"),
                 miniContentPanel(
                   plotlyOutput("improve1", height = "100%")
                 )
    ),
    miniTabPanel("Imp 2", icon = icon("bar-chart"),
                 miniContentPanel(
                   plotlyOutput("improve2", height = "100%")
                 )
    )
  )
)

server <- function(input, output, session) {
  
  watchlist_agg <- function(data, what, named, bind = TRUE) {
    
    # Capitalize first letter: mean -> Mean, avg -> Avg, etc.
    new_fact_name <- paste0(toupper(substr(what, 1, 1)), substr(what, 2, nchar(what)))
    
    # Get "what" grouped by Iteration
    agg_data <- eval(parse(text = paste0("data[, list(Watchlist = '", new_fact_name, "', ", named, " = ", what, "(", named, ")), by = Iteration]")))
    
    # Turn into factor the watchlist
    agg_data[[2]] <- as.factor(agg_data[[2]])
    
    if (bind) {
      
      # Get names of watchlist
      temp_levels <- levels(data[[2]])
      
      # Return data
      data <- rbind(data, agg_data)
      
      # Reset levels and put new level last
      levels(data[[2]]) <- c(temp_levels, new_fact_name)
      
      return(data)
      
    } else {
      
      return(agg_data)
      
    }
    
  }
  
  renderEmpty <- function(msg) {
    return(ggplotly(ggplot() + annotate("text", x = 0, y = 0, label = msg) + theme_bw()))
  }
  
  renderTime <- function(data) {
    
    my_time <- data[, list(Time = sum(Time)), by = Iteration]
    my_plot <- ggplotly(ggplot(data = data,
                               aes_string(x = "Iteration", y = "Time", fill = "Watchlist")) +
                          geom_area(alpha = 0.6, stat = "identity") +
                          labs(title = paste0("Training Time [", sprintf("%.03f", sum(data[[3]]) / 1000), "s]"),
                               x = paste0("Iteration [", 1, "-", max(data[[1]]), "]"),
                               y = paste0("Time (ms) [avg = ", sprintf("%.03f", mean(data[[3]]) / 1000), "s/iter]")) +
                          geom_hline(yintercept = min(my_time[[2]]), color = "gray", linetype = "dashed") +
                          geom_vline(xintercept = which.min(my_time[[2]]) + 1, color = "gray", linetype = "dashed") +
                          geom_hline(yintercept = max(my_time[[2]]), color = "gray", linetype = "dashed") +
                          geom_vline(xintercept = which.max(my_time[[2]]) + 1, color = "gray", linetype = "dashed") +
                          scale_fill_manual(values = Laurae::brewer.pal_extended(length(levels(data$Watchlist)) + 1, input$palette)[1:length(levels(data$Watchlist))]) +
                          theme_bw())
    my_plot <- layout(my_plot, hovermode = "FALSE")
    for (i in 1:(length(my_plot$x$data) - 4)) {
      my_plot$x$data[[i]]$text <- paste("Iteration: ", my_plot$x$data[[i]]$x, "<br>Time: ", sprintf("%.06f", my_plot$x$data[[i]]$y), " ms<br>Watchlist: ", my_plot$x$data[[i]]$name, sep = "")
    }
    return(my_plot)
    
  }
  
  renderMetric <- function(initial_data, agg_data, named, improvement = FALSE) {
    
    if (is.null(agg_data)) {
      
      # No aggregate data => only one watchlist
      my_plot <- ggplotly(ggplot(data = initial_data,
                                 aes_string(x = "Iteration", y = named)) +
                            geom_point(alpha = 0.2) +
                            geom_line(alpha = 0.2, size = 1) +
                            labs(title = paste0(named, ifelse(improvement == TRUE, " Imp+", ""), " [", paste(sprintf(paste0("%.0", input$digits, "f"), range(initial_data[[named]])), collapse = ", "), "]"),
                                 x = paste0("Iteration [", which.min(initial_data[[3]])[1], "-", which.max(initial_data[[3]])[1], "]"),
                                 y = paste0(named, ifelse(improvement == TRUE, " Imp+", ""), " [", sprintf(paste0("%.0", input$digits, "f"), min(initial_data[[3]])), ", ", sprintf(paste0("%.0", input$digits, "f"), max(initial_data[[3]])), "]")) +
                            geom_hline(yintercept = min(initial_data[[3]]), color = "gray", linetype = "dashed") +
                            geom_vline(xintercept = which.min(initial_data[[3]])[1], color = "gray", linetype = "dashed") +
                            geom_hline(yintercept = max(initial_data[[3]]), color = "gray", linetype = "dashed") +
                            geom_vline(xintercept = which.max(initial_data[[3]])[1], color = "gray", linetype = "dashed") +
                            theme_bw())
      my_plot <- layout(my_plot, hovermode = "FALSE")
      my_plot$x$data[[1]]$text <- paste("Iteration: ", my_plot$x$data[[1]]$x, "<br>", named, ": ", sprintf(paste0("%.0", input$digits, "f"), my_plot$x$data[[1]]$y), "<br>Watchlist: ", my_plot$x$data[[1]]$name, sep = "")
      return(my_plot)
      
    } else {
      
      # Has aggregate data => multiple watchlist
      
      # Get names of watchlist
      temp_levels <- levels(initial_data[[2]])
      
      # Return data
      initial_data <- rbind(initial_data, agg_data)
      
      # Reset levels and put new level last
      levels(initial_data[[2]]) <- c(temp_levels, levels(agg_data[[2]]))
      
      # Return plot
      my_plot <- ggplotly(ggplot(data = initial_data,
                                 aes_string(x = "Iteration", y = named, color = "Watchlist")) +
                            geom_point(alpha = 0.3) +
                            geom_line(alpha = 0.3, size = 1) +
                            labs(title = paste0(gsub("Improve", " Imp+", named), " [", paste(sprintf(paste0("%.0", input$digits, "f"), range(agg_data[[3]])), collapse = ", "), "]"),
                                 x = paste0("Iteration [", which.min(agg_data[[3]])[1], "-", which.max(agg_data[[3]])[1], "]"),
                                 y = paste0(gsub("Improve", " Imp+", named), ifelse(improvement == TRUE, " Imp+", ""), " [", sprintf(paste0("%.0", input$digits, "f"), min(agg_data[[3]])), ", ", sprintf(paste0("%.0", input$digits, "f"), max(agg_data[[3]])), "]")) +
                            geom_hline(yintercept = min(agg_data[[3]]), color = "gray", linetype = "dashed") +
                            geom_vline(xintercept = which.min(agg_data[[3]])[1], color = "gray", linetype = "dashed") +
                            geom_hline(yintercept = max(agg_data[[3]]), color = "gray", linetype = "dashed") +
                            geom_vline(xintercept = which.max(agg_data[[3]])[1], color = "gray", linetype = "dashed") +
                            scale_colour_manual(values = Laurae::brewer.pal_extended(length(levels(initial_data$Watchlist)), input$palette)) +
                            theme_bw())
      my_plot <- layout(my_plot, hovermode = "FALSE")
      for (i in 1:(length(my_plot$x$data) - 4)) {
        my_plot$x$data[[i]]$text <- paste("Iteration: ", my_plot$x$data[[i]]$x, "<br>", gsub("Improve", " Imp+", named), ": ", sprintf(paste0("%.0", input$digits, "f"), my_plot$x$data[[i]]$y), "<br>Watchlist: ", my_plot$x$data[[i]]$name, sep = "")
      }
      my_plot$x$data[[length(levels(initial_data[[2]]))]]$line$color <- gsub("\\w\\.\\w", "0.8", my_plot$x$data[[length(levels(initial_data[[2]]))]]$line$color)
      return(my_plot)
      
    }
    
  }
  
  refresher <- function() {
    
    if (!is.null(mini_data)) {
      
      if (nrow(mini_data) > 0) {
        
        # Metric
        if (nrow(mini_data) > 1) {
          
          smaller_data0 <- mini_data[(length(levels(mini_data[[2]])) + 1):nrow(mini_data), c(1, 2, 4), with = FALSE]
          
          output$timing <- renderPlotly({
            renderTime(smaller_data0)
          })
          
        }
        
        if (length(metric_print) == 2) {
          
          # Only 2 metric
          smaller_data11 <- mini_data[, c(1, 2, metric_print[1]), with = FALSE]
          smaller_data12 <- mini_data[, c(1, 2, metric_imp[1]), with = FALSE]
          smaller_data21 <- mini_data[, c(1, 2, metric_print[2]), with = FALSE]
          smaller_data22 <- mini_data[, c(1, 2, metric_imp[2]), with = FALSE]
          
          output$metric1 <- renderPlotly({
            renderMetric(initial_data = smaller_data11, agg_data = if (length(levels(smaller_data11[[2]])) == 1) {NULL} else {watchlist_agg(smaller_data11, "mean", colnames(mini_data)[metric_print[1]], bind = FALSE)}, named = colnames(mini_data)[metric_print[1]])
          })
          
          output$improve1 <- renderPlotly({
            renderMetric(initial_data = smaller_data12, agg_data = if (length(levels(smaller_data12[[2]])) == 1) {NULL} else {watchlist_agg(smaller_data12, "mean", colnames(mini_data)[metric_imp[1]], bind = FALSE)}, named = colnames(mini_data)[metric_imp[1]])
          })
          
          output$metric2 <- renderPlotly({
            renderMetric(initial_data = smaller_data21, agg_data = if (length(levels(smaller_data21[[2]])) == 1) {NULL} else {watchlist_agg(smaller_data21, "mean", colnames(mini_data)[metric_print[2]], bind = FALSE)}, named = colnames(mini_data)[metric_print[2]])
          })
          
          output$improve2 <- renderPlotly({
            renderMetric(initial_data = smaller_data22, agg_data = if (length(levels(smaller_data22[[2]])) == 1) {NULL} else {watchlist_agg(smaller_data22, "mean", colnames(mini_data)[metric_imp[2]], bind = FALSE)}, named = colnames(mini_data)[metric_imp[2]])
          })
          
        } else if (length(metric_print) == 1) {
          
          # Only 1 metric
          smaller_data11 <- mini_data[, c(1, 2, metric_print[1]), with = FALSE]
          smaller_data12 <- mini_data[, c(1, 2, metric_imp[1]), with = FALSE]
          
          output$metric1 <- renderPlotly({
            renderMetric(initial_data = smaller_data11, agg_data = if (length(levels(smaller_data11[[2]])) == 1) {NULL} else {watchlist_agg(smaller_data11, "mean", colnames(mini_data)[metric_print[1]], bind = FALSE)}, named = colnames(mini_data)[metric_print[1]])
          })
          
          output$improve1 <- renderPlotly({
            renderMetric(initial_data = smaller_data12, agg_data = if (length(levels(smaller_data12[[2]])) == 1) {NULL} else {watchlist_agg(smaller_data12, "mean", colnames(mini_data)[metric_imp[1]], bind = FALSE)}, named = colnames(mini_data)[metric_imp[1]])
          })
          
          output$metric2 <- renderPlotly({
            renderEmpty("No metric recorded!")
          })
          
          output$improve2 <- renderPlotly({
            renderEmpty("No metric recorded!")
          })
          
        } else {
          
          # No metric to record
          output$timing <- renderPlotly({
            renderEmpty("No metric recorded!")
          })
          
          output$metric1 <- renderPlotly({
            renderEmpty("No metric recorded!")
          })
          
          output$improve1 <- renderPlotly({
            renderEmpty("No metric recorded!")
          })
          
          output$metric2 <- renderPlotly({
            renderEmpty("No metric recorded!")
          })
          
          output$improve2 <- renderPlotly({
            renderEmpty("No metric recorded!")
          })
          
        }
        
      } else {
        
        # No data recorded
        output$timing <- renderPlotly({
          renderEmpty("No data recorded!")
        })
        
        output$metric1 <- renderPlotly({
          renderEmpty("No data recorded!")
        })
        
        output$improve1 <- renderPlotly({
          renderEmpty("No data recorded!")
        })
        
        output$metric2 <- renderPlotly({
          renderEmpty("No data recorded!")
        })
        
        output$improve2 <- renderPlotly({
          renderEmpty("No data recorded!")
        })
        
      }
      
    } else {
      
      output$timing <- renderPlotly({
        renderEmpty(paste0('Non existing log file yet: "', file_where, '"'))
      })
      
      output$metric1 <- renderPlotly({
        renderEmpty(paste0('Non existing log file yet: "', file_where, '"'))
      })
      
      output$improve1 <- renderPlotly({
        renderEmpty(paste0('Non existing log file yet: "', file_where, '"'))
      })
      
      output$metric2 <- renderPlotly({
        renderEmpty(paste0('Non existing log file yet: "', file_where, '"'))
      })
      
      output$improve2 <- renderPlotly({
        renderEmpty(paste0('Non existing log file yet: "', file_where, '"'))
      })
      
    }
    
    return(NULL)
    
  }
  
  selector <- function() {
    
    if (!is.null(mini_data)) {
      
      if (ncol(mini_data) > 4) {
        updateSelectInput(session, "chosen1", choices = c("None", colnames(mini_data)[5:(4 + ((ncol(mini_data) - 4) / 2))]))
        updateSelectInput(session, "chosen2", choices = c("None", colnames(mini_data)[5:(4 + ((ncol(mini_data) - 4) / 2))]))
      } else {
        updateSelectInput(session, "chosen1", choices = "None", selected = "None")
        updateSelectInput(session, "chosen2", choices = "None", selected = "None")
      }
      
      if (length(metric_print) == 2) {
        
        updateSelectInput(session, "chosen1", selected = colnames(mini_data)[metric_print[1]])
        updateSelectInput(session, "chosen2", selected = colnames(mini_data)[metric_print[2]])
        
      } else if (length(metric_print) == 1) {
        
        updateSelectInput(session, "chosen1", selected = colnames(mini_data)[metric_print[1]])
        updateSelectInput(session, "chosen2", selected = "None")
        
      } else {
        
        updateSelectInput(session, "chosen1", selected = "None")
        updateSelectInput(session, "chosen2", selected = "None")
        
      }
      
    } else {
      
      updateSelectInput(session, "chosen1", selected = "None")
      updateSelectInput(session, "chosen2", selected = "None")
      
    }
    
  }
  
  observeEvent(c(input$autoselect, input$chosen1, input$chosen2, input$chosen3), {
    
    if (input$autoselect == 2) {
      
      metric_print <<- which(colnames(mini_data) %in% c(input$chosen1, input$chosen2, input$chosen3))
      metric_imp <<- metric_print + ((ncol(mini_data) - 4) / 2)
      
    } else {
      
      if (is.null(mini_data)) {
        
        metric_print <<- NULL
        metric_imp <<- NULL
        
      } else {
        
        if (ncol(mini_data) > 4) {
          
          metric_print <<- 5:(4 + ((min(ncol(mini_data), 8) - 4) / 2))
          metric_imp <<- metric_print + ((ncol(mini_data) - 4) / 2)
          
        } else {
          
          metric_print <<- NULL
          metric_imp <<- NULL
          
        }
        
      }
      
    }
    
    isolate({
      selector()
    })
    
    refresher()
    
  })
  
  observeEvent(input$palette, {
    output$color_plot <- renderPlot({
      new_palette <- Laurae::brewer.pal_extended(10, input$palette)
      return(pie(rep(1, length(new_palette)), col = new_palette, main = paste0("Palette: '", input$palette, "'.")))
    })
  })
  
  refresher()
  
  poll_check <- function() {
    if (input$block) {
      ""
    } else if (file.exists(file_where)) {
      file.info(file_where)$mtime[1]
    } else {
      ""
    }
  }
  
  poll_value <- function() {
    if (file.exists(file_where)) {
      fread(file_where, stringsAsFactors = TRUE)
    } else {
      NULL
    }
  }
  
  poller <- reactivePoll(polling,
                         session,
                         checkFunc = poll_check,
                         valueFunc = poll_value)
  
  observe({
    if (file.exists(file_where)) {
      if (!input$block) {
        mini_data <<- poller()
        selector()
        refresher()
      }
    }
  })
  
  eventReactive(input$now, {
    if (file.exists(file_where)) {
      mini_data <<- fread(file_where, stringsAsFactors = TRUE)
      mini_data[[2]] <<- as.factor(mini_data[[2]])
      selector()
      refresher()
    } else {
      mini_data <<- NULL
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
  
  observeEvent(input$restarter, {
    session$reload()
  })
  
}

shinyApp(ui, server, options = list(host = my_ip, port = port, browser = browser))
