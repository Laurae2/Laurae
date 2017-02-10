#' ggplot facet function with printed formula (non Plotly)
#'
#' This function prints linear formula used on each facet for ggplot. Refer to \code{stat_smooth} for description. Does not support Plotly.
#' 
#' See this gist: https://gist.github.com/kdauria/524eade46135f6348140
#' 
#' Extra edits were made to fit the needs of this package.
#' @param mapping ...
#' @param data ...
#' @param geom ...
#' @param position ...
#' @param ... ...
#' @param method ...
#' @param formula ...
#' @param se ...
#' @param n ...
#' @param span ...
#' @param fullrange ...
#' @param level ...
#' @param method.args ...
#' @param na.rm ...
#' @param show.legend ...
#' @param inherit.aes ...
#' @param xpos ...
#' @param ypos ...
#' 
#' @return See stat_smooth.
#' 
#' @export

stat_smooth_func <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = ggproto("StatSmooth",
                    Stat,
                    setup_params = function(data, params) {
                      # Figure out what type of smoothing to do: loess for small datasets,
                      # gam with a cubic regression basis for large data
                      # This is based on the size of the _largest_ group.
                      if (identical(params$method, "auto")) {
                        max_group <- max(table(data$group))
                        
                        if (max_group < 1000) {
                          params$method <- "loess"
                        } else {
                          params$method <- "gam"
                          params$formula <- y ~ s(x, bs = "cs")
                        }
                      }
                      if (identical(params$method, "gam")) {
                        params$method <- mgcv::gam
                      }
                      
                      params
                    },
                    compute_group = function(data, scales, method = "auto", formula = y~x,
                                             se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                             xseq = NULL, level = 0.95, method.args = list(),
                                             na.rm = FALSE, xpos = NULL, ypos = NULL) {
                      if (length(unique(data$x)) < 2) {
                        # Not enough data to perform fit
                        return(data.frame())
                      }
                      
                      if (is.null(data$weight)) data$weight <- 1
                      
                      if (is.null(xseq)) {
                        if (is.integer(data$x)) {
                          if (fullrange) {
                            xseq <- scales$x$dimension()
                          } else {
                            xseq <- sort(unique(data$x))
                          }
                        } else {
                          if (fullrange) {
                            range <- scales$x$dimension()
                          } else {
                            range <- range(data$x, na.rm = TRUE)
                          }
                          xseq <- seq(range[1], range[2], length.out = n)
                        }
                      }
                      # Special case span because it's the most commonly used model argument
                      if (identical(method, "loess")) {
                        method.args$span <- span
                      }
                      
                      if (is.character(method)) method <- match.fun(method)
                      
                      base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                      model <- do.call(method, c(base.args, method.args))
                      
                      m = model
                      
                      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                                       list(a = format(coef(m)[1], digits = 3), 
                                            b = format(coef(m)[2], digits = 3), 
                                            r2 = format(summary(m)$r.squared, digits = 3)))
                      func_string = as.character(as.expression(eq))
                      
                      if(is.null(xpos)) xpos = min(data$x)*0.9
                      if(is.null(ypos)) ypos = max(data$y)*0.9
                      data.frame(x=xpos, y=ypos, label=func_string)
                      
                    },
                    required_aes = c("x", "y")
    ),
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}

#' ggplot facet function with printed formula (Plotly)
#'
#' This function prints linear formula used on each facet for ggplot. Refer to \code{stat_smooth} for description. Supports Plotly. I do not know what exactly Plotly is doing to the geom_text, but it puts them... at the wrong places (cut text). You can ALWAYS hover the text if needed. I intentionally put the mutliplication coefficient 2nd to avoid reading issues (formula: y=mx+b).
#' 
#' See this gist: https://gist.github.com/kdauria/524eade46135f6348140
#' 
#' Extra edits were made to fit the needs of this package.
#' @param mapping ...
#' @param data ...
#' @param geom ...
#' @param position ...
#' @param ... ...
#' @param method ...
#' @param formula ...
#' @param se ...
#' @param n ...
#' @param span ...
#' @param fullrange ...
#' @param level ...
#' @param method.args ...
#' @param na.rm ...
#' @param show.legend ...
#' @param inherit.aes ...
#' @param xpos ...
#' @param ypos ...
#' 
#' @return See stat_smooth.
#' 
#' @export

stat_smooth_func.plotly <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = ggproto("StatSmooth",
                   Stat,
                   setup_params = function(data, params) {
                     # Figure out what type of smoothing to do: loess for small datasets,
                     # gam with a cubic regression basis for large data
                     # This is based on the size of the _largest_ group.
                     if (identical(params$method, "auto")) {
                       max_group <- max(table(data$group))
                       
                       if (max_group < 1000) {
                         params$method <- "loess"
                       } else {
                         params$method <- "gam"
                         params$formula <- y ~ s(x, bs = "cs")
                       }
                     }
                     if (identical(params$method, "gam")) {
                       params$method <- mgcv::gam
                     }
                     
                     params
                   },
                   compute_group = function(data, scales, method = "auto", formula = y~x,
                                            se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                            xseq = NULL, level = 0.95, method.args = list(),
                                            na.rm = FALSE, xpos = NULL, ypos = NULL) {
                     if (length(unique(data$x)) < 2) {
                       # Not enough data to perform fit
                       return(data.frame())
                     }
                     
                     if (is.null(data$weight)) data$weight <- 1
                     
                     if (is.null(xseq)) {
                       if (is.integer(data$x)) {
                         if (fullrange) {
                           xseq <- scales$x$dimension()
                         } else {
                           xseq <- sort(unique(data$x))
                         }
                       } else {
                         if (fullrange) {
                           range <- scales$x$dimension()
                         } else {
                           range <- range(data$x, na.rm = TRUE)
                         }
                         xseq <- seq(range[1], range[2], length.out = n)
                       }
                     }
                     # Special case span because it's the most commonly used model argument
                     if (identical(method, "loess")) {
                       method.args$span <- span
                     }
                     
                     if (is.character(method)) method <- match.fun(method)
                     
                     base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                     model <- do.call(method, c(base.args, method.args))
                     
                     m = model
                     
                     a = format(coef(m)[1], digits = 3)
                     b = format(coef(m)[2], digits = 3)
                     r2 = sprintf("%.03f", summary(m)$r.squared)
                     func_string <- paste0("b=", a, "<br>m=", b, "<br>r^2=", r2)
                     #func_string <- paste0("y = ", a, " + ", b, "x - r^2 = ", r2)
                     
                     if(is.null(xpos)) xpos = min(data$x)*0.9
                     if(is.null(ypos)) ypos = max(data$y)*0.9
                     data.frame(x=xpos, y=ypos, label=func_string)
                     
                   },
                   required_aes = c("x", "y")
    ),
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}
