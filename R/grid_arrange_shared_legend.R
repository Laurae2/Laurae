#' ggplot multiple plot per page
#'
#' This function allows to plot multiple ggplot plots in the same page while keepin the same legend.
#' 
#' @param ... Type: ggplot. Input any ggplot you want.
#' @param ncol Type: integer. The number of columns in your final page. Defaults to \code{"length(list(...))"}.
#' @param nrow Type: itneger The number of rows in your final page. Defaults to \code{1}.
#' @param position Type: character. Where to put the legend. Defaults to \code{c("bottom", "right")}
#' 
#' @return The requested ggplots in the same page.
#' 
#' @examples
#' \dontrun{
#' dataset(mtcars)
#' p1 <- ggplot(mtcars, aes(x = mpg, y = as.factor(cyl), fill = vs)) + geom_point()
#' p2 <- ggplot(mtcars, aes(x = mpg, y = as.factor(cyl), fill = vs)) + geom_boxplot()
#' grid_arrange_shared_legend(p1, p2)
#' }
#' 
#' @export

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) {x$name}) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}