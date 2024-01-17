#' @title Indifference curve
#'
#' @param curve: Curve to display. This will override the sample curve.
#' @param x: X-axis values where to create intersections.
#' @param labels: If 'x' is specified, these are the labels for the intersection points.
#' @param linecolor: Line color of the curve.
#' @param title: Name of the plot.
#' @param xlabel: Name of the X-axis.
#' @param ylabel: Name of the Y-axis.
#' 
#' @import ggplot2 Hmisc

library(ggplot2)
library(Hmisc)

indifference_curve <- function(curve,
                               x,
                               labels,
                               linecolor = "black",
                               title = "Indifference curve",
                               xlabel = "X",
                               ylabel = "Y") {
  
  if(missing(curve)) {
    # Example indifference curve
    xmax <- 10
    ymax <- 10
    curve <- data.frame(bezier(x = c(1, 0.2*xmax, xmax), y = c(ymax, 0.2*ymax, 1)))
  }
  
  gg <- ggplot(mapping = aes(x = x, y = y))
  gg <- gg + geom_line(data = data.frame(curve), color = linecolor, linewidth = 1, linetype = 1)
  
  if(!missing(x)) {
    
    if(missing(labels)) {
      labels <- LETTERS[seq_along(x)]
    }
    
    intersections <- data.frame()
    
    x_values <- curve$x
    y_values <- curve$y
    
    interp_function <- splinefun(x_values, y_values)
    
    for (i in seq_along(x)) {
      point <- data.frame(x = x[i], y = interp_function(x[i]))
      intersections <- rbind(intersections, point)
    }
    
    gg <- gg +  geom_segment(data = intersections, aes(x = x, y = 0, xend = x, yend = y), linetype = "dotted") +
      geom_segment(data = intersections, aes(x = 0, y = y, xend = x, yend = y), linetype = "dotted")  +
      geom_point(data = intersections, size = 3) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$x)) + 1), breaks = c(min(curve$x), max(curve$x), round(intersections$x, 1))) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$y)) + 1), breaks = c(min(curve$y), max(curve$y), round(intersections$y, 1))) +
      geom_text(data = intersections, aes(x = x, y = y, label = labels, vjust = -0.2, hjust = -0.6))
  
  } else {
    
    gg <- gg + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$x)) + 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$y)) + 1))
  
  }
  
  gg <- gg + labs(x = xlabel, y = ylabel, title = title) + theme_classic() + coord_equal()
  
  return(gg)
}
