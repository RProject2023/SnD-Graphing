#' @title Indifference curve
#' @description This function plots an indifference curve.
#'
#' @param x: Points on the x-axis along which the curve is drawn. At least three points in ascending order must be given.
#' @param y: Points on the y-axis along which the curve is drawn. At least three points in desending order must be given.
#' @param n_curves: Number of curves to display.
#' @param x_intersections: X-axis values where to create intersections.
#' @param labels: If 'x_intersections' is specified, these are the labels for the intersection points.
#' @param linecolor: Line color of the curve.
#' @param title: Name of the plot.
#' @param xlabel: Name of the X-axis.
#' @param ylabel: Name of the Y-axis.
#' 
#' @return A new graph with the indifference curve.

library(ggplot2)
library(Hmisc)

indifference_curve <- function(x,
                               y,
                               n_curves=1,
                               x_intersections,
                               labels,
                               linecolor = "black",
                               title = "Indifference curve",
                               xlabel = "X",
                               ylabel = "Y") {
  
  if(missing(x) || length(x) < 3) {
    stop("'x' must have at least three elements.")
  }
  
  if(missing(y) || length(y) < 3) {
    stop("'y' must have at least three elements.")
  }
  
  if(!(all(diff(x) > 0))) {
    stop("'x' should be in ascending order.") 
  }
  
  if(!(all(diff(y) < 0))) {
    stop("'y' should be in descending order.") 
  }
  
  curve <- data.frame(bezier(x = x, y = y))
  
  curves <- list()
  for(i in 0:(n_curves-1)) {
    curves[[i+1]] <- data.frame(curve) + i
  }
  
  gg <- ggplot(mapping = aes(x = x, y = y))
  for(i in 0:(n_curves-1)) {
    gg <- gg + geom_line(data = curves[[i+1]], color = linecolor, linewidth = 1, linetype = 1)
  }
  
  if(!missing(x_intersections)) {
    
    if(missing(labels)) {
      num_labels <- length(x_intersections) * n_curves
      labels <- LETTERS[seq_len(num_labels)]
    }
    
    if(length(labels) != length(x_intersections) * n_curves) {
      stop("Number of labels is incorrect.")
    }
    
    intersections <- data.frame()
    
    for (j in seq_along(curves)) {
      x_values <- curves[[j]]$x
      y_values <- curves[[j]]$y
      
      interp_function <- splinefun(x_values, y_values)
      
      for (i in seq_along(x_intersections)) {
        point <- data.frame(x = x_intersections[i], y = interp_function(x_intersections[i]))
        intersections <- rbind(intersections, point)
      }
    }
    
    gg <- gg +  geom_segment(data = intersections, aes(x = x, y = 0, xend = x, yend = y), linetype = "dotted") +
      geom_segment(data = intersections, aes(x = 0, y = y, xend = x, yend = y), linetype = "dotted")  +
      geom_point(data = intersections, size = 3) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$x)) + n_curves), breaks = round(intersections$x, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$y)) + n_curves), breaks = round(intersections$y, 1)) +
      geom_text(data = intersections, aes(x = x, y = y, label = labels, vjust = -0.2, hjust = -0.6))
  
  } else {
    
    gg <- gg + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$x)) + n_curves)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$y)) + n_curves))
  
  }
  
  gg <- gg + labs(x = xlabel, y = ylabel, title = title) + theme_classic() + coord_equal()
  
  return(gg)
}
