library(dplyr)
library(ggplot2)
library(Hmisc)

# Variables:
#   ...: list of points in the form p = (x1, y1, x2, y2)
#   title: title of the displayed graph
#   color: colors of the curves
#   minX: minimum value of the x-axis coordinate
#   maxX: maximum value of the x-axis coordinate
#   minY: minimum value of the y-axis coordinate
#   maxY: maximum value of the y-axis coordinate
#   xlabel: label of the x-axis
#   ylabel: label of the y-axis
#   pointsHidden: are the points hidden
#   lineCoordinateHidden: are the points marked on the coordinates

linear_curve_2_ponts <- function(...,
                                 title,
                                 color,
                                 minX=0,
                                 maxX=10,
                                 minY=0,
                                 maxY=10,
                                 xlabel="Q - quantity",
                                 ylabel="P - price",
                                 pointsHidden=TRUE,
                                 lineCoordinateHidden=TRUE) {
  
  gg <- ggplot(data.frame(x = c(minX, maxX), y = c(minY, minY)), aes(x = x, y = y))
  points_df <- data.frame()
  segments_df <- data.frame()
  
  for(i in seq_along(list(...))) {
    points <- unlist(list(...)[[i]])
    
    x1 <- points[1]
    y1 <- points[2]
    x2 <- points[3]
    y2 <- points[4]
    
    x_axis <- c(x1, x2)
    y_axis <- c(y1, y2)
    points_df <- rbind(points_df, data.frame(x = x_axis, y = y_axis))
    segments_df <- rbind(segments_df, data.frame(x = c(x1, x1, minX), y = c(y1, minY, y1), xend = x1, yend = y1))
    segments_df <- rbind(segments_df, data.frame(x = c(x2, x2, minX), y = c(y2, minY, y2), xend = x2, yend = y2))
    
    slope <- (y2 - y1) / (x2 - x1)
    intercept <- y1 - slope * x1
    
    gg <- gg + geom_abline(intercept = intercept, slope = slope, color = color[i])
  }
  
  if (!pointsHidden) {
    gg <- gg + geom_point(data = points_df, aes(x = x, y = y), color = "black", size = 3)    
  }
  
  if (!lineCoordinateHidden) {
    gg <- gg + geom_segment(data = segments_df, aes(x = x, y = y, xend = xend, yend = yend), linetype = "dotted", color = "black") +
      scale_x_continuous(expand = c(minX, minX), limits = c(minX, maxX), breaks = round(points_df$x, 1)) +
      scale_y_continuous(expand = c(minY, minY), limits = c(minY, maxY), breaks = round(points_df$y, 1))
  } else {
    gg <- gg +
      xlim(minX, maxX) + 
      ylim(minY, maxY)
  }
  
  gg <- gg +
    labs(title = title, x = xlabel, y = ylabel) +
    theme_classic()
  
  return(gg)
}

# Variables:
#   ...: list of characteristic in the form c = (slope, intercept)
#   title: title of the displayed graph
#   color: colors of the curves
#   minX: minimum value of the x-axis coordinate
#   maxX: maximum value of the x-axis coordinate
#   minY: minimum value of the y-axis coordinate
#   maxY: maximum value of the y-axis coordinate
#   xlabel: label of the x-axis
#   ylabel: label of the y-axis

linear_curve_characteristic <- function(...,
                                          title,
                                          color,
                                          minX=0,
                                          maxX=10,
                                          minY=0,
                                          maxY=10,
                                          xlabel="Q - quantity",
                                          ylabel="P - price") {
  
  gg <- ggplot(data.frame(x = c(minX, maxX), y = c(minY, minY)), aes(x = x, y = y))
  points_df <- data.frame()
  segments_df <- data.frame()
  
  for(i in seq_along(list(...))) {
    characteristic <- unlist(list(...)[[i]])
    
    slope <- characteristic[1]
    intercept <- characteristic[2]
    
    gg <- gg + geom_abline(intercept = intercept, slope = slope, color = color[i])
  }
  
  gg <- gg +
    xlim(minX, maxX) + 
    ylim(minY, maxY) + 
    labs(title = title, x = xlabel, y = ylabel) +
    theme_classic()
  
  return(gg)
}

# Variables:
#   ...: list of curves in the form curve <- bezier(x = c(x1, x2, x3...), y = c(y1, y2, y3...)) %>% as.data.frame()
#   title: title of the displayed graph
#   color: colors of the curves
#   xlabel: label of the x-axis
#   ylabel: label of the y-axis

curve_N_ponts <- function(...,
                          title,
                          color,
                          xlabel = "Q - quantity",
                          ylabel = "P - price") {
  
  curve <- list(...)
  ncurve <- length(curve)
  gg <- ggplot(mapping = aes(x = x, y = y))
  
  for(i in 1:length(curve)) {
    gg <- gg + geom_line(data = data.frame(curve[[i]]), color = color[i], linewidth = 1, linetype = 1)
  }
  
  gg <- gg +
    labs(x = xlabel, y = ylabel, title = title) +
    theme_classic()
  
  return(gg)
}

# Variables:
#   curve1: first curve
#   curve2: second curve
#   gg: graph on which the intersection point of curve 1 and curve 2 will be plotted

curve_intersect <- function(curve1, curve2, gg) {
  curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
  curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)
  
  point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x), c(min(curve1$x), max(curve1$x)))$root
  
  point_y <- curve2_f(point_x)
  intersections <- bind_rows(list(x = point_x, y = point_y))
  
  gg <- gg +
    geom_segment(data = intersections, aes(x = point_x, y = 0, xend = point_x, yend = point_y), lty = "dotted") +
    geom_segment(data = intersections, aes(x = point_x, y = point_y, xend = 0, yend = point_y), lty = "dotted") +
    geom_point(data = intersections, size = 3) +
    geom_text(data = intersections, aes(x = point_x, y = point_y,  label = sprintf("(%0.1f, %0.1f)", point_x, point_y), hjust = -0.3))
  
  
  return(gg)
}