library(dplyr)
library(ggplot2)
library(Hmisc)

linear_curve_2_ponts <- function(...,
                                 title,
                                 color="black",
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
    
    gg <- gg + geom_abline(intercept = intercept, slope = slope, color = color)
  }
  
  if (!pointsHidden) {
    gg <- gg + geom_point(data = points_df, aes(x = x, y = y), color = "black", size = 3)    
  }
  
  if (!lineCoordinateHidden) {
    gg <- gg + geom_segment(data = segments_df, aes(x = x, y = y, xend = xend, yend = yend), linetype = "dotted", color = "black")
  }
  
  gg <- gg +
    xlim(minX, maxX) + 
    ylim(minY, maxY) + 
    labs(title = title, x = xlabel, y = ylabel) +
    theme_classic()
  
  return(gg)
}


linear_curve_2_characteristic <- function(...,
                                          title,
                                          color="black",
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
    
    gg <- gg + geom_abline(intercept = intercept, slope = slope, color = color)
  }
  
  gg <- gg +
    xlim(minX, maxX) + 
    ylim(minY, maxY) + 
    labs(title = title, x = xlabel, y = ylabel) +
    theme_classic()
  
  return(gg)
}

find_intersection <- function(p1, p2, gg) {
  slope1 <- (p1[4] - p1[2]) / (p1[3] - p1[1])
  intercept1 <- p1[2] - slope1 * p1[1]
  
  slope2 <- (p2[4] - p2[2]) / (p2[3] - p2[1])
  intercept2 <- p2[2] - slope2 * p2[1]
  
  x <- (intercept2 - intercept1) / (slope1 - slope2)
  y <- slope1 * x + intercept1
  
  points_df <- data.frame()
  segments_df <- data.frame()
  points_df <- rbind(points_df, data.frame(x = x, y = y))
  segments_df <- rbind(segments_df, data.frame(x = c(x, x, 0), y = c(y, 0, y), xend = x, yend = y))
  
  gg <- gg +
    geom_point(data = points_df, aes(x = x, y = y), color = "black", size = 3) +
    geom_segment(data = segments_df, aes(x = x, y = y, xend = xend, yend = yend), linetype = "dotted", color = "black")
  
  
  return (gg)
}

curve_N_ponts <- function(...,
                          title = NULL,
                          xmax = 10,
                          ymax = 10,
                          x,
                          linecol,
                          xlabel = "Q - quantity",
                          ylabel = "P - price") {
  
  curve <- list(...)
  ncurve <- length(curve)
  p <- ggplot(mapping = aes(x = x, y = y))
  
  for(i in 1:length(curve)) {
    p <- p + geom_line(data = data.frame(curve[[i]]), color = linecol[i], linewidth = 1, linetype = 1)
  }
  
  p <- p +
    labs(x = xlabel, y = ylabel, title = title) +
    theme_classic()+
    coord_equal()
  
  return(p)
}

curve_intersect <- function(curve1, curve2, gg) {
  curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
  curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)
  
  point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x), c(min(curve1$x), max(curve1$x)))$root
  
  point_y <- curve2_f(point_x)
  intersections <- bind_rows(list(x = point_x, y = point_y))
  
  gg <- gg +
    geom_segment(data = intersections, aes(x = point_x, y = 0, xend = point_x, yend = point_y), lty = "dotted") +
    geom_segment(data = intersections, aes(x = point_x, y = point_y, xend = 0, yend = point_y), lty = "dotted") +
    geom_point(data = intersections, size = 3)
  
  return(gg)
}