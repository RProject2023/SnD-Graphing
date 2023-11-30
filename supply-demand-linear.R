library(ggplot2)
library(patchwork)

linear_curve_2_ponts <- function(x1, y1, x2, y2, title, color="black", minX=0, maxX=10, minY=0, maxY=10) {
  x_axis <- c(x1, x2)
  y_axis <- c(y1, y2)
  points <- data.frame(x_axis, y_axis)
  slope <- (y2 - y1) / (x2 - x1)
  intercept <- y1 - slope * x1
  
  gg <- ggplot(points, aes(x_axis, y_axis)) +
    geom_point(color = color, size = 3) +
    geom_abline(intercept = intercept, slope = slope, color = color) +
    geom_segment(aes(x = x1, y = y1, xend = x1, yend = minY), linetype = "dashed", color = "black") +
    geom_segment(aes(x = x2, y = y2, xend = x2, yend = minY), linetype = "dashed", color = "black") +
    geom_segment(aes(x = x1, y = y1, xend = minX, yend = y1), linetype = "dashed", color = "black") +
    geom_segment(aes(x = x2, y = y2, xend = minX, yend = y2), linetype = "dashed", color = "black") +
    xlim(minX, maxX) +
    ylim(minY, maxY) +
    labs(title = title, x = "Q - quantity", y = "P - price")
  
  return(gg)
}

linear_curve_2_ponts(1, 2, 5, 7, "Supply", "blue") + supply(3, 10, 6, 7, "Demand", "red")

