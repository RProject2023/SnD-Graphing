library(ggplot2)

#plot line graph based on points
draw_by_points <- function(x1, y1, x2, y2) {
  x_axis <- c(x1, x2)
  y_axis <- c(y1, y2)
  points <- data.frame(x_axis, y_axis)
  
  ggplot(points, aes(x_axis, y_axis, colour = "red")) +
    geom_point() + geom_line()
}

#plot line graph based on characteristics of graph
draw_by_characteristics <- function(slope, intercept) {
  
  draw_by_points(x1=-intercept/slope, y1=0, x2=0, y2=intercept)
  
}

draw_by_points(1, 2, 3, 4)
draw_by_characteristics(4, 12)
