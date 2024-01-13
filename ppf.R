# Variables:
#   curve: curve of Production-Possibility Frontier
#   title: title of the displayed graph
#   color: colors of the curves
#   x: points on the Production Possibility Frontier
#   labels: labels of points on the Production Possibility Frontier
#   xlabel: label of the x-axis
#   ylabel: label of the y-axis

ppf <- function(curve,
                title = "Production-possibility frontier",
                color = "blue",
                x,
                labels,
                xlabel = "Product A",
                ylabel = "Product B") {
  
  gg <- ggplot(mapping = aes(x = x, y = y))
  
  gg <- gg + geom_line(data = data.frame(curve), color = color, linewidth = 1, linetype = 1)


  if(!missing(x)){

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
      scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1), breaks = round(intersections$x, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1), breaks = round(intersections$y, 1)) +
      geom_text(data = intersections, aes(x = x, y = y, label = labels, vjust = -0.5, hjust = -0.2))
    
  } else {
    gg <- gg + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1))
  }
  
  gg <- gg +
    labs(x = xlabel, y = ylabel, title = title) +
    theme_classic()
  
  return(gg)
}



