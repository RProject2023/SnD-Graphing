# Variables:
#   ...: curves of Production-Possibility Frontier
#   title: title of the displayed graph
#   colors: colors of the curves
#   x: points on the Production Possibility Frontier
#   labels: labels of points on the Production Possibility Frontier
#   xlabel: label of the x-axis
#   ylabel: label of the y-axis

ppf <- function(...,
                title = "Production-possibility frontier",
                colors,
                x,
                labels,
                xlabel = "Product A",
                ylabel = "Product B") {
  
  curves <- list(...)
  
  gg <- ggplot(mapping = aes(x = x, y = y))
  
  for (i in seq_along(curves)) {
    gg <- gg + geom_line(data = data.frame(curves[[i]]), color = colors[i], linewidth = 1, linetype = 1)
  }
  
  if(!missing(x)){
    
    intersections <- data.frame()
    
    for (j in seq_along(curves)) {
      x_values <- curves[[j]]$x
      y_values <- curves[[j]]$y
      
      interp_function <- splinefun(x_values, y_values)
      
      for (i in seq_along(x)) {
        point <- data.frame(x = x[i], y = interp_function(x[i]))
        intersections <- rbind(intersections, point)
      }
    }
    
    gg <- gg +  geom_segment(data = intersections, aes(x = x, y = 0, xend = x, yend = y), linetype = "dotted") +
      geom_segment(data = intersections, aes(x = 0, y = y, xend = x, yend = y), linetype = "dotted")  +
      geom_point(data = intersections, size = 3) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curves)) + 1), breaks = round(intersections$x, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curves)) + 1), breaks = round(intersections$y, 1)) +
      geom_text(data = intersections, aes(x = x, y = y, label = labels, vjust = -0.5, hjust = -0.2))
    
  } else {
    gg <- gg + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curves)) + 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curves)) + 1))
  }
  
  gg <- gg +
    labs(x = xlabel, y = ylabel, title = title) +
    theme_classic()
  
  return(gg)
}
