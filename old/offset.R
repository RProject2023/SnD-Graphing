library(ggplot2)
library(Hmisc)

# Variables:
#   ...: curves
#   offset: how much the graph needs to be moved (either a specific value or a percentage, 0 by default)
#   is_percentage: FALSE if offset represents exact value, TRUE if it represents a percentage (FALSE by default)
#   title: title of the displayed graph
#   colors: colors of the curves
#   colors2: colors of the moved curves
#   minX: minimum value of the x-axis coordinate
#   maxX: maximum value of the x-axis coordinate
#   xlabel: label of the x-axis
#   ylabel: label of the y-axis

move_curves_along_x_axis <- function(...,
                                    offset = 0,
                                    is_percentage = FALSE,
                                    title = "Move along x-axis",
                                    colors = "blue",
                                    colors2 = "red",
                                    minX=0,
                                    maxX=20,
                                    xlabel = "x",
                                    ylabel = "y" ){
  
  curves <- list(...)
  
  gg <- ggplot(mapping = aes(x = x, y = y)) + xlim(minX, maxX)
  
  for (i in seq_along(curves)) {
    data = data.frame(curves[[i]])
    
    gg <- gg + geom_line(data = data, color = colors[i], linewidth = 1, linetype = 1)
    
    if (is_percentage) {
      data$x <- data$x + diff(range(data$x)) * (offset / 100)
    } else {
      data$x <- data$x + offset
    }
    
    gg <- gg + geom_line(data = data, color = colors2[i], linewidth = 1, linetype = 1)
  }
  
  gg <- gg +
    labs(title = title, x = xlabel, y = ylabel)
  
  return(gg)
}

# Variables:
#   plot: graph which needs to be moved along the x axis
#   offset: how much the graph needs to be moved (either a specific value or a percentage, 0 by default)
#   is_percentage: FALSE if offset represents exact value, TRUE if it represents a percentage (FALSE by default)

move_graph_along_x_axis <- function(plot, offset = 0, is_percentage=FALSE) {
  
  layers <- plot$layers
  
  move_layer <- lapply(layers, function(layer){
    try(
      if (is_percentage) {
        layer$data$x <- layer$data$x + diff(range(layer$data$x)) * (offset / 100)
      } else {
        layer$data$x <- layer$data$x + offset
      })
    return(layer)
  })
  
  plot2 <- plot
  plot2$layers <- move_layer
  
  return(plot2)
}
