ppf <- function(curve,
                xmax = 10,
                ymax = 10,
                x,
                labels,
                color = "blue",
                title = "Production-possibility frontier",
                xlabel = "Product A",
                ylabel = "Product B"){
  
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
    theme_classic()+
    coord_equal()
  
  return(gg)
}

curve_data <- bezier(x = c(0, 4, 7, 10), y = c(11, 9, 6, 0)) %>% as.data.frame()
ppf(curve = curve_data, xmax = 12, ymax = 12, x = c(2, 5, 8), labels = c("A", "B", "C"))

