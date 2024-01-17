library(ggplot2)
# Variables:
#   ... : one or more input functions
#   title: title of the displayed graph
#   xlabel: label of the x-axis
#   ylabel: label of the y-axis
#   color: colors of the curves (in form of a list)
#   label: curve label (in form of a list)
#   x_coordinates: list of x coordinates for points
#   y_coordinates: list of y coordinates for points
#   pointColor: colors for the points (in form of a list)
#   showInfo: is the point information shown
#   pointLabel: point label (in form of a list)
#   axisValues: list of values to show on the x-axis
#   minX: minimum value of the x-axis coordinate
#   maxX: maximum value of the x-axis coordinate
#   minY: minimum value of the y-axis coordinate
#   maxY: maximum value of the y-axis coordinate


curve_by_function <- function(...,
                              title="", 
                              xlabel="Q - quantity",
                              ylabel="P - price",
                              color="blue",       #curve color
                              label="",         #curve label
                              x_coordinates=NULL, #x-coordinates for points to draw
                              y_coordinates=NULL, #y-coordinates for points to draw
                              pointColor="blue",
                              showInfo=TRUE,   #show point values on x&y axis
                              pointLabel="",    #label for point
                              axisValues=NULL,    #show values on x-axis
                              minX=-50, 
                              maxX=50, 
                              minY=-100,
                              maxY=2500 ){
  
  curves <- list(...)
  ncurves <- length(curves)
  
  if(ncurves > 1 && length(color) != ncurves) {
    color = rep("blue", ncurves)
  }
  
  
  gg <- ggplot(data.frame(x=c(minX, maxX)), aes(x = x)) +
    labs(title = title, x = xlabel, y = ylabel) +
    coord_cartesian(ylim = c(minY, maxY))
  
  #add curves
  for(i in 1:ncurves){
    gg <- gg + stat_function(fun = curves[[i]], colour = color[i]) 
  }
  
  #add curve names
  if(!is.null(label)){
    curve_names <- data.frame(
      x = maxX,
      y = sapply(curves, function(f) f(maxX)),
      label = label
    )
    gg <- gg + geom_text(data=curve_names, aes(x=x, y=y, label=label), hjust=-1)#todo for curve names
  }
  
  #optional add x-axis, y-axis
  #gg <- gg + geom_vline(aes(xintercept = 0))+ #add y-axis where you want
  #      geom_hline(aes(yintercept = 0)) #add back in x axis 
  
  
  
  #add points 
  
  if(!is.null(x_coordinates)) {
    if (is.null(y_coordinates)) {
      points <- do.call(rbind, lapply(seq_along(x_coordinates), function(j) {
        data.frame(point_x = x_coordinates[j], point_y = sapply(curves, function(f) f(x_coordinates[j])), group = as.factor(j))
      }))
      y_coordinates = points$point_y
    } else {
      points <- data.frame(point_x=x_coordinates, point_y=y_coordinates)
    }
    gg <- gg + geom_point(data=points, aes(x=point_x, y=point_y), color=pointColor, size = 3)
    
    if(showInfo) {
      gg <- gg + geom_segment(data=points, aes(x=point_x, y=minY,  xend=point_x, yend=point_y), linetype="dashed") +
        geom_segment(data=points, aes(x=minX, y=point_y, xend=point_x, yend=point_y), linetype="dashed") +
        geom_text(data=points, aes(x=point_x, y=point_y, label=pointLabel), hjust=-1, vjust=1) 
    }
  }
  
  
  #add numbers to x-axis
  x_limits <- c(minX, maxX)
  
  if(showInfo){ axisValues <- append(axisValues, x_coordinates)}
  
  y_axisValues <- y_coordinates
  
  if(!is.null(axisValues)) {
    gg <- gg + scale_x_continuous(breaks = c(pretty(x_limits), axisValues))
    if(!is.null(y_axisValues)) {
      gg <- gg + scale_y_continuous(breaks = y_axisValues)
    }
  } else {
    gg <- gg + scale_x_continuous(limits = x_limits)
  }
  
  
  return(gg)
}