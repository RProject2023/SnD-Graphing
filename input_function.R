library(ggplot2)

curve_by_function <- function(...,
                              x=NULL,#points to draw
                              y=NULL, 
                              showPointValues=TRUE,
                              pointNames,
                              title, 
                              colors="blue",
                              minX=-50, 
                              maxX=50, 
                              minY=-10,
                              maxY=30,
                              xlabel="Q - quantity",
                              ylabel="P - price",
                              curveName = NULL,
                              x_axisValues = NULL) {

  curves <- list(...)
  ncurves <- length(curves)
  print(ncurves)
  
  
  gg <- ggplot(data.frame(x=c(minX, maxX)), aes(x = x)) +
        labs(title, x = xlabel, y = ylabel)
  
  #add curves
  for(i in 1:ncurves){
    gg <- gg + stat_function(fun = curves[[i]], colour = colors[i]) 
  }
  
  #add curve names
  if(!is.null(curveName)){
    curve_names <- data.frame(
      x = maxX,
      y = sapply(curves, function(f) f(maxX)),
      label = curveName
    )
    print(curve_names)
    gg <- gg + geom_text(data=curve_names, aes(x=x, y=y, label=label), hjust=-1)#todo for curve names
  }
  
  #optional add x-axis, y-axis
  #gg <- gg + geom_vline(aes(xintercept = 0))+ #add y-axis where you want
  #      geom_hline(aes(yintercept = 0)) #add back in x axis 
  
  
  
  #add points 
  if (is.null(y)) {
    points <- do.call(rbind, lapply(seq_along(x), function(j) {
      data.frame(x = x[j], y = sapply(curves, function(f) f(x[j])), group = as.factor(j))
    }))
    y = points$y
    print(y)
  } else {#todo if y is defined
    points <- data.frame(x=x, y=y)
  }
  
  gg <- gg + geom_point(data=points, aes(x=x, y=y), color="blue", size = 3) +
            geom_text(data=points, aes(x=x, y=y, label=pointNames), hjust=-1, vjust=1) +
            geom_segment(data=points, aes(x=x, y=minY,  xend=x, yend=y), linetype="dashed") +
            geom_segment(data=points, aes(x=minX, y=y, xend=x, yend=y), linetype="dashed") 
            
  
  
  
  #add numbers to x-axis
  x_limits <- c(minX, maxX)
  
  if(showPointValues){ x_axisValues <- append(x_axisValues, x)}
  
  y_axisValues <- y
    
  if(!is.null(x_axisValues)) {
    gg <- gg + scale_x_continuous(breaks = c(pretty(x_limits), x_axisValues)) +
          scale_y_continuous(breaks = y_axisValues)
  } else {
    gg <- gg + scale_x_continuous(limits = x_limits)
  }
  
  
  return(gg)
}





my_function1 <- function(x) {return(x^2)}
my_function2 <- function(x) {return(-x^2)}

curve_by_function(my_function1, title="my_function", colors=c("red", "purple"), curveName = c("p"), x=c(20, 35), pointNames = c("p1", "p2"))#, y=c(500, 1000))
