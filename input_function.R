library(ggplot2)


curve_by_function <- function(...,
                              title, 
                              color="red",
                              minX=-50, 
                              maxX=50, 
                              minY=-5, 
                              maxY=30,
                              xlabel="Q - quantity",
                              ylabel="P - price") {

  gg <- ggplot(data.frame(x=c(minX, maxX)), aes(x = x))
  

  curves <- list(...)
  ncurves <- length(curves)
  print(ncurves)
  
  
  for(i in 1:ncurves){
    gg <- gg + stat_function(fun = curves[[i]], color = color) 
  }
  
  
  gg <- gg + xlim(minX, maxX) + 
    labs(title, x = xlabel, y = ylabel)
  
  
  return(gg)
}

my_function1 <- function(x) {
  return(x^2)
}

my_function2 <- function(x) {
  return(-x^2)
}

curve_by_function(my_function1, my_function2, title="my_function", color="blue")


