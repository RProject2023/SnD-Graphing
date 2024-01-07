library(ggplot2)

plot_smooth_exponential_polynomial <- function(df, x_col = "q", y_col = "p", method = "auto") {
  plot <- ggplot(df, aes_string(x = x_col, y = y_col)) +
    geom_smooth(method = method, se = FALSE) +
    labs(
      title = paste("Smoothed", ifelse(tolower(method) == "lm", "Polynomial", method), "Function Plot"),
      x = x_col,
      y = y_col
    ) +
    theme_minimal()

  return(plot)
}

plot_equilibrium <- function(supplyPlot, demandPlot, x_col = "q", y_col = "p") {
  
  supplyPlotData <- supplyPlot$data
  demandPlotData <- demandPlot$data


  equilibrium <- data.frame(x = NA, y = NA)
  colnames(equilibrium) <- c(x_col, y_col)
  
  
  for (i in 1:(nrow(supplyPlotData) - 1)) {
    for (j in 1:(nrow(demandPlotData) - 1)) {
      x1 <- supplyPlotData[[x_col]][i]
      x2 <- supplyPlotData[[x_col]][i + 1]
      y1 <- supplyPlotData[[y_col]][i]
      y2 <- supplyPlotData[[y_col]][i + 1]

      x3 <- demandPlotData[[x_col]][j]
      x4 <- demandPlotData[[x_col]][j + 1]
      y3 <- demandPlotData[[y_col]][j]
      y4 <- demandPlotData[[y_col]][j + 1]

      if (is.na(equilibrium[[x_col]]) && is.na(equilibrium[[y_col]])) {
        if (abs((x2 - x1) * (y4 - y3) - (x4 - x3) * (y2 - y1)) > 1e-10) {
          t1 <- ((x3 - x1) * (y4 - y3) - (x4 - x3) * (y3 - y1)) / ((x2 - x1) * (y4 - y3) - (x4 - x3) * (y2 - y1))
          t2 <- ((x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)) / ((x2 - x1) * (y4 - y3) - (x4 - x3) * (y2 - y1))

          if (t1 >= 0 && t1 <= 1 && t2 >= 0 && t2 <= 1) {
            equilibrium[[x_col]] <- x1 + t1 * (x2 - x1)
            equilibrium[[y_col]] <- y1 + t1 * (y2 - y1)
          }
        }
      }
    }
  }

  return(equilibrium)
}


col_x <- 'quantity'
col_y <- 'price'

supply <- data.frame(quantity = 1:10, price = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
demand <- data.frame(quantity = 1:10, price = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))

supplyPlot <- plot_smooth_exponential_polynomial(supply, x_col = col_x, y_col = col_y)
demandPlot <- plot_smooth_exponential_polynomial(demand, x_col = col_x, y_col = col_y)

equilibrium <- plot_equilibrium(supplyPlot, demandPlot, col_x, col_y)

print(equilibrium)

