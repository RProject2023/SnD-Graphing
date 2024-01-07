library(ggplot2)

# Create df1 with rising trend (supply)
df1 <- data.frame(x = 1:10, y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

# Create df2 with falling trend (demand)
df2 <- data.frame(x = 1:10, y = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))

plot_smooth_curves <- function(df1, df2, x_col = "x", y_col = "y") {
  plot <- ggplot() +
    geom_smooth(data = df1, aes_string(x = x_col, y = y_col), color = "blue", se = FALSE) +
    geom_smooth(data = df2, aes_string(x = x_col, y = y_col), color = "red", se = FALSE) +
    labs(
      title = "Smoothed Curves Plot",
      x = "X-axis",
      y = "Y-axis"
    ) +
    theme_minimal()

  return(plot)
}

plot_smooth_exponential_polynomial <- function(df, x_col = "x", y_col = "y", method = "auto") {
  plot <- ggplot(df, aes_string(x = x_col, y = y_col)) +
    geom_smooth(method = method, se = FALSE) +
    labs(
      title = paste("Smoothed", ifelse(tolower(method) == "lm", "Polynomial", method), "Function Plot"),
      x = "X-axis",
      y = "Y-axis"
    ) +
    theme_minimal()

  return(plot)
}

plot_intersection <- function(plot1, plot2) {
  # Extract the data from the plots
  data1 <- ggplot_build(plot1)$data[[1]]
  data2 <- ggplot_build(plot2)$data[[1]]

  # Find the intersection coordinates
  intersection <- data.frame(x = NA, y = NA)
  for (i in 1:(nrow(data1) - 1)) {
    for (j in 1:(nrow(data2) - 1)) {
      x1 <- data1$x[i]
      x2 <- data1$x[i + 1]
      y1 <- data1$y[i]
      y2 <- data1$y[i + 1]
      x3 <- data2$x[j]
      x4 <- data2$x[j + 1]
      y3 <- data2$y[j]
      y4 <- data2$y[j + 1]

      if (is.na(intersection$x) && is.na(intersection$y)) {
        if (abs((x2 - x1) * (y4 - y3) - (x4 - x3) * (y2 - y1)) > 1e-10) {
          t1 <- ((x3 - x1) * (y4 - y3) - (x4 - x3) * (y3 - y1)) / ((x2 - x1) * (y4 - y3) - (x4 - x3) * (y2 - y1))
          t2 <- ((x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)) / ((x2 - x1) * (y4 - y3) - (x4 - x3) * (y2 - y1))

          if (t1 >= 0 && t1 <= 1 && t2 >= 0 && t2 <= 1) {
            intersection$x <- x1 + t1 * (x2 - x1)
            intersection$y <- y1 + t1 * (y2 - y1)
          }
        }
      }
    }
  }

  return(intersection)
}


intersection <- plot_intersection(plot_smooth_exponential_polynomial(df1), plot_smooth_exponential_polynomial(df2))
print(intersection)
