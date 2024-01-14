# Variables:
#   L: labor input
#   K: Capital input
#   output_levels: output Q
#   A: technology of the production process
#   alpha: the elasticity of production of labor
#   beta: the elasticity of production of capital

#installing packages 
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if(!require('extras')) {
  install.packages('extras')
  library('extras')
}

#isoquant plotting function
isoquant <- function(L, K, output_levels, A, alpha, beta) {
  data <- expand.grid(L = L, K = K)
  data$Q <- A * pow(data$L, alpha) * pow(data$K, beta)  #formula for COBB-DOUGLAS production function 
  ggplot(data, aes(x = L, y = K, z = Q)) +
  geom_contour(aes(z = Q), breaks = output_levels, color = "blue", linetype = "solid", size = 0.8) +
  labs(x = "Labor (L)", y = "Capital (K)", title = paste("Isoquant Curve(s) for Q =", paste(output_levels, collapse = ", "))) +
  theme_minimal()
}

#PARAMETERS

L <- seq(1, 100, length.out = 100) 
K <- seq(1, 100, length.out = 100) 

#output Q (1 or more)
output_levels <- c(25,50,75) 

A <- 1 
#alpha [0,1]
alpha <- 0.5 
#beta [0,1]
beta <- 0.5 

isoquant(L,K,output_levels,A,alpha,beta)
