supplier_revenue <- function(equilibrium) {
    revenue <- 0
    if (!is.na(equilibrium$x) && !is.na(equilibrium$y)) {
        revenue <- equilibrium$x * equilibrium$y
    }

    return(revenue)
}
