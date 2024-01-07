supplier_surplus <- function(supplyPlot, demandPlot, price) {
    # Calculate the supplier surplus
    surplus <- 0
    for (i in 1:(nrow(supplyPlot) - 1)) {
        x1 <- supplyPlot$x[i]
        x2 <- supplyPlot$x[i + 1]
        y1 <- supplyPlot$y[i]
        y2 <- supplyPlot$y[i + 1]

        if (price >= y1 && price <= y2) {
            surplus <- surplus + (price - y1) * (x2 - x1) / 2
        }
    }

    return(surplus)
}
