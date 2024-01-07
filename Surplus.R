# Variables:
#   supplyPlot: supply plot
#   demandPlot: demand plot
#   x_col: x column name
#   y_col: y column name
#   target: target of surplus calculation (supplier or consumer)
#   price_col: column name of price (x or y, required because q and p aren't standard names)

surplus <- function(supplyPlot, demandPlot, price, target = "supplier", price_col = "y", x_col = "q", y_col = "p") {
    surplus <- 0
    supplyPlotData <- supplyPlot$data
    demandPlotData <- demandPlot$data

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

            if (price_col == "y") {
                if (price >= y1 && price <= y2 && price >= y4 && price <= y3) {
                    # print(y1,y2,y3,y4)
                    x_upper <- ((price - y1) / (y2 - y1)) * (x2 - x1) + x1
                    x_lower <- ((price - y3) / (y4 - y3)) * (x4 - x3) + x3
                    surplus <- x_upper - x_lower
                    if(target == "supplier"){
                      return( surplus)
                    } 
                    return(-surplus)
                    
                }
            } else {
                if (price >= x1 && price <= x2 && price >= x4 && price <= x3) {
                    y_upper <- ((price - x1) / (x2 - x1)) * (y2 - y1) + y1
                    y_lower <- ((price - x3) / (x4 - x3)) * (y4 - y3) + y3
                    surplus <- y_upper - y_lower
                    if(target == "supplier"){
                      return( surplus)
                    } 
                    return(-surplus)
                }
            }
        }
    }

    return(surplus)
}
