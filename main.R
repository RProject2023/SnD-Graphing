library(ggplot2)
library(Hmisc)
library(dplyr)



#' @title Indifference curve
#' @description This function plots an indifference curve.
#'
#' @param curve: Curve to display. This will override the sample curve.
#' @param x: X-axis values where to create intersections.
#' @param labels: If 'x' is specified, these are the labels for the intersection points.
#' @param linecolor: Line color of the curve.
#' @param title: Name of the plot.
#' @param xlabel: Name of the X-axis.
#' @param ylabel: Name of the Y-axis.
#'
#' @import ggplot2 Hmisc

indifference_curve <- function(curve,
                               x,
                               labels,
                               linecolor = "black",
                               title = "Indifference curve",
                               xlabel = "X",
                               ylabel = "Y") {
    if (missing(curve)) {
        # Example indifference curve
        xmax <- 10
        ymax <- 10
        curve <- data.frame(bezier(x = c(1, 0.2 * xmax, xmax), y = c(ymax, 0.2 * ymax, 1)))
    }

    gg <- ggplot(mapping = aes(x = x, y = y))
    gg <- gg + geom_line(data = data.frame(curve), color = linecolor, linewidth = 1, linetype = 1)

    if (!missing(x)) {
        if (missing(labels)) {
            labels <- LETTERS[seq_along(x)]
        }

        intersections <- data.frame()

        x_values <- curve$x
        y_values <- curve$y

        interp_function <- splinefun(x_values, y_values)

        for (i in seq_along(x)) {
            point <- data.frame(x = x[i], y = interp_function(x[i]))
            intersections <- rbind(intersections, point)
        }

        gg <- gg + geom_segment(data = intersections, aes(x = x, y = 0, xend = x, yend = y), linetype = "dotted") +
            geom_segment(data = intersections, aes(x = 0, y = y, xend = x, yend = y), linetype = "dotted") +
            geom_point(data = intersections, size = 3) +
            scale_x_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve$x)) + 1), breaks = c(min(curve$x), max(curve$x), round(intersections$x, 1))) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve$y)) + 1), breaks = c(min(curve$y), max(curve$y), round(intersections$y, 1))) +
            geom_text(data = intersections, aes(x = x, y = y, label = labels, vjust = -0.2, hjust = -0.6))
    } else {
        gg <- gg + scale_x_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve$x)) + 1)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve$y)) + 1))
    }

    gg <- gg + labs(x = xlabel, y = ylabel, title = title) + theme_classic() + coord_equal()

    return(gg)
}

#' @title Curve by Function
#' @description This function plots one or more curves defined by a function.
#'
#' @param ...: One or more input functions.
#' @param title: Title of the displayed graph.
#' @param xlabel: Label of the x-axis.
#' @param ylabel: Label of the y-axis.
#' @param color: Colors of the curves (in the form of a list).
#' @param label: Curve labels (in the form of a list).
#' @param x_coordinates: List of x coordinates for points to draw.
#' @param y_coordinates: List of y coordinates for points to draw.
#' @param pointColor: Colors for the points (in the form of a list).
#' @param showInfo: Boolean indicating whether to show point information.
#' @param pointLabel: Point labels (in the form of a list).
#' @param axisValues: List of values to show on the x-axis.
#' @param minX: Minimum value of the x-axis coordinate.
#' @param maxX: Maximum value of the x-axis coordinate.
#' @param minY: Minimum value of the y-axis coordinate.
#' @param maxY: Maximum value of the y-axis coordinate.
#'
#' @import ggplot2


curve_by_function <- function(...,
                              title = "",
                              xlabel = "Q - quantity",
                              ylabel = "P - price",
                              color = "blue", # curve color
                              label = "", # curve label
                              x_coordinates = NULL, # x-coordinates for points to draw
                              y_coordinates = NULL, # y-coordinates for points to draw
                              pointColor = "blue",
                              showInfo = TRUE, # show point values on x&y axis
                              pointLabel = "", # label for point
                              axisValues = NULL, # show values on x-axis
                              minX = -50,
                              maxX = 50,
                              minY = -100,
                              maxY = 2500) {
    curves <- list(...)
    ncurves <- length(curves)

    if (ncurves > 1 && length(color) != ncurves) {
        color <- rep("blue", ncurves)
    }


    gg <- ggplot(data.frame(x = c(minX, maxX)), aes(x = x)) +
        labs(title = title, x = xlabel, y = ylabel) +
        coord_cartesian(ylim = c(minY, maxY))

    # add curves
    for (i in 1:ncurves) {
        gg <- gg + stat_function(fun = curves[[i]], colour = color[i])
    }

    # add curve names
    if (!is.null(label)) {
        curve_names <- data.frame(
            x = maxX,
            y = sapply(curves, function(f) f(maxX)),
            label = label
        )
        gg <- gg + geom_text(data = curve_names, aes(x = x, y = y, label = label), hjust = -1) # todo for curve names
    }

    # optional add x-axis, y-axis
    # gg <- gg + geom_vline(aes(xintercept = 0))+ #add y-axis where you want
    #      geom_hline(aes(yintercept = 0)) #add back in x axis



    # add points

    if (!is.null(x_coordinates)) {
        if (is.null(y_coordinates)) {
            points <- do.call(rbind, lapply(seq_along(x_coordinates), function(j) {
                data.frame(point_x = x_coordinates[j], point_y = sapply(curves, function(f) f(x_coordinates[j])), group = as.factor(j))
            }))
            y_coordinates <- points$point_y
        } else {
            points <- data.frame(point_x = x_coordinates, point_y = y_coordinates)
        }
        gg <- gg + geom_point(data = points, aes(x = point_x, y = point_y), color = pointColor, size = 3)

        if (showInfo) {
            gg <- gg + geom_segment(data = points, aes(x = point_x, y = minY, xend = point_x, yend = point_y), linetype = "dashed") +
                geom_segment(data = points, aes(x = minX, y = point_y, xend = point_x, yend = point_y), linetype = "dashed") +
                geom_text(data = points, aes(x = point_x, y = point_y, label = pointLabel), hjust = -1, vjust = 1)
        }
    }


    # add numbers to x-axis
    x_limits <- c(minX, maxX)

    if (showInfo) {
        axisValues <- append(axisValues, x_coordinates)
    }

    y_axisValues <- y_coordinates

    if (!is.null(axisValues)) {
        gg <- gg + scale_x_continuous(breaks = c(pretty(x_limits), axisValues))
        if (!is.null(y_axisValues)) {
            gg <- gg + scale_y_continuous(breaks = y_axisValues)
        }
    } else {
        gg <- gg + scale_x_continuous(limits = x_limits)
    }


    return(gg)
}

#' @title Isoquant Curve
#' @description This function plots the isoquant curve(s) for a given production function.
#'
#' @param  L: labor input
#' @param  K: Capital input
#' @param  output_levels: output Q
#' @param  A: technology of the production process
#' @param  alpha: the elasticity of production of labor
#' @param  beta: the elasticity of production of capital
#'
#' @import ggplot2

isoquant <- function(L, K, output_levels, A, alpha, beta) {
    data <- expand.grid(L = L, K = K)
    data$Q <- A * pow(data$L, alpha) * pow(data$K, beta) # formula for COBB-DOUGLAS production function
    ggplot(data, aes(x = L, y = K, z = Q)) +
        geom_contour(aes(z = Q), breaks = output_levels, color = "blue", linetype = "solid", size = 0.8) +
        labs(x = "Labor (L)", y = "Capital (K)", title = paste("Isoquant Curve(s) for Q =", paste(output_levels, collapse = ", "))) +
        theme_minimal()
}

#' @title Production Possibility Frontier
#' @description This function plots the Production Possibility Frontier.
#'
#' @param  ...: curves of Production-Possibility Frontier
#' @param  title: title of the displayed graph
#' @param  colors: colors of the curves
#' @param  x: points on the Production Possibility Frontier
#' @param  labels: labels of points on the Production Possibility Frontier
#' @param  xlabel: label of the x-axis
#' @param  ylabel: label of the y-axis
#'
#' @import ggplot2 Hmisc

production_possibility_frontier <- function(...,
                                            title = "Production-possibility frontier",
                                            colors,
                                            x,
                                            labels,
                                            xlabel = "Product A",
                                            ylabel = "Product B") {
    curves <- list(...)

    gg <- ggplot(mapping = aes(x = x, y = y))

    for (i in seq_along(curves)) {
        gg <- gg + geom_line(data = data.frame(curves[[i]]), color = colors[i], linewidth = 1, linetype = 1)
    }

    if (!missing(x)) {
        intersections <- data.frame()

        for (j in seq_along(curves)) {
            x_values <- curves[[j]]$x
            y_values <- curves[[j]]$y

            interp_function <- splinefun(x_values, y_values)

            for (i in seq_along(x)) {
                point <- data.frame(x = x[i], y = interp_function(x[i]))
                intersections <- rbind(intersections, point)
            }
        }

        gg <- gg + geom_segment(data = intersections, aes(x = x, y = 0, xend = x, yend = y), linetype = "dotted") +
            geom_segment(data = intersections, aes(x = 0, y = y, xend = x, yend = y), linetype = "dotted") +
            geom_point(data = intersections, size = 3) +
            scale_x_continuous(expand = c(0, 0), limits = c(0, max(unlist(curves)) + 1), breaks = round(intersections$x, 1)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, max(unlist(curves)) + 1), breaks = round(intersections$y, 1)) +
            geom_text(data = intersections, aes(x = x, y = y, label = labels, vjust = -0.5, hjust = -0.2))
    } else {
        gg <- gg + scale_x_continuous(expand = c(0, 0), limits = c(0, max(unlist(curves)) + 1)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, max(unlist(curves)) + 1))
    }

    gg <- gg +
        labs(x = xlabel, y = ylabel, title = title) +
        theme_classic()

    return(gg)
}

#####

#' @title Variables:
#' @description This function plots the curves based on the 2 given points.
#'
#' @param   ...: list of points in the form p = (x1, y1, x2, y2)
#' @param   title: title of the displayed graph
#' @param   color: colors of the curves
#' @param   minX: minimum value of the x-axis coordinate
#' @param   maxX: maximum value of the x-axis coordinate
#' @param   minY: minimum value of the y-axis coordinate
#' @param   maxY: maximum value of the y-axis coordinate
#' @param   xlabel: label of the x-axis
#' @param   ylabel: label of the y-axis
#' @param   pointsHidden: are the points hidden
#' @param   lineCoordinateHidden: are the points marked on the coordinates
#'
#' @import ggplot2

linear_curve_2_ponts <- function(...,
                                 title,
                                 color,
                                 minX = 0,
                                 maxX = 10,
                                 minY = 0,
                                 maxY = 10,
                                 xlabel = "Q - quantity",
                                 ylabel = "P - price",
                                 pointsHidden = TRUE,
                                 lineCoordinateHidden = TRUE) {
    gg <- ggplot(data.frame(x = c(minX, maxX), y = c(minY, minY)), aes(x = x, y = y))
    points_df <- data.frame()
    segments_df <- data.frame()

    for (i in seq_along(list(...))) {
        points <- unlist(list(...)[[i]])

        x1 <- points[1]
        y1 <- points[2]
        x2 <- points[3]
        y2 <- points[4]

        x_axis <- c(x1, x2)
        y_axis <- c(y1, y2)
        points_df <- rbind(points_df, data.frame(x = x_axis, y = y_axis))
        segments_df <- rbind(segments_df, data.frame(x = c(x1, x1, minX), y = c(y1, minY, y1), xend = x1, yend = y1))
        segments_df <- rbind(segments_df, data.frame(x = c(x2, x2, minX), y = c(y2, minY, y2), xend = x2, yend = y2))

        slope <- (y2 - y1) / (x2 - x1)
        intercept <- y1 - slope * x1

        gg <- gg + geom_abline(intercept = intercept, slope = slope, color = color[i])
    }

    if (!pointsHidden) {
        gg <- gg + geom_point(data = points_df, aes(x = x, y = y), color = "black", size = 3)
    }

    if (!lineCoordinateHidden) {
        gg <- gg + geom_segment(data = segments_df, aes(x = x, y = y, xend = xend, yend = yend), linetype = "dotted", color = "black") +
            scale_x_continuous(expand = c(minX, minX), limits = c(minX, maxX), breaks = round(points_df$x, 1)) +
            scale_y_continuous(expand = c(minY, minY), limits = c(minY, maxY), breaks = round(points_df$y, 1))
    } else {
        gg <- gg +
            xlim(minX, maxX) +
            ylim(minY, maxY)
    }

    gg <- gg +
        labs(title = title, x = xlabel, y = ylabel) +
        theme_classic()

    return(gg)
}

#' @title   Linear curve characteristic
#' @description This function plots the curves based on the curve's characteristics.
#'
#' @param   ...: list of characteristic in the form c = (slope, intercept)
#' @param   title: title of the displayed graph
#' @param   color: colors of the curves
#' @param   minX: minimum value of the x-axis coordinate
#' @param   maxX: maximum value of the x-axis coordinate
#' @param   minY: minimum value of the y-axis coordinate
#' @param   maxY: maximum value of the y-axis coordinate
#' @param   xlabel: label of the x-axis
#' @param   ylabel: label of the y-axis
#'
#' @import ggplot2

linear_curve_characteristic <- function(...,
                                        title,
                                        color,
                                        minX = 0,
                                        maxX = 10,
                                        minY = 0,
                                        maxY = 10,
                                        xlabel = "Q - quantity",
                                        ylabel = "P - price") {
    gg <- ggplot(data.frame(x = c(minX, maxX), y = c(minY, minY)), aes(x = x, y = y))
    points_df <- data.frame()
    segments_df <- data.frame()

    for (i in seq_along(list(...))) {
        characteristic <- unlist(list(...)[[i]])

        slope <- characteristic[1]
        intercept <- characteristic[2]

        gg <- gg + geom_abline(intercept = intercept, slope = slope, color = color[i])
    }

    gg <- gg +
        xlim(minX, maxX) +
        ylim(minY, maxY) +
        labs(title = title, x = xlabel, y = ylabel) +
        theme_classic()

    return(gg)
}

#' @title  Curve by N points
#' @description This function plots the curves based on the given points.
#'
#' @param   ...: list of curves in the form curve <- bezier(x = c(x1, x2, x3...), y = c(y1, y2, y3...)) %>% as.data.frame()
#' @param   title: title of the displayed graph
#' @param   color: colors of the curves
#' @param   xlabel: label of the x-axis
#' @param   ylabel: label of the y-axis

curve_N_ponts <- function(...,
                          title,
                          color,
                          xlabel = "Q - quantity",
                          ylabel = "P - price") {
    curve <- list(...)
    ncurve <- length(curve)
    gg <- ggplot(mapping = aes(x = x, y = y))

    for (i in 1:length(curve)) {
        gg <- gg + geom_line(data = data.frame(curve[[i]]), color = color[i], linewidth = 1, linetype = 1)
    }

    gg <- gg +
        labs(x = xlabel, y = ylabel, title = title) +
        theme_classic()

    return(gg)
}

#' @title  Point of equilibrium
#' @description This function plots the point of equilibrium (intersection) of two curves.
#'
#' @param  curve1: first curve
#' @param  curve2: second curve
#' @param  gg: graph on which the intersection point of curve 1 and curve 2 will be plotted
#'
#' @import ggplot2

equilibrium <- function(curve1, curve2, gg) {
    curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
    curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)

    point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x), c(min(curve1$x), max(curve1$x)))$root

    point_y <- curve2_f(point_x)
    intersections <- bind_rows(list(x = point_x, y = point_y))

    gg <- gg +
        geom_segment(data = intersections, aes(x = point_x, y = 0, xend = point_x, yend = point_y), lty = "dotted") +
        geom_segment(data = intersections, aes(x = point_x, y = point_y, xend = 0, yend = point_y), lty = "dotted") +
        geom_point(data = intersections, size = 3) +
        geom_text(data = intersections, aes(x = point_x, y = point_y, label = sprintf("(%0.1f, %0.1f)", point_x, point_y), hjust = -0.3))

    out <- list("plot" = gg, "x" = point_x, "y" = point_y)
    return(out)
}


#' @title Supplier's revenue
#' @description This function calculates the supplier's revenue.
#'
#' @param  curve1: first curve
#' @param  curve2: second curve
#' @param gg: graph on which the revenue will be plotted
#'
#' @import ggplot2

supplier_revenue <- function(curve1, curve2, gg) {
    equilibrium <- equilibrium(curve1, curve2, gg)

    revenue <- 0
    if (!is.na(equilibrium$x) && !is.na(equilibrium$y)) {
        revenue <- equilibrium$x * equilibrium$y
    }

    # draw a straight line from the origin to the point of equilibrium
    gg <- gg +
        geom_segment(aes(x = 0, y = 0, xend = equilibrium$x, yend = equilibrium$y), color = "red", size = 1) +
        geom_text(aes(x = equilibrium$x / 2, y = equilibrium$y / 2, label = sprintf("Revenue = %0.1f", revenue), hjust = -0.3))

    out <- list("plot" = gg, "revenue" = revenue)
    return(out)
}

#' @title Surplus area plot
#' @description This function plots the surplus area.
#'
#' @param curve1: first curve
#' @param curve2: second curve
#' @param gg: graph on which the surplus area will be plotted
#' @param target: target of surplus calculation (supplier - 0,  consumer - 1, both - 2)
#'
#' @import ggplot2

surplus_area <- function(curve1, curve2, gg, target = 0) {
    equilibrium <- equilibrium(curve1, curve2, gg)

    surplus <- 0
    if (!is.na(equilibrium$x) && !is.na(equilibrium$y)) {
        surplus <- equilibrium$x * equilibrium$y
    }

    # draw a straight line from the origin to the point of equilibrium
    gg <- gg +
        geom_segment(aes(x = 0, y = 0, xend = equilibrium$x, yend = equilibrium$y), color = "red", size = 1) +
        geom_text(aes(x = equilibrium$x / 2, y = equilibrium$y / 2, label = sprintf("Surplus = %0.1f", surplus), hjust = -0.3))

    if (target == 0) {
        gg <- gg + geom_area(data = data.frame(curve1), aes(x = x, y = y), fill = "red", alpha = 0.5)
    } else if (target == 1) {
        gg <- gg + geom_area(data = data.frame(curve2), aes(x = x, y = y), fill = "red", alpha = 0.5)
    } else if (target == 2) {
        gg <- gg + geom_area(data = data.frame(curve1), aes(x = x, y = y), fill = "red", alpha = 0.5) +
            geom_area(data = data.frame(curve2), aes(x = x, y = y), fill = "red", alpha = 0.5)
    }

    out <- list("plot" = gg, "surplus" = surplus)
    return(out)
}
