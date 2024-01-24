library(ggplot2)
library(Hmisc)
library(dplyr)
library(extras)


#' @title Variables:
#' @description This function plots the curves based on the 2 given points.
#'
#' @param   ...: list of points in the form p = (x1, y1, x2, y2)
#' @param   title: title of the displayed graph
#' @param   colors: colors of the curves
#' @param   minX: minimum value of the x-axis coordinate
#' @param   maxX: maximum value of the x-axis coordinate
#' @param   minY: minimum value of the y-axis coordinate
#' @param   maxY: maximum value of the y-axis coordinate
#' @param   xlabel: label of the x-axis
#' @param   ylabel: label of the y-axis
#' @param   pointsHidden: are the points hidden
#' @param   lineCoordinateHidden: are the points marked on the coordinates
#' @param   gg: graph on which the linear curves will be plotted
#'
#' @return a new graph with the curves based on the 2 given points and a list of curves defined by 2 points

linear_curve_2_points <- function(...,
                                  title,
                                  colors,
                                  minX = 0,
                                  maxX = 10,
                                  minY = 0,
                                  maxY = 10,
                                  xlabel = "Q - quantity",
                                  ylabel = "P - price",
                                  pointsHidden = TRUE,
                                  lineCoordinateHidden = TRUE,
                                  gg = NULL) {
  
    if(length(list(...)) != length(colors)) {
        stop("Number of elements in '...' should be the same as the number of elements in 'colors'.")
    }
  
    if (is.null(gg)) {
        gg <- ggplot(data.frame(x = c(minX, maxX), y = c(minY, minY)), aes(x = x, y = y))
    }

    points_df <- data.frame()
    segments_df <- data.frame()
    curves_list <- list()
    
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
        
        x_values <- seq(minX, maxX)
        y_values <- intercept + slope * x_values
        
        curves_list[[i]] <- bezier(x = x_values, y = y_values) %>% as.data.frame()
        
        gg <- gg + geom_abline(intercept = intercept, slope = slope, color = colors[i], linewidth = 1)
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
        theme_minimal()

    return(list("plot" = gg, "curves" = curves_list))
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
#' @param   gg: graph on which the linear curves will be plotted
#'
#' @return a new graph with the curves based on the curve's characteristics and a list of curves defined by characteristics

linear_curve_characteristic <- function(...,
                                        title,
                                        colors,
                                        minX = 0,
                                        maxX = 10,
                                        minY = 0,
                                        maxY = 10,
                                        xlabel = "Q - quantity",
                                        ylabel = "P - price",
                                        gg = NULL) {
  
    if(length(list(...)) != length(colors)) {
        stop("Number of elements in '...' should be the same as the number of elements in 'colors'.")
    }
  
    if (is.null(gg)) {
        gg <- ggplot(data.frame(x = c(minX, maxX), y = c(minY, minY)), aes(x = x, y = y))
    }

    points_df <- data.frame()
    segments_df <- data.frame()
    curves_list <- list()
    
    for (i in seq_along(list(...))) {
        characteristic <- unlist(list(...)[[i]])

        slope <- characteristic[1]
        intercept <- characteristic[2]
        
        x_values <- seq(minX, maxX)
        y_values <- intercept + slope * x_values
        
        curves_list[[i]] <- bezier(x = x_values, y = y_values) %>% as.data.frame()
        
        gg <- gg + geom_abline(intercept = intercept, slope = slope, color = colors[i], linewidth = 1)
    }

    gg <- gg +
        xlim(minX, maxX) +
        ylim(minY, maxY) +
        labs(title = title, x = xlabel, y = ylabel) +
        theme_minimal()

    return(list("plot" = gg, "curves" = curves_list))
}

#' @title  Curve by N points
#' @description This function plots the curves based on the given points.
#'
#' @param   ...: list of curves in the form curve <- bezier(x = c(x1, x2, x3...), y = c(y1, y2, y3...)) %>% as.data.frame()
#' @param   title: title of the displayed graph
#' @param   colors: colors of the curves
#' @param   minX: minimum value of the x-axis coordinate
#' @param   maxX: maximum value of the x-axis coordinate
#' @param   minY: minimum value of the y-axis coordinate
#' @param   maxY: maximum value of the y-axis coordinate
#' @param   xlabel: label of the x-axis
#' @param   ylabel: label of the y-axis
#' @param   gg: graph on which the curves will be plotted
#'
#' @return a modified graph with the curves based on the given points

curve_N_points <- function(...,
                           title,
                           colors,
                           minX = 0,
                           maxX = 10,
                           minY = 0,
                           maxY = 10,
                           xlabel = "Q - quantity",
                           ylabel = "P - price",
                           gg = NULL) {
    curve <- list(...)
    ncurve <- length(curve)
    
    if(length(list(...)) != length(colors)) {
      stop("Number of elements in '...' should be the same as the number of elements in 'colors'.")
    }
    
    if (is.null(gg)) {
        gg <- ggplot(mapping = aes(x = x, y = y))
    }

    for (i in 1:length(curve)) {
        gg <- gg + geom_line(data = data.frame(curve[[i]]), color = colors[i], linewidth = 1, linetype = 1)
    }

    gg <- gg +
        xlim(minX, maxX) +
        ylim(minY, maxY) +
        labs(x = xlabel, y = ylabel, title = title) +
        theme_minimal()

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
#' @param gg: graph on which the curves will be plotted
#'
#' @return A new graph with the curves based on the given functions.


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
                              minX = 0,
                              maxX = 50,
                              minY = 0,
                              maxY = 2500,
                              gg = NULL) {
    curves <- list(...)
    ncurves <- length(curves)

    if (ncurves > 1 && length(color) != ncurves) {
        color <- rep("blue", ncurves)
    }

    if (is.null(gg)) {
      gg <- ggplot(data.frame(x = c(minX, maxX)), aes(x = x))
    }
    
    gg <- gg + labs(title = title, x = xlabel, y = ylabel) +
        coord_cartesian(ylim = c(minY, maxY)) +
        theme_minimal()

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
#' @param  gg: graph on which the revenue will be plotted
#' @param  color: color of the revenue line
#' @param  displayValue: display the revenue value on the graph
#'
#' @return a list containing the modified graph with the supplier's revenue and the revenue value

supplier_revenue <- function(curve1, curve2, gg, color = "green", displayValue = TRUE) {
    equilibrium <- equilibrium(curve1, curve2, gg)

    revenue <- 0
    if (!is.na(equilibrium$x) && !is.na(equilibrium$y)) {
        revenue <- equilibrium$x * equilibrium$y
    }

    # draw a straight line from the origin to the point of equilibrium
    gg <- gg +
        geom_segment(aes(x = equilibrium$x, y = 0, xend = equilibrium$x, yend = equilibrium$y), color = color, size = 1) +
        geom_segment(aes(x = 0, y = equilibrium$y, xend = equilibrium$x, yend = equilibrium$y), color = color, size = 1) +
        geom_ribbon(aes(x = c(0, equilibrium$x), ymin = 0, ymax = equilibrium$y, fill = color), alpha = 0.15, inherit.aes = FALSE)
    if (displayValue) {
        gg <- gg +
            geom_text(aes(x = equilibrium$x / 2, y = equilibrium$y / 2, label = sprintf("Revenue = %0.1f", revenue), hjust = -0.3))
    }

    out <- list("plot" = gg, "revenue" = revenue)
    return(out)
}

#' @title Surplus area plot
#' @description This function plots the surplus area.
#'
#' @param curve1: first curve
#' @param curve2: second curve
#' @param gg: graph on which the surplus area will be plotted
#'
#' @return a modified graph with the surplus area

surplus_area <- function(curve1, curve2, gg) {
    df1x <- curve1$x
    df1y <- curve1$y
    df2x <- curve2$x
    df2y <- curve2$y

    equilibrium <- equilibrium(curve1, curve2, gg)

    df1x <- df1x[df1x <= equilibrium$x]
    df2x <- df2x[df2x <= equilibrium$x]
    df1y <- df1y[df1y <= equilibrium$y]
    df2y <- df2y[df2y >= equilibrium$y]

    gg <- gg +
        geom_ribbon(aes(x = df1x, ymin = df1y, ymax = equilibrium$y, fill = "Producer surplus"), alpha = 0.15, inherit.aes = FALSE) +
        geom_ribbon(aes(x = df2x, ymin = equilibrium$y, ymax = df2y, fill = "Consumer surplus"), alpha = 0.15, inherit.aes = FALSE)

    return(gg)
}


#' @title Indifference curve
#' @description This function plots an indifference curve.
#'
#' @param x: Points on the x-axis along which the curve is drawn. At least three points in ascending order must be given.
#' @param y: Points on the y-axis along which the curve is drawn. At least three points in desending order must be given.
#' @param n_curves: Number of curves to display.
#' @param x_intersections: X-axis values where to create intersections.
#' @param labels: If 'x_intersections' is specified, these are the labels for the intersection points.
#' @param linecolor: Line color of the curve.
#' @param title: Name of the plot.
#' @param xlabel: Name of the X-axis.
#' @param ylabel: Name of the Y-axis.
#'
#' @return A list containing the new graph with the indifference curve and the curves.

indifference_curve <- function(x,
                               y,
                               n_curves = 1,
                               x_intersections,
                               labels,
                               linecolor = "black",
                               title = "Indifference curve",
                               xlabel = "X",
                               ylabel = "Y") {
    if (missing(x) || length(x) < 3) {
        stop("'x' must have at least three elements.")
    }

    if (missing(y) || length(y) < 3) {
        stop("'y' must have at least three elements.")
    }

    if (!(all(diff(x) > 0))) {
        stop("'x' should be in ascending order.")
    }

    if (!(all(diff(y) < 0))) {
        stop("'y' should be in descending order.")
    }

    curve <- data.frame(bezier(x = x, y = y))

    curves <- list()
    for (i in 0:(n_curves - 1)) {
        curves[[i + 1]] <- data.frame(curve) + i
    }

    gg <- ggplot(mapping = aes(x = x, y = y))
    for (i in 0:(n_curves - 1)) {
        gg <- gg + geom_line(data = curves[[i + 1]], color = linecolor, linewidth = 1, linetype = 1)
    }

    if (!missing(x_intersections)) {
        if (missing(labels)) {
            num_labels <- length(x_intersections) * n_curves
            labels <- LETTERS[seq_len(num_labels)]
        }

        if (length(labels) != length(x_intersections) * n_curves) {
            stop("Number of labels is incorrect.")
        }

        intersections <- data.frame()

        for (j in seq_along(curves)) {
            x_values <- curves[[j]]$x
            y_values <- curves[[j]]$y

            interp_function <- splinefun(x_values, y_values)

            for (i in seq_along(x_intersections)) {
                point <- data.frame(x = x_intersections[i], y = interp_function(x_intersections[i]))
                intersections <- rbind(intersections, point)
            }
        }

        gg <- gg + geom_segment(data = intersections, aes(x = x, y = 0, xend = x, yend = y), linetype = "dotted") +
            geom_segment(data = intersections, aes(x = 0, y = y, xend = x, yend = y), linetype = "dotted") +
            geom_point(data = intersections, size = 3) +
            scale_x_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve$x)) + n_curves), breaks = round(intersections$x, 1)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve$y)) + n_curves), breaks = round(intersections$y, 1)) +
            geom_text(data = intersections, aes(x = x, y = y, label = labels, vjust = -0.2, hjust = -0.6))
    } else {
        gg <- gg + scale_x_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve$x)) + n_curves)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve$y)) + n_curves))
    }

    gg <- gg + labs(x = xlabel, y = ylabel, title = title) + theme_minimal()

    out <- list("plot" = gg, "curves" = curves)
    return(out)
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
#' @return a new graph with the Production Possibility Frontier

production_possibility_frontier <- function(...,
                                            title = "Production-possibility frontier",
                                            colors,
                                            x,
                                            labels,
                                            xlabel = "Product A",
                                            ylabel = "Product B",
                                            gg = NULL) {
    curves <- list(...)

    if (is.null(gg)) {
        gg <- ggplot(mapping = aes(x = x, y = y))
    }

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
        theme_minimal()

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
#' @return a new graph with the isoquant curve(s) for the given production function

isoquant <- function(L, K, output_levels, A, alpha, beta) {
    data <- expand.grid(L = L, K = K)
    data$Q <- A * pow(data$L, alpha) * pow(data$K, beta) # formula for COBB-DOUGLAS production function
    ggplot(data, aes(x = L, y = K, z = Q)) +
        geom_contour(aes(z = Q), breaks = output_levels, color = "blue", linetype = "solid", size = 0.8) +
        labs(x = "Labor (L)", y = "Capital (K)", title = paste("Isoquant Curve(s) for Q =", paste(output_levels, collapse = ", "))) +
        theme_minimal()
}



#' @title Move Curves Along x-axis
#' @description This function moves the curves along the x-axis by a specified offset.
#'
#' @param ... Curves to be plotted.
#' @param offset How much the graph needs to be moved (either a specific value or a percentage, 0 by default).
#' @param is_percentage FALSE if offset represents an exact value, TRUE if it represents a percentage (FALSE by default).
#' @param title Title of the displayed graph.
#' @param colors Colors of the curves.
#' @param colors2 Colors of the moved curves.
#' @param minX Minimum value of the x-axis coordinate.
#' @param maxX Maximum value of the x-axis coordinate.
#' @param xlabel Label of the x-axis.
#' @param ylabel Label of the y-axis.
#'
#' @return The modified graph with the specified offset along the x-axis.

move_curves_along_x_axis <- function(...,
                                     offset = 0,
                                     is_percentage = FALSE,
                                     title = "Move along x-axis",
                                     colors = "blue",
                                     colors2 = "red",
                                     minX = 0,
                                     maxX = 20,
                                     xlabel = "x",
                                     ylabel = "y") {
    curves <- list(...)
    moved_curves <- list()

    gg <- ggplot(mapping = aes(x = x, y = y)) +
        xlim(minX, maxX)

    for (i in seq_along(curves)) {
        data <- data.frame(curves[[i]])

        gg <- gg + geom_line(data = data, color = colors[i], linewidth = 1, linetype = 1)

        if (is_percentage) {
            data$x <- data$x + diff(range(data$x)) * (offset / 100)
        } else {
            data$x <- data$x + offset
        }

        moved_curves[[i]] <- data

        gg <- gg + geom_line(data = data, color = colors2[i], linewidth = 1, linetype = 1)
    }

    gg <- gg +
        theme_minimal() +
        labs(title = title, x = xlabel, y = ylabel)

    return(list("plot" = gg, "moved_curves" = moved_curves))
}

#' @title Move Graph Along x-axis
#' @description This function moves the graph along the x-axis by a specified offset.
#'
#' @param plot Graph which needs to be moved along the x-axis.
#' @param offset How much the graph needs to be moved (either a specific value or a percentage, 0 by default).
#' @param is_percentage FALSE if the offset represents an exact value, TRUE if it represents a percentage (FALSE by default).
#'
#' @return The modified graph with the specified offset along the x-axis.

move_graph_along_x_axis <- function(plot, offset = 0, is_percentage = FALSE) {
    layers <- plot$layers

    move_layer <- lapply(layers, function(layer) {
        try(
            if (is_percentage) {
                layer$data$x <- layer$data$x + diff(range(layer$data$x)) * (offset / 100)
            } else {
                layer$data$x <- layer$data$x + offset
            }
        )
        return(layer)
    })

    plot2 <- plot
    plot2$layers <- move_layer

    return(plot2)
}
