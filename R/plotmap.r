#------------------------------------------------------------------------------
#' Create new ogawa forest plot
#'
#' Create new plot for ogawa forest plot map.
#'
#' @param xmin
#'     a minimum value of x coordinate.
#' @param xmax
#'     a maximum value of x coordinate.
#' @param ymin
#'     a minimum value of y coordinate.
#' @param ymax
#'     a maximum value of y coordinate.
#' @param label_pos_x
#'     a numeric vector with two elements. The first element denotes distance
#'     (% of shorter axis) between small X axis labels and the grid and the
#'     second denotes distance between large X axis labels and the grid.
#' @param label_pos_y
#'     a numeric vector with two elements. The first element denotes distance
#'     (% of shorter axis) between small Y axis labels and the grid and the
#'     second denotes distance between large Y axis labels and the grid.
#'
#' @return an \code{ogawa_plot} object which can be passed to other functions.
#'
#' @examples
#' x <- create_ogawa_plot()
#' add_grid(x)
#'
#' @export
#------------------------------------------------------------------------------
create_ogawa_plot <- function(
    xmin = 0, xmax = 300, ymin = 0, ymax = 200,
    label_pos_x = c(0.02, 0.05), label_pos_y = c(0.02, 0.06), ...
) {
    x <- create_object(xmin, xmax, ymin, ymax, label_pos_x, label_pos_y)
    adjustment <- min(abs(x$xmax - x$xmin), abs(x$ymax - x$ymin))
    x_margin <- adjustment * label_pos_y[2]
    y_margin <- adjustment * label_pos_x[2]
    plot(
        NA, type = "n", xlim = c(x$xmin - x_margin, x$xmax),
        ylim = c(x$ymin, x$ymax + y_margin),
        bty = "n", axes = FALSE, xlab = "", ylab = "", ...
    )
    return(x)
}


#------------------------------------------------------------------------------
#   Create ogawa_plot object.
#------------------------------------------------------------------------------
create_object <- function(xmin, xmax, ymin, ymax, label_pos_x, label_pos_y) {
    mod5 <- function(x) x - (x %% 5)
    object <- list(
        xmin = mod5(xmin), xmax = mod5(xmax), ymin = mod5(ymin),
        ymax = mod5(ymax), label_pos_x = label_pos_x, label_pos_y = label_pos_y
    )
    class(object) <- "ogawa_plot"
    return(object)
}


#------------------------------------------------------------------------------
#' Add grid lines for the plot
#'
#' A low level graphic function draws grid lines for the ogawa plot map.
#'
#' @param x
#'     an \code{ogawa.plot} object.
#' @param adds_sq_legend
#'     if TRUE, adds an legend for sub-quadrats.
#' @param draws_1_2ha
#'     if TRUE, draws the 1.2ha core plot region.
#' @param grid_level
#'     an integer representing level of grid line to draw.
#'     \code{0}: draw all grid lines, \code{1}: omit 5m grid lines,
#'     \code{2}: omit 5m and 10m grid lines, and \code{4}: omit all grid lines.
#'
#' @export
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics text
#' @importFrom grDevices dev.hold
#' @importFrom grDevices dev.flush
#------------------------------------------------------------------------------
add_grid <- function(
    x, adds_sq_legend = TRUE, draws_1_2ha = TRUE, grid_level = 0
) {
    stopifnot(grid_level %in% 0:3)
    dev.hold()
    do.call(
        draw_grid, c(x, list(grid_level = grid_level))
    )
    do.call(draw_labels, x)
    if (draws_1_2ha) {
        do.call(draw_1_2ha, x)
    }
    if (adds_sq_legend) {
        do.call(draw_sq_legend, x)
    }
    dev.flush()
}


#------------------------------------------------------------------------------
#   Draw grid lines.
#------------------------------------------------------------------------------
draw_grid <- function(xmin, xmax, ymin, ymax, grid_level, ...) {
    # Draw outline.
    rect(xmin, ymin, xmax, ymax, lwd = 2)
    # Prepare grid lines.
    x <- (xmin / 5):(xmax / 5)
    v_lines <- data.frame(
        x1 = x * 5, y1 = ymin, x2 = x * 5, y2 = ymax,
        lwd = ifelse(x %% 4 == 0, 2, 1),
        level = ifelse(x %% 4 == 0, 3, ifelse(x %% 2 == 0, 2, 1))
    )
    y <- (ymin / 5):(ymax / 5)
    h_lines <- data.frame(
        x1 = xmin, y1 = y * 5, x2 = xmax, y2 = y * 5,
        lwd = ifelse(y %% 4 == 0, 2, 1),
        level = ifelse(y %% 4 == 0, 3, ifelse(y %% 2 == 0, 2, 1))
    )
    l <- rbind(v_lines, h_lines)
    # Filter grid lines.
    l <- l[l$level > grid_level, ]
    # Draw lines.
    segments(l$x1, l$y1, l$x2, l$y2, lwd = l$lwd)
}


#------------------------------------------------------------------------------
#   Draw labels.
#------------------------------------------------------------------------------
draw_labels <- function(
    xmin, xmax, ymin, ymax, label_pos_x, label_pos_y, ...
) {
    adjust_x <- abs(xmax - xmin)
    adjust_y <- abs(ymax - ymin)
    # X-axis label (small).
    x <- (xmin / 10):(xmax / 10)
    xs <- data.frame(
        x = x * 10, y = ymax + adjust_y * label_pos_x[1], labels = x + 1,
        cex = 0.7, font = 1
    )
    # X-axis label (large).
    x <- (xmin / 20 + 1):(xmax / 20)
    xl <- data.frame(
        x = x * 20 - 10, y = ymax + adjust_y * label_pos_x[2],
        labels = LETTERS[x], cex = 1.5, font = 2
    )
    # Y-axis label (small).
    y <- (ymin / 10):(ymax / 10)
    ys <- data.frame(
        x = xmin - adjust_x * label_pos_y[1], y = y * 10,
        labels = ((200 - ymin) / 10 + 1):((200 - ymax) / 10 + 1),
        cex = 0.7, font = 1
    )
    # Y-axis label (large).
    y <- (ymin / 20 + 1):(ymax / 20)
    yl <- data.frame(
        x = xmin - adjust_x * label_pos_y[2],
        y = ymax + 10 - 2 * y * 10 + ymin,
        labels = ((200 - ymax) / 20 + 1):((200 - ymin) / 20), cex = 1.5,
        font = 2
    )
    do.call(text, rbind(xs, xl, ys, yl))
}


#------------------------------------------------------------------------------
#   Draw 1.2ha region.
#------------------------------------------------------------------------------
draw_1_2ha <- function(xmin, xmax, ymin, ymax, ...) {
    rect(
        max(xmin, 120), max(ymin, 60), min(xmax, 220), min(ymax, 180), lwd = 5
    )
}


#------------------------------------------------------------------------------
#   Draw sub-quadrat region.
#------------------------------------------------------------------------------
draw_sq_legend <- function(xmin, xmax, ymin, ymax, ...) {
    for (i in 1:4) {
        text(
            xmin + 5 + (i + 1) %% 2 * 10, ymax - 5 - ((i) %/% 3) * 10,
            letters[i], cex = 1.5, font = 2
        )
        text(
            xmin + 22.5 + (i + 1) %% 2 * 5, ymax + 2.5 - (i + 1) %/% 2 * 5,
            i, cex = 0.9
        )
    }
}


#------------------------------------------------------------------------------
#' Fill quadrats
#'
#' Low-level graphic function fills quadrats specified by quadrat codes.
#'
#' @param Q quadrat codes.
#' @param ... graphic parameters passed to \code{\link[graphics]{rect}}.
#'
#' @export
#' @importFrom graphics rect
#------------------------------------------------------------------------------
fill_q <- function(Q, ...) {
    Q <- na.omit(Q)
    if (length(Q) == 0) {
        return()
    }
    r <- q_to_rect(Q)
    rect(r$x1, r$y1, r$x2, r$y2, ...)
}


#------------------------------------------------------------------------------
#' Draw quadrats as points with/without jitter.
#'
#' @param Q quadrat codes.
#' @param jitter if TRUE, add jitter for coordinates of overlapping points.
#' @param ... graphic parameters passed to \code{\link[graphics]{points}}.
#'
#' @export
#' @importFrom graphics points
#------------------------------------------------------------------------------
point_q <- function(Q, jitter = FALSE, ...) {
    Q <- na.omit(Q)
    if (length(Q) > 0) {
        return()
    }
    xy <- as.data.frame(q_to_point(Q, pos = "center"))
    point_xy(Q, xy, jitter, ...)
}


#------------------------------------------------------------------------------
#' Draw row and column as points with/without jitter.
#'
#' @param R row.
#' @param C column.
#' @param jitter if TRUE, add jitter for coordinates of overlapping points.
#' @param ... graphic parameters passed to \code{\link[graphics]{points}}.
#'
#' @export
#' @importFrom graphics points
#------------------------------------------------------------------------------
point_rc <- function(R, C, jitter = FALSE, ...) {
    xy <- rc_to_coord(R, C)
    point_xy(paste(R, C, sep = "-"), xy, jitter, ...)
}


#------------------------------------------------------------------------------
#   Draw points with/without jitter
#------------------------------------------------------------------------------
point_xy <- function(Q, xy, jitter, ...) {
    if (jitter) {
        xy$duplicated <- Q %in% Q[duplicated(Q)]
        xy$x <- ifelse(xy$duplicated, jitter(xy$x, factor = 1.1), xy$x)
        xy$y <- ifelse(xy$duplicated, jitter(xy$y, factor = 1.1), xy$y)
    }
    points(xy$x, xy$y, ...)
}


#------------------------------------------------------------------------------
#' Create a map of ogawa forest plot with colored quadrats
#'
#' @param Q
#'     quadrat codes to be filled.
#' @param adds_sq_legend
#'     if TRUE, adds an legend for sub-quadrats.
#' @param draws_1_2ha
#'     if TRUE, draws the 1.2ha core plot region.
#' @param contour
#'     if TRUE, draw contour.
#' @param col fill color(s).
#' @param type = c("rect", "point")
#'     if "rect", draws filled rectangular for each quadrat.
#'     If "point", draws points for each quadrat.
#' @param jitter
#'     if TRUE, add jitter for coordinates of overlapping points.
#'     Ignored when type is "rect".
#' @param ...
#'     graphic parameters passed to \code{\link[graphics]{rect}} to draw
#'     quadrats.
#'
#' @export
#------------------------------------------------------------------------------
create_quadrat_map <- function(
    Q, adds_sq_legend = TRUE, draws_1_2ha = TRUE, contour = FALSE,
    col = rgb(1, 0, 0, 0.5), type = c("rect", "point"), jitter = FALSE, ...
) {
    type <- match.arg(type)
    x <- create_ogawa_plot()
    if (contour) {
        draw_contour()
    }
    on.exit(add_grid(x, adds_sq_legend, draws_1_2ha))
    if (missing(Q)) {
        return()
    }
    if (type == "rect") {
        fill_q(Q, col = col, ...)
    } else {
        point_q(Q, jitter = jitter, col = col, ...)
    }
}


#------------------------------------------------------------------------------
#'   Add contour
#'
#' @export
#------------------------------------------------------------------------------
draw_contour <- function() {
    elevation <- read.csv(
        system.file("elevation.csv", package = "ogawa"), header = FALSE
    )
    elevation <- t(as.matrix(elevation))[, nrow(elevation):1]
    contour(
        z = elevation, x = seq(0, 300, by = 10), y = seq(0, 200, by = 10),
        add = TRUE, nlevels = 30, lwd = 0.5
    )
}


#------------------------------------------------------------------------------
#' Create a map of ogawa forest plot with points
#'
#' @param x x coordinates.
#' @param y y coordinates.
#' @param adds_sq_legend
#'     if TRUE, adds an legend for sub-quadrats.
#' @param draws_1_2ha
#'     if TRUE, draws the 1.2ha core plot region.
#' @param contour
#'     if TRUE, draw contour.
#' @param col fill color(s).
#' @param ...
#'     graphic parameters passed to \code{\link[graphics]{rect}} to draw
#'     quadrats.
#'
#' @export
#------------------------------------------------------------------------------
create_point_map <- function(
    x, y, adds_sq_legend = TRUE, draws_1_2ha = TRUE, contour = FALSE,
    col = rgb(1, 0, 0, 0.5), ...
) {
    p <- create_ogawa_plot()
    if (contour) {
        draw_contour()
    }
    on.exit(add_grid(p, adds_sq_legend, draws_1_2ha))
    if (missing(x) | missing(y)) {
        return()
    }
    points(x, y, col = col, ...)
}
