#------------------------------------------------------------------------------
#' Create new ogawa forest plot
#'
#' Create new plot for ogawa forest plot map.
#'
#' @param xmin
#' a minimum value of x coordinate.
#' @param xmax
#' a maximum value of x coordinate.
#' @param ymin
#' a minimum value of y coordinate.
#' @param ymax
#' a maximum value of y coordinate.
#' @param label.pos.x
#' a numeric vector with two elements. The first element denotes distance
#' (% of shorter axis) between small X axis labels and the grid and the
#' second denotes distance between large X axis labels and the grid.
#' @param label.pos.y
#' a numeric vector with two elements. The first element denotes distance
#' (% of shorter axis) between small Y axis labels and the grid and the
#' second denotes distance between large Y axis labels and the grid.
#'
#' @return an \code{ogara.plot} object which can be passed to other functions.
#'
#' @examples
#' x <- create.ogawa.plot()
#' add.grid(x)
#'
#' @export
#------------------------------------------------------------------------------
create.ogawa.plot <- function(
	xmin = 0, xmax = 300, ymin = 0, ymax = 200,
	label.pos.x = c(0.02, 0.05), label.pos.y = c(0.02, 0.06)
) {
	x <- create.object(xmin, xmax, ymin, ymax, label.pos.x, label.pos.y)
	adjustment <- min(abs(x$xmax - x$xmin), abs(x$ymax - x$ymin))
	x.margin <- adjustment * label.pos.y[2]
	y.margin <- adjustment * label.pos.x[2]
	plot(
		NA, type = "n", xlim = c(x$xmin - x.margin, x$xmax),
		ylim = c(x$ymin, x$ymax + y.margin),
		bty = "n", axes = FALSE, xlab = "", ylab = ""
	)
	return(x)
}


#------------------------------------------------------------------------------
#  Create ogawa.plot object.
#------------------------------------------------------------------------------
create.object <- function(xmin, xmax, ymin, ymax, label.pos.x, label.pos.y) {
	mod5 <- function(x) x - (x %% 5)
	object <- list(
		xmin = mod5(xmin), xmax = mod5(xmax), ymin = mod5(ymin),
		ymax = mod5(ymax), label.pos.x = label.pos.x, label.pos.y = label.pos.y
	)
	class(object) <- "ogawa.plot"
	return(object)
}


#------------------------------------------------------------------------------
#' Add grid lines for the plot
#'
#' A low level graphic function draws grid lines for the ogawa plot map.
#'
#' @param x
#' an \code{ogawa.plot} object.
#' @param adds.sq.legend
#' if TRUE, adds an legend for sub-quadrats.
#' @param draws.1.2ha
#' if TRUE, draws the 1.2ha core plot region.
#' @param grid.level
#' an integer representing level of grid line to draw.
#' \code{0}: draw all grid lines, \code{1}: omit 5m grid lines,
#' \code{2}: omit 5m and 10m grid lines, and \code{4}: omit all grid lines.
#'
#' @export
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics text
#' @importFrom grDevices dev.hold
#' @importFrom grDevices dev.flush
#------------------------------------------------------------------------------
add.grid <- function(
	x, adds.sq.legend = TRUE, draws.1.2ha = TRUE, grid.level = 0
) {
	stopifnot(grid.level %in% 0:3)
	dev.hold()
	do.call(
		draw.grid, c(x, list(grid.level = grid.level))
	)
	do.call(draw.labels, x)
	if (draws.1.2ha) {
		do.call(draw.1.2ha, x)
	}
	if (adds.sq.legend) {
		do.call(draw.sq.legend, x)
	}
	dev.flush()
}


#------------------------------------------------------------------------------
#  Draw grid lines.
#------------------------------------------------------------------------------
draw.grid <- function(xmin, xmax, ymin, ymax, grid.level, ...) {
	# Draw outline.
	rect(xmin, ymin, xmax, ymax, lwd = 2)
	# Prepare grid lines.
	x <- (xmin / 5):(xmax / 5)
	v.lines <- data.frame(
		x1 = x * 5, y1 = ymin, x2 = x * 5, y2 = ymax,
		lwd = ifelse(x %% 4 == 0, 2, 1),
		level = ifelse(x %% 4 == 0, 3, ifelse(x %% 2 == 0, 2, 1))
	)
	y <- (ymin / 5):(ymax / 5)
	h.lines <- data.frame(
		x1 = xmin, y1 = y * 5, x2 = xmax, y2 = y * 5,
		lwd = ifelse(y %% 4 == 0, 2, 1),
		level = ifelse(y %% 4 == 0, 3, ifelse(y %% 2 == 0, 2, 1))
	)
	l <- rbind(v.lines, h.lines)
	# Filter grid lines.
	l <- l[l$level > grid.level, ]
	# Draw lines.
	segments(l$x1, l$y1, l$x2, l$y2, lwd = l$lwd)
}


#------------------------------------------------------------------------------
#  Draw labels.
#------------------------------------------------------------------------------
draw.labels <- function(
	xmin, xmax, ymin, ymax, label.pos.x, label.pos.y, ...
) {
	adjust.x <- abs(xmax - xmin)
	adjust.y <- abs(ymax - ymin)
	# X-axis label (small).
	x <- (xmin / 10):(xmax / 10)
	xs <- data.frame(
		x = x * 10, y = ymax + adjust.y * label.pos.x[1], labels = x + 1,
		cex = 0.7, font = 1
	)
	# X-axis label (large).
	x <- (xmin / 20 + 1):(xmax / 20)
	xl <- data.frame(
		x = x * 20 - 10, y = ymax + adjust.y * label.pos.x[2],
		labels = LETTERS[x], cex = 1.5, font = 2
	)
	# Y-axis label (small).
	y <- (ymin / 10):(ymax / 10)
	ys <- data.frame(
		x = xmin - adjust.x * label.pos.y[1], y = y * 10,
		labels = ymax / 10 - y + 1, cex = 0.7, font = 1
	)
	# Y-axis label (large).
	y <- (ymin / 20 + 1):(ymax / 20)
	yl <- data.frame(
		x = xmin - adjust.x * label.pos.y[2],
		y = ymax + 10 - 2 * y * 10 + ymin, labels = y, cex = 1.5, font = 2
	)
	do.call(text, rbind(xs, xl, ys, yl))
}


#------------------------------------------------------------------------------
#  Draw 1.2ha region.
#------------------------------------------------------------------------------
draw.1.2ha <- function(xmin, xmax, ymin, ymax, ...) {
	rect(
		max(xmin, 120), max(ymin, 60), min(xmax, 220), min(ymax, 180), lwd = 5
	)
}


#------------------------------------------------------------------------------
#  Draw sub-quadrat region.
#------------------------------------------------------------------------------
draw.sq.legend <- function(xmin, xmax, ymin, ymax, ...) {
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
#' Low-level graphic function fills quadrats specifyied by quadrat codes.
#'
#' @param Q quadrat codes.
#' @param ... graphic parameters passed to \code{\link[graphics]{rect}}.
#'
#' @export
#' @importFrom graphics rect
#------------------------------------------------------------------------------
fill.q <- function(Q, ...) {
	r <- q.to.rect(Q)
	rect(r$x1, r$y1, r$x2, r$y2, ...)
}

#------------------------------------------------------------------------------
#' Create a map of ogawa forest plot with colored quadrats
#'
#' @param Q
#' quadrat codes to be filled.
#' @param adds.sq.legend
#' if TRUE, adds an legend for sub-quadrats.
#' @param draws.1.2ha
#' if TRUE, draws the 1.2ha core plot region.
#' @param ...
#' graphic parameters passed to \code{\link[graphics]{rect}} to draw quadrats.
#'
#' @export
#------------------------------------------------------------------------------
create.quadrat.map <- function(
	Q, adds.sq.legend = TRUE, draws.1.2ha = TRUE, ...
) {
	x <- create.ogawa.plot()
	fill.q(Q, ...)
	add.grid(x, adds.sq.legend, draws.1.2ha)
}
