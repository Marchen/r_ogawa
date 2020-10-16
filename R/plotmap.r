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
#' @param x an \code{ogawa.plot} object.
#' @param adds.sq.legend if TRUE, adds an legend for sub-quadrats.
#' @param draws.1.2ha if TRUE, draws the 1.2ha core plot region.
#'
#' @export
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics text
#------------------------------------------------------------------------------
add.grid <- function(x, adds.sq.legend = TRUE, draws.1.2ha = TRUE) {
	do.call(draw.grid, x)
	do.call(draw.labels, x)
	if (draws.1.2ha) {
		do.call(draw.1.2ha, x)
	}
	if (adds.sq.legend) {
		do.call(draw.sq.legend, x)
	}
}


#------------------------------------------------------------------------------
#  Draw grid lines.
#------------------------------------------------------------------------------
draw.grid <- function(xmin, xmax, ymin, ymax, ...) {
	rect(xmin, ymin, xmax, ymax, lwd = 2)
	for (x in (xmin / 5):(xmax / 5)) {
		for (y in (ymin / 5):(ymax / 5)) {
			segments(x * 5, ymin, x * 5, ymax, lwd = ifelse(x %% 4 == 0, 2, 1))
			segments(
				xmin, y * 5, xmax, y * 5, lwd = ifelse(y %% 4 == 0, 2, 1)
			)
		}
	}
}


#------------------------------------------------------------------------------
#  Draw labels.
#------------------------------------------------------------------------------
draw.labels <- function(xmin, xmax, ymin, ymax, label.pos.x, label.pos.y) {
	adjust <- min(abs(ymax - ymin), abs(xmax - xmin))
	# X-axis label (small).
	for (x in (xmin / 10):(xmax / 10)) {
		text(x * 10, ymax + adjust * label.pos.x[1], x + 1, cex = 0.7)
	}
	# X-axis label (large).
	for (x in (xmin / 20 + 1):(xmax / 20)) {
		text(
			x * 20 - 10, ymax + adjust * label.pos.x[2], LETTERS[x],
			cex = 1.5, font = 2
		)
	}
	# Y-axis label (small).
	for (y in (ymin / 10):(ymax / 10)) {
		text(
			xmin - adjust * label.pos.y[1], y * 10, ymax / 10 - y + 1,
			cex = 0.7
		)
	}
	# Y-axis label (large).
	for (y in (ymin / 20 + 1):(ymax / 20)) {
		text(
			xmin - adjust * label.pos.y[2],
			ymax + 10 - 2 * y * 10 + ymin, y, cex = 1.5, font = 2
		)
	}
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
