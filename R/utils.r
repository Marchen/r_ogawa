#------------------------------------------------------------------------------
# A regular expression representing quadrat codes.
#------------------------------------------------------------------------------
Q_REGEXP = "^([A-O]{1})([1-9]{1}|0[1-9]{1}|10)([a-d]{1})([1-4]{0,1})$"


#------------------------------------------------------------------------------
#' Is core plot?
#'
#' Determine whether the quadrat is in the 1.2ha  plot.
#'
#' @param Q
#' vector of quadrat code(s), e.g., "A1b2" or "B02c3".
#' @param invalid
#' a value returned for invalid quadrat codes including empty characters and
#' NAs.
#'
#' @return
#' logical indicating whether specified quadrats are in 1.2ha region.
#' For NAs and empty characters, return NA.
#'
#' @export
#------------------------------------------------------------------------------
is.core <- function(Q, invalid = NA) {
	x <- q.to.elements(Q)
	result <- ifelse(
		!is.valid.q(Q), invalid,
		x$Q1 %in% LETTERS[7:11] & x$Q2 >= 2 & x$Q2 <= 7
	)
	return(result)
}


#------------------------------------------------------------------------------
#' Validate quadrat codes
#'
#' @param Q quadrat code(s).
#'
#' @return
#' returns TRUE for quadrat codes in valid format and FALSE for invalid format.
#'
#' @export
#------------------------------------------------------------------------------
is.valid.q <- function(Q) {
	is.valid <- grepl(Q_REGEXP, Q)
	return(is.valid)
}


#------------------------------------------------------------------------------
#' Standardize quadrat codes
#'
#' Convert quadrat code(s) in the A1a1 format to the A01a1 format.
#'
#' @param Q vector of quadrat code.
#'
#' @return a character vector of quadrate code(s).
#'
#' @export
#------------------------------------------------------------------------------
standardize.q <- function(Q) {
	Q1 <- gsub("([A-O]{1})([0-9]{1,2})(.*)", "\\1", Q)
	Q2 <- as.integer(gsub("([A-O]{1})([0-9]{1,2})(.*)", "\\2", Q))
	Q3 <- gsub("([A-O]{1})([0-9]{1,2})(.*)", "\\3", Q)
	return(sprintf("%s%02s%s", Q1, Q2, Q3))
}


#------------------------------------------------------------------------------
#' Split quadrat codes into elements
#'
#' @param Q vector of quadrat code(s).
#'
#' @return
#' a list of quadrat code elements.
#' For invalit quadrat codes, NAs are returned.
#'
#' @export
#------------------------------------------------------------------------------
q.to.elements <- function(Q) {
	Q[!is.valid.q(Q)] <- NA
	Q1 <- gsub(Q_REGEXP, "\\1", Q)
	Q2 <- as.integer(gsub(Q_REGEXP, "\\2", Q))
	SQ1 <- gsub(Q_REGEXP, "\\3", Q)
	SQ2 <- as.integer(gsub(Q_REGEXP, "\\4", Q))
	return(list(Q1 = Q1, Q2 = Q2, SQ1 = SQ1, SQ2 = SQ2))
}


#------------------------------------------------------------------------------
#' Construct quadrat codes from elements.
#'
#' Construct quadrat codes from elements (Q1, Q2, SQ1, SQ2).
#'
#' @param Q1 vector of capital alphabet part of quadrat code.
#' @param Q2 vector of first numerical part of quadrat code.
#' @param SQ1 vector of lower alphabet part of quadrat code.
#' @param SQ2 vector of second numerical part of quadrat code.
#'
#' @return a character vector of quadrat code(s).
#'
#' @export
#------------------------------------------------------------------------------
construct.q <- function(Q1, Q2, SQ1, SQ2) {
	fn <- function(x) ifelse(is.na(x), "", x)
	Q2 <- ifelse(is.na(Q2), "??", sprintf("%02s", Q2))
	return(paste0(fn(Q1), Q2, fn(SQ1), fn(SQ2)))
}


#------------------------------------------------------------------------------
#' Convert quadrat codes to x coordinates
#'
#' Convert quadrat codes to x coordinates of top-right of the quadrats.
#'
#' @param Q vector of quadrat code(s).
#' @param Q1 vector of capital alphabet part of quadrat code.
#' @param Q2 vector of first numerical part of quadrat code.
#' @param SQ1 vector of lower alphabet part of quadrat code.
#' @param SQ2 vector of second numerical part of quadrat code.
#'
#' @return a vector of x-coordinates.
#'
#' @seealso \code{\link{q.to.y}}
#' @export
#------------------------------------------------------------------------------
q.to.x <- function(Q, Q1 = NA, Q2 = NA, SQ1 = NA, SQ2 = NA) {
	if (!missing(Q)) {
		params <- q.to.elements(Q)
		Q1 <- params$Q1
		Q2 <- params$Q2
		SQ1 <- params$SQ1
		SQ2 <- params$SQ2
	}
	if (any(is.na(c(Q1, Q2, SQ1)))) {
		stop("Needs specifying Q or all of Q1, Q2, SQ1 (and SQ2)")
	}
	x <- (
		(as.integer(sapply(Q1, charToRaw)) - 65) * 20
		+ ifelse(SQ1 %in% c("a", "c"), 0, 1) * 10
		+ ifelse(!is.na(SQ2), (SQ2 - 1) %% 2 + 1, 2) * 5
	)
	return(x)
}


#------------------------------------------------------------------------------
#' Convert quadrat codes to y coordinates
#'
#' Convert quadrat codes to y coordinates of top-right of the quadrats.
#'
#' @param Q vector of quadrat code(s).
#' @param Q1 vector of capital alphabet part of quadrat code.
#' @param Q2 vector of first numerical part of quadrat code.
#' @param SQ1 vector of lower alphabet part of quadrat code.
#' @param SQ2 vector of second numerical part of quadrat code.
#'
#' @return a vector of y-coordinates.
#'
#' @seealso \code{\link{q.to.x}}
#' @export
#------------------------------------------------------------------------------
q.to.y <- function(Q, Q1 = NA, Q2 = NA, SQ1 = NA, SQ2 = NA) {
	if (!missing(Q)) {
		params <- q.to.elements(Q)
		Q1 <- params$Q1
		Q2 <- params$Q2
		SQ1 <- params$SQ1
		SQ2 <- params$SQ2
	}
	if (any(is.na(c(Q1, Q2, SQ1)))) {
		stop("Needs specifying Q or all of Q1, Q2, SQ1 and SQ2")
	}
	sq1 <- c(a = 0, b = 0, c = 1, d = 1)
	y <- 200 - (Q2 - 1) * 20 - sq1[SQ1] * 10
	y <- y - ifelse(is.na(SQ2), 0, (floor((SQ2 - 1) / 2) * 5))
	names(y) <- NULL
	return(y)
}


#------------------------------------------------------------------------------
#' Convert quadrat code to coordinates of rectangular for the quadrat
#'
#' @param Q vector of quadrat code(s).
#' @param Q1 vector of capital alphabet part of quadrat code.
#' @param Q2 vector of first numerical part of quadrat code.
#' @param SQ1 vector of lower alphabet part of quadrat code.
#' @param SQ2 vector of second numerical part of quadrat code.
#'
#' @return list of coordinates (x1, x2, y1, y2) for the rectangular.
#'
#' @export
#------------------------------------------------------------------------------
q.to.rect <- function(Q, Q1 = NA, Q2 = NA, SQ1 = NA, SQ2 = NA) {
	if (!missing(Q)) {
		params <- q.to.elements(Q)
		Q1 <- params$Q1
		Q2 <- params$Q2
		SQ1 <- params$SQ1
		SQ2 <- params$SQ2
	}
	x1 <- q.to.x(Q1 = Q1, Q2 = Q2, SQ1 = SQ1, SQ2 = SQ2)
	y1 <- q.to.y(Q1 = Q1, Q2 = Q2, SQ1 = SQ1, SQ2 = SQ2)
	x2 <- x1 - ifelse(is.na(SQ2), 10, 5)
	y2 <- y1 - ifelse(is.na(SQ2), 10, 5)
	return(list(x1 = x1, x2 = x2, y1 = y1, y2 = y2))
}


#------------------------------------------------------------------------------
#' Check if the plot pairs are adjacent
#'
#' @param q1 quadrat codes, should have equal length with q2.
#' @param q2 quadrat codes, should have equal length with q1.
#'
#' @return Returns TRUE if the two plots are adjacent and FALSE if are not.
#'
#' @examples
#' q1 <- c("A1a2", "A1a2", "A1a1")
#' q2 <- c("A1a4", "B1a2", "A1a3")
#' is.adjacent(q1, q2)
#'
#' @export
#------------------------------------------------------------------------------
is.adjacent <- function(q1, q2) {
	stopifnot(length(q1) == length(q2))
	r1 <- as.data.frame(q.to.rect(q1))
	r2 <- as.data.frame(q.to.rect(q2))
	x1 <- lapply(1:nrow(r1), function(i) c(r1$x1[i], r1$x2[i]))
	x2 <- lapply(1:nrow(r2), function(i) c(r2$x1[i], r2$x2[i]))
	y1 <- lapply(1:nrow(r1), function(i) c(r1$y1[i], r1$y2[i]))
	y2 <- lapply(1:nrow(r2), function(i) c(r2$y1[i], r2$y2[i]))
	intersect.x <- mapply(function(x, y) length(intersect(x, y)) > 0, x1, x2)
	intersect.y <- mapply(function(x, y) length(intersect(x, y)) > 0, y1, y2)
	return(intersect.x & intersect.y)
}


#------------------------------------------------------------------------------
#' Check if quadrats are in a specified region
#'
#' Check if quadrats are in a specified region.
#' The region can be specified by using minimum and maximum coordinates or two
#' quadrats indicating starting from and to.
#'
#' @param Q quadrate codes.
#' @param xmin minimum x-coordinate of the region.
#' @param xmax maximum x-coordinate of the region.
#' @param ymin minimum y-coordinate of the region.
#' @param ymax maximum y-coordinate of the region.
#' @param q.from a quadrat code specifying the region begins.
#' @param q.to a quadrat code specifying the region ends.
#'
#' @return a logical vector indicating if the quadrats are in the region.
#'
#' @examples
#' is.in("A1a1", 0, 100, 0, 200)
#' is.in("A1a1", q.from="B1a1", q.to = "B3c2")
#'
#' @export
#------------------------------------------------------------------------------
is.in <- function(
	Q, xmin = NA, xmax = NA, ymin = NA, ymax = NA, q.from = NA, q.to = NA
) {
	if (!is.na(q.from) & !is.na(q.to)) {
		if (length(q.from) != 1 | length(q.to) != 1) {
			stop("Length of 'q.from' and 'q.to' should be one.")
		}
		r1 <- q.to.rect(q.from)
		r2 <- q.to.rect(q.to)
		xmin <- min(r1$x2, r2$x2)
		xmax <- max(r1$x1, r2$x1)
		ymin <- min(r1$y2, r2$y2)
		ymax <- max(r1$y1, r2$y1)
	}
	r <- q.to.rect(Q)
	x <- xmin <= r$x1 & xmin <= r$x2 & r$x1 <= xmax & r$x2 <= xmax
	y <- ymin <= r$y1 & ymin <= r$y2 & r$y1 <= ymax & r$y2 <= ymax
	return(x & y)
}
