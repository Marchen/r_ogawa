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
	fn <- function(Q) {
		if (is.na(Q) | Q == "") {
			return(NA)
		}
		Q1 <- toupper(substr(Q, 1, 1))
		Q2 <- as.integer(substr(Q, 2, 3))
		return(Q1 %in% LETTERS[7:11] & Q2 >= 2 & Q2 <= 7)
	}
	return(sapply(Q, fn))
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
#' @return a list of quadrat code elements.
#'
#' @export
#------------------------------------------------------------------------------
q.to.elements <- function(Q) {
	q.regexp <- "^([A-Z]{1})([0-9]{1,2})([a-d]{1})([1-4]{0,1})$"
	is.wrong.format <- !grepl(q.regexp, Q)
	if (any(is.wrong.format)) {
		stop(sprintf("Wrong Q format: %s", Q[is.wrong.format]))
	}
	Q1 <- gsub(q.regexp, "\\1", Q)
	Q2 <- as.integer(gsub(q.regexp, "\\2", Q))
	SQ1 <- gsub(q.regexp, "\\3", Q)
	SQ2 <- as.integer(gsub(q.regexp, "\\4", Q))
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
