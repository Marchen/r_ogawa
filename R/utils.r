#------------------------------------------------------------------------------
#   A regular expression representing quadrat codes.
#------------------------------------------------------------------------------
Q_REGEXP <- "^([ZA-O]{1})([1-9]{1}|0[1-9]{1}|10|11)([a-d]{0,1})([1-4]{0,1})$"


#------------------------------------------------------------------------------
#' Returns all quadrat codes
#'
#' @return character representing all quadrat codes.
#'
#' @export
#------------------------------------------------------------------------------
all_q <- function() {
    q <- expand.grid(
        LETTERS[1:15], as.character(1:10), letters[1:4], as.character(1:4),
        stringsAsFactors = FALSE
    )
    return(standardize_q(apply(q, 1, paste0, collapse = "")))
}


#------------------------------------------------------------------------------
#' Is core plot?
#'
#' Determine whether the quadrat is in the 1.2ha  plot.
#'
#' @param Q
#'     vector of quadrat code(s), e.g., "A1b2" or "B02c3".
#' @param invalid
#'     a value returned for invalid quadrat codes including empty characters
#'     and NAs.
#'
#' @return
#'     logical indicating whether specified quadrats are in 1.2ha region.
#'     For NAs and empty characters, return NA.
#'
#' @export
#------------------------------------------------------------------------------
is_core <- function(Q, invalid = NA) {
    x <- q_to_elements(Q)
    result <- ifelse(
        !is_valid_q(Q), invalid,
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
#'     returns TRUE for quadrat codes in valid format and FALSE for invalid
#'     format.
#'
#' @export
#------------------------------------------------------------------------------
is_valid_q <- function(Q) {
    is_valid <- grepl(Q_REGEXP, Q)
    return(is_valid)
}


#------------------------------------------------------------------------------
#' Standardize quadrat codes
#'
#' Convert quadrat code(s) in the A1a1 format to the A01a1 format.
#'
#' @param Q vector of quadrat code.
#'
#' @return a character vector of quadrat code(s).
#'
#' @export
#------------------------------------------------------------------------------
standardize_q <- function(Q) {
    Q1 <- gsub("([A-O]{1})([0-9]{1,2})(.*)", "\\1", Q)
    Q2 <- as.integer(gsub("([A-O]{1})([0-9]{1,2})(.*)", "\\2", Q))
    Q3 <- gsub("([A-O]{1})([0-9]{1,2})(.*)", "\\3", Q)
    return(sprintf("%s%02i%s", Q1, Q2, Q3))
}


#------------------------------------------------------------------------------
#' Split quadrat codes into elements
#'
#' @param Q vector of quadrat code(s).
#'
#' @return
#'     a list of quadrat code elements.
#'     For invalid quadrat codes, NAs are returned.
#'
#' @export
#------------------------------------------------------------------------------
q_to_elements <- function(Q) {
    Q[!is_valid_q(Q)] <- NA
    Q1 <- gsub(Q_REGEXP, "\\1", Q)
    Q2 <- as.integer(gsub(Q_REGEXP, "\\2", Q))
    SQ1 <- gsub(Q_REGEXP, "\\3", Q)
    SQ1[SQ1 == ""] <- NA
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
construct_q <- function(Q1, Q2, SQ1, SQ2) {
    fn <- function(x) ifelse(is.na(x), "", x)
    Q2 <- ifelse(is.na(Q2), "??", sprintf("%02s", Q2))
    return(paste0(fn(Q1), Q2, fn(SQ1), fn(SQ2)))
}


#------------------------------------------------------------------------------
#' Convert quadrat codes to x coordinates
#'
#' Convert quadrat codes to x coordinates of bottom-left of the quadrats.
#' When SQ2 is not specified, returns coordinates of 10m grid.
#' When SQ1 is not specified, returns coordinates of 20m grid.
#'
#' @param Q vector of quadrat code(s).
#' @param Q1 vector of capital alphabet part of quadrat code.
#' @param Q2
#'     vector of first numerical part of quadrat code.
#'     Can be specified for intuitiveness, but not used for any calculation.
#' @param SQ1 vector of lower alphabet part of quadrat code.
#' @param SQ2 vector of second numerical part of quadrat code.
#'
#' @return a vector of x-coordinates.
#'
#' @seealso \code{\link{q_to_y}}
#' @export
#------------------------------------------------------------------------------
q_to_x <- function(Q, Q1, Q2 = NA, SQ1 = NA, SQ2 = NA) {
    if (missing(Q) & missing(Q1)) {
        stop("Needs specifying Q or Q1")
    }
    if (!missing(Q)) {
        Q1 <- q_to_elements(Q)$Q1
        SQ1 <- q_to_elements(Q)$SQ1
        SQ2 <- q_to_elements(Q)$SQ2
    } else {
        SQ2 <- as.integer(SQ2)
    }
    q1 <- setNames(-1:14, LETTERS[c(26, 1:15)])
    sq2 <- c(0, 5, 0, 5)
    x <- unname(
        q1[Q1] * 20
        + ifelse(SQ1 %in% c("a", "c", NA), 0, 10)
        + ifelse(!is.na(SQ2), sq2[SQ2], 0)
    )
    return(x)
}


#------------------------------------------------------------------------------
#' Convert quadrat codes to y coordinates
#'
#' Convert quadrat codes to y coordinates of bottom-left of the quadrats.
#' When SQ2 is not specified, returns coordinates of 10m grid.
#' When SQ1 is not specified, returns coordinates of 20m grid.
#'
#' @param Q vector of quadrat code(s).
#' @param Q1
#'     vector of capital alphabet part of quadrat code.
#'     Can be specified for intuitiveness, but not used for any calculation.
#' @param Q2 vector of first numerical part of quadrat code.
#' @param SQ1 vector of lower alphabet part of quadrat code.
#' @param SQ2 vector of second numerical part of quadrat code.
#'
#' @return a vector of y-coordinates.
#'
#' @seealso \code{\link{q_to_x}}
#' @export
#------------------------------------------------------------------------------
q_to_y <- function(Q, Q1 = NA, Q2, SQ1 = NA, SQ2 = NA) {
    if (missing(Q) & missing(Q2)) {
        stop("Needs specifying Q or Q2")
    }
    if (!missing(Q)) {
        Q2 <- q_to_elements(Q)$Q2
        SQ1 <- q_to_elements(Q)$SQ1
        SQ2 <- q_to_elements(Q)$SQ2
    }
    sq1 <- c(a = 10, b = 10, c = 0, d = 0)
    sq2 <- c(5, 5, 0, 0)
    y <- unname(
        200 - Q2 * 20
        + ifelse(is.na(SQ1), 0, sq1[SQ1])
        + ifelse(is.na(SQ2), 0, sq2[SQ2])
    )
    return(y)
}


#------------------------------------------------------------------------------
#' Convert quadrat codes to coordinates of points
#'
#' @param Q
#'     vector of quadrat code(s). When Q is specified, Q1, Q2, SQ1 and Q2 are
#'     ignored.
#' @param Q1
#'     vector of capital alphabet part of quadrat code.
#' @param Q2
#'     vector of first numerical part of quadrat code.
#' @param SQ1
#'     vector of lower alphabet part of quadrat code.
#' @param SQ2
#'     vector of second numerical part of quadrat code.
#' @param pos
#'     position of the quadrat, can be one of "center", "topleft", "topright",
#'     "bottomleft", "bottomright".
#'
#' @return list of x and y coordinates of the point(s).
#'
#' @export
#------------------------------------------------------------------------------
q_to_point <- function(
    Q, Q1 = NA, Q2 = NA, SQ1 = NA, SQ2 = NA,
    pos = c("center", "topleft", "topright", "bottomleft", "bottomright")
) {
    x <- q_to_x(Q, Q1, Q2, SQ1, SQ2)
    y <- q_to_y(Q, Q1, Q2, SQ1, SQ2)
    pos <- match.arg(pos)
    if (!missing(Q)) {
        SQ1 <- q_to_elements(Q)$SQ1
        SQ2 <- q_to_elements(Q)$SQ2
    }
    shift <- ifelse(is.na(SQ1), 20, ifelse(is.na(SQ2), 10, 5))
    x <- x + switch(
        pos, center = shift / 2, topleft = shift, topright = 0,
        bottomleft = shift, bottomright = 0
    )
    y <- y + switch(
        pos, center = shift / 2, topleft = 0, topright = 0,
        bottomleft = shift, bottomright = shift
    )
    return(list(x = x, y = y))
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
#' @return
#'     list of coordinates (x1, y1, x2, y2) for the rectangular,
#'     where (x1, y1) is bottom-left, (x2, y2) is top-right.
#'
#' @export
#------------------------------------------------------------------------------
q_to_rect <- function(Q = NA, Q1 = NA, Q2 = NA, SQ1 = NA, SQ2 = NA) {
    x1 <- q_to_x(Q, Q1, Q2, SQ1, SQ2)
    y1 <- q_to_y(Q, Q1, Q2, SQ1, SQ2)
    if (!missing(Q)) {
        SQ1 <- q_to_elements(Q)$SQ1
        SQ2 <- q_to_elements(Q)$SQ2
    }
    size <- ifelse(is.na(SQ1), 20, ifelse(is.na(SQ2), 10, 5))
    x2 <- x1 + size
    y2 <- y1 + size
    return(list(x1 = x1, x2 = x2, y1 = y1, y2 = y2))
}


#------------------------------------------------------------------------------
#' Check if quadrat pairs touch each other
#'
#' @param q1
#'     quadrat codes, having length of one, or equal length with q2.
#' @param q2
#'     quadrat codes, having length of one, or equal length with q1.
#' @param corner
#'     if TRUE, quadrats touch only at their corners also treated as touched.
#'
#' @return
#'     Returns TRUE if the two plots touch each other and FALSE if do not.
#'
#' @examples
#' q1 <- c("A1a2", "A1a2", "A1a1")
#' q2 <- c("A1a4", "B1a2", "A1a3")
#' touches(q1, q2)
#'
#' @export
#------------------------------------------------------------------------------
touches <- function(q1, q2, corner = FALSE) {
    # Check validity of quadrat codes.
    if (any(!is_valid_q(c(q1, q2)))) {
        stop("Invalid quadrat codes were given for q1 and/or q2.")
    }
    # If length of q1 or q2 is 1,
    # repeat q1 or q2 to have same length with the other.
    if (length(q1) == 1) {
        q1 <- rep(q1, length(q2))
    }
    if (length(q2) == 1) {
        q2 <- rep(q2, length(q1))
    }
    # Check length of q1 and q2.
    stopifnot(length(q1) == length(q2))
    # Check q1 and q2 intersect.
    r1 <- as.data.frame(q_to_rect(q1))
    r2 <- as.data.frame(q_to_rect(q2))
    x1 <- lapply(1:nrow(r1), function(i) c(r1$x1[i], r1$x2[i]))
    x2 <- lapply(1:nrow(r2), function(i) c(r2$x1[i], r2$x2[i]))
    y1 <- lapply(1:nrow(r1), function(i) c(r1$y1[i], r1$y2[i]))
    y2 <- lapply(1:nrow(r2), function(i) c(r2$y1[i], r2$y2[i]))
    intersect_x <- mapply(function(x, y) length(intersect(x, y)) > 0, x1, x2)
    intersect_y <- mapply(function(x, y) length(intersect(x, y)) > 0, y1, y2)
    has_intersection <- intersect_x & intersect_y
    # Check intersection at a corner.
    if (corner) {
        return (has_intersection)
    }
    return(has_intersection & !touches_at_corner(x1, x2, y1, y2))
}


#------------------------------------------------------------------------------
#   (Internal) Check if the pair(s) of quadrat touches at their corner.
#
#   @param x1 x-coordinates of the quadrat 1.
#   @param x2 x-coordinates of the quadrat 2.
#   @param y1 y-coordinates of the quadrat 1.
#   @param y2 y-coordinates of the quadrat 2.
#------------------------------------------------------------------------------
touches_at_corner <- function(x1, x2, y1, y2) {
    to_corner <- function(x, y) lapply(
        mapply(
            function(x, y) expand.grid(x = x, y = y), x, y, SIMPLIFY = FALSE
        ),
        function(x) apply(x, 1, c, simplify = FALSE)
    )
    result <- sapply(
        mapply(
            intersect, to_corner(x1, y1), to_corner(x2, y2), SIMPLIFY = FALSE
        ),
        function(x) length(x) == 1
    )
    return(result)
}


#------------------------------------------------------------------------------
#' Check all quadrats are touching each other
#'
#' @param ...
#'     quadrat codes.
#' @param corner
#'     if TRUE, quadrats contiguous at their corners are also treated as
#'     contiguous. Defaults to TRUE.
#'
#' @return Returns TRUE if all specified quadrats are contiguous.
#' @export
#------------------------------------------------------------------------------
all_touching <- function(..., corner = TRUE) {
    qs <- unique(c(...))
    if (any(!is_valid_q(qs))) {
        stop("Invalid quadrat codes were given.")
    }
    return(all(sapply(qs, touches, q2 = qs, corner = corner)))
}


#------------------------------------------------------------------------------
#' Find adjacent quadrats
#'
#' @param q character having length of 1 representing quadrat code to find.
#' @param corner
#'     if TRUE, includes quadrats touching at the corners of the quadrat.
#' @return character vector representing codes of adjacent quadrats.
#' @export
#'
#  TODO: Implement better algorithm using coordinates.
#------------------------------------------------------------------------------
find_adjacent_qs <- function(q, corner = TRUE) {
    stopifnot(length(q) == 1)
    all_qs <- all_q()
    adjacent_qs <- all_qs[touches(all_qs, rep(q, length(all_qs)))]
    adjacent_qs <- adjacent_qs[adjacent_qs != q]
    if (!corner) {
        d <- dist(as.data.frame(q_to_point(c(q, adjacent_qs))))
        adjacent_qs <- adjacent_qs[d[1:length(adjacent_qs)] == 5]
    }
    return(adjacent_qs)
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
#' is.in("A1a1", q.from = "B1a1", q.to = "B3c2")
#'
#' @export
#------------------------------------------------------------------------------
is_in <- function(
    Q, xmin = NA, xmax = NA, ymin = NA, ymax = NA, q_from = NA, q_to = NA
) {
    if (!is.na(q_from) & !is.na(q_to)) {
        if (length(q_from) != 1 | length(q_to) != 1) {
            stop("Length of 'q.from' and 'q.to' should be one.")
        }
        r1 <- q_to_rect(q_from)
        r2 <- q_to_rect(q_to)
        xmin <- min(r1$x2, r2$x2)
        xmax <- max(r1$x1, r2$x1)
        ymin <- min(r1$y2, r2$y2)
        ymax <- max(r1$y1, r2$y1)
    }
    r <- q_to_rect(Q)
    x <- xmin <= r$x1 & xmin <= r$x2 & r$x1 <= xmax & r$x2 <= xmax
    y <- ymin <= r$y1 & ymin <= r$y2 & r$y1 <= ymax & r$y2 <= ymax
    return(x & y)
}


#------------------------------------------------------------------------------
#' Validate coordinates
#'
#' @param  sq2 last digit of quadrat code.
#' @param  x x-coordinate.
#' @param  y y coordinate.
#'
#' @return TRUE if the coordinate is not valid.
#'
#' @export
#------------------------------------------------------------------------------
invalid_xy <- function(sq2, x, y) {
    result <- (
        (sq2 == 1 & (x > 5 | y < 5))
        | (sq2 == 2 & (x < 5 | y < 5))
        | (sq2 == 3 & (x > 5 | y > 5))
        | (sq2 == 4 & (x < 5 | y > 5))
    )
    return(result)
}


#' @export
rc_to_coord <- function(row, column) {
    return(list(x = (column - 1) * 10, y = 200 - (row - 1) * 10))
}
