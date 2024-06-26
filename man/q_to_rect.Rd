% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{q_to_rect}
\alias{q_to_rect}
\title{Convert quadrat code to coordinates of rectangular for the quadrat}
\usage{
q_to_rect(Q = NA, Q1 = NA, Q2 = NA, SQ1 = NA, SQ2 = NA)
}
\arguments{
\item{Q}{vector of quadrat code(s).}

\item{Q1}{vector of capital alphabet part of quadrat code.}

\item{Q2}{vector of first numerical part of quadrat code.}

\item{SQ1}{vector of lower alphabet part of quadrat code.}

\item{SQ2}{vector of second numerical part of quadrat code.}
}
\value{
list of coordinates (x1, y1, x2, y2) for the rectangular,
    where (x1, y1) is bottom-left, (x2, y2) is top-right.
}
\description{
Convert quadrat code to coordinates of rectangular for the quadrat
}
