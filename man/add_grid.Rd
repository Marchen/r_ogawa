% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotmap.r
\name{add_grid}
\alias{add_grid}
\title{Add grid lines for the plot}
\usage{
add_grid(x, adds_sq_legend = TRUE, draws_1_2ha = TRUE, grid_level = 0)
}
\arguments{
\item{x}{an \code{ogawa.plot} object.}

\item{adds_sq_legend}{if TRUE, adds an legend for sub-quadrats.}

\item{draws_1_2ha}{if TRUE, draws the 1.2ha core plot region.}

\item{grid_level}{an integer representing level of grid line to draw.
\code{0}: draw all grid lines, \code{1}: omit 5m grid lines,
\code{2}: omit 5m and 10m grid lines, and \code{4}: omit all grid lines.}
}
\description{
A low level graphic function draws grid lines for the ogawa plot map.
}
