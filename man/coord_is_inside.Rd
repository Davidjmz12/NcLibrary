% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordinates_functions.R
\name{coord_is_inside}
\alias{coord_is_inside}
\title{Coordinate point is inside the grid.}
\usage{
coord_is_inside(coordinate, longitude_v, latitude_v)
}
\arguments{
\item{coordinate}{coordinate point}

\item{longitude_v}{longitude array}

\item{latitude_v}{latitude array}
}
\value{
TRUE if the coordinate point is inside the grid.
}
\description{
Check if a coordinate limit is outside a grid of points
}
\examples{
coord_is_inside(c(2,3),c(0,10),c(0,10))
}
