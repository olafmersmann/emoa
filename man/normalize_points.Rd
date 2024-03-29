% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicators.r
\name{normalize_points}
\alias{normalize_points}
\title{Scale point cloud}
\usage{
normalize_points(points, minval, maxval)
}
\arguments{
\item{points}{Matrix containing points, one per column.}

\item{minval}{Optional lower limits for the new bounding box.}

\item{maxval}{Optional upper limits for the new bounding box.}
}
\value{
Scaled points.
}
\description{
Rescale all points to lie in the box bounded by \code{minval}
and \code{maxval}.
}
\author{
Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}
}
