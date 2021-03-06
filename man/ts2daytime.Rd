% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts2daytime.R
\name{ts2daytime}
\alias{ts2daytime}
\title{Calculate solar elevation or daytime.}
\usage{
ts2daytime(long, lat, ts, dawn_degree = 12, type = "daytime")
}
\arguments{
\item{long}{A vector with longitude coordinates in WGS84.}

\item{ts}{A vector with timestamps in POSIXct.}

\item{dawn_degree}{A integer defining sun elevation below the horizon at
start of dawn and end of dusk (12 refers to a sun elevation of -12 degree)}

\item{type}{Either "daytime" or "elevation".}

\item{long}{A vector with latitude coordinates in WGS84.}
}
\value{
A vector with daytimes ("night", "dawn", "day", "dusk") when type =
  "daytime" or a vector with sun elevation in degrees when type = "elevation".
}
\description{
\code{ts2daytime} calculates solar elevation or daytime for a given timestamp
  and location on earth.
}
\examples{
 ts2daytime(lon = 7.82,
             lat = 48.00,
             ts = as.POSIXct("2017-01-01 12:00:00"),
             type = "daytime",
             dawn_degree = 12)

}
