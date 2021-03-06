% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.activity.R
\name{plot.activity}
\alias{plot.activity}
\title{Plotting method for \code{activity} objects}
\usage{
\method{plot}{activity}(x, select = "activity", threshold = NULL,
  select_animal_id = NULL, ...)
}
\arguments{
\item{x}{An object of class \code{activity}. Depending on choice of plot, it
should containing \code{$ctivity_data} or aggregated activity thresholds
(\code{$activity_thresholds_aggregated}), and optionally \code{gps_data}.}

\item{select}{Type of plot, either 'activity', 'activity_eval', 'thresholds' or 'thresholds'.}
}
\description{
\code{plot.activity} creates plots for activity object.
}
