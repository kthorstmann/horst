% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotModeration.R
\name{plotMod}
\alias{plotMod}
\title{Moderation Plot plots theree variables}
\usage{
plotMod(data, x, y, mod, ...)
}
\arguments{
\item{data}{A dataset containint at least three variables to plot.}

\item{x}{The variable to be plotted on the x-axis.}

\item{y}{The variable to be plotted on the y-axis.}

\item{mod}{The moderator variable to be plotted in a rainbow-color.}

\item{...}{Further parameters passed on to \code{\link{plot}}.}
}
\value{
A graphical representation of a moderation, displaying each point in three variables.
}
\description{
This function plots three variables, two on each axis, one in the color dimension.
}
\details{
The variable \code{mod} is currently plotted in 5-percent quantiles for each color.
}

