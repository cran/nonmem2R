% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Simulation.R
\name{grid.sim.internal1}
\alias{grid.sim.internal1}
\title{Internal function}
\usage{
grid.sim.internal1(n, means = NULL, sigma, grid.param = NULL, pure.sim = FALSE)
}
\arguments{
\item{n}{Number of simulations}

\item{means}{vector of mean values}

\item{sigma}{covariance matrix}

\item{grid.param}{the index of the parameter for which a fix grid (from qnorm) is used instead of a simulated grid (rnorm).
If grid.param=NULL ( default) the fix grid will be used for the parameter with largest variance.}

\item{pure.sim}{Pure sim or use grid simulation}
}
\value{
row-matrix of parameters
}
\description{
Internal function
}
\keyword{internal}
