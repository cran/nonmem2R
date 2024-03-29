% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_GOFs.R
\name{do.individual.GOF}
\alias{do.individual.GOF}
\title{Individual GOF-plots}
\usage{
do.individual.GOF(
  data,
  x = "TIME",
  y = c("DV", "IPRED", "PRED"),
  type = c("point", "line", "line"),
  title = "",
  per.page = 20,
  fx = NULL,
  fy = NULL,
  ylab = "Observations/Predictions",
  equal.lims = TRUE,
  global.ggplot.options = NULL,
  control = GOF.control()
)
}
\arguments{
\item{data}{data.frame to plot}

\item{x}{character string with name of column for x}

\item{y}{character string vector with names of columns for y, default = c(DV, IPRED , PRED)}

\item{type}{plot symbol or line for each y variable, length of type must match length of y}

\item{title}{title}

\item{per.page}{number of subjects/panels on each page}

\item{fx}{function for transformation of x before plotting}

\item{fy}{function for transformation of y before plotting}

\item{ylab}{y axis label}

\item{equal.lims}{use same x- and y-limits on all pages and panels (TRUE), or use allow to vary across pages (FALSE)}

\item{global.ggplot.options}{ggplot option added to  each ggplot before plotting/returning object}

\item{control}{an optional list of control settings. See GOF.control for the names of the settable control values and their effect.}
}
\description{
DV, IPRED and PRED (default) individual GOF plots with one panel per subject.
Caption added as reurned by get.caption
}
\examples{
# Get path to the example files included in nonmem2R package
file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
sdtab<-read.table(file=file1,skip=1,header=TRUE)
set.script.name("MyScript.R")
do.individual.GOF(subset(sdtab,DV>0 & ID<13))
}
