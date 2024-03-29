% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_GOFs.R
\name{basic.GOF4}
\alias{basic.GOF4}
\alias{basic.GOF6}
\title{Basic 4- and 6-panel GOF}
\usage{
basic.GOF4(
  data,
  residual = "CWRES",
  idv1 = "IPRED",
  idv2 = "TIME",
  title = "",
  color = "",
  log.scale = FALSE,
  global.ggplot.options = NULL,
  refline = TRUE,
  add.loess = TRUE,
  control = GOF.control()
)

basic.GOF6(
  data,
  residual = "CWRES",
  idv1 = "IPRED",
  idv2 = "TIME",
  title = "",
  color = "",
  log.scale = FALSE,
  global.ggplot.options = NULL,
  refline = TRUE,
  add.loess = TRUE,
  bins = NA,
  control = GOF.control()
)
}
\arguments{
\item{data}{data.frame to plot}

\item{residual}{column name for residuals, default="CWRES"}

\item{idv1}{independent variable for plot of sqrt of absolute CWRES}

\item{idv2}{independent variable for plot of CWRES}

\item{title}{title}

\item{color}{data columns to set different colors in plot, interpreted as factor}

\item{log.scale}{use log scale for DV, IPRED and PRED (TRUE) or normal scale (FALSE)}

\item{global.ggplot.options}{ggplot option added to  each ggplot before plotting/returning object}

\item{refline}{add reference line (default =TRUE) or not (FALSE)}

\item{add.loess}{add loess smoother to plot (TRUE), or not (FALSE)}

\item{control}{an optional list of control settings. See GOF.control for the names of the settable control values and their effect.}

\item{bins}{number of histogram bins, if NA, bins are set using Sturges' formula}
}
\description{
basic.GOF4 provides a 4-panel GOF plot showing \cr
i) Observations (DV) vs population predictions (PRED) \cr
ii) Observations (DV) vs individual predictions (IPRED) \cr
iii) sqrt(abs(CWRES)) vs individual predictions (IPRED), and \cr
iV) CWRES vs TIME or other columns as set by idv \cr

basic.GOF6 provides 2 additional GOF's showing histogram and qqnorm GOF's for CWRES.

Caption is added as reurned by get.caption
}
\examples{
# Get path to the example files included in nonmem2R package
file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
sdtab<-read.table(file=file1,skip=1,header=TRUE)
set.script.name("MyScript.R")
## Example 4 panel basic GOF
basic.GOF4(subset(sdtab,DV>0),idv2="TAPD")
## Example 6 panel basic GOF
basic.GOF6(subset(sdtab,DV>0),idv1="TAPD",idv2="PRED")
}
