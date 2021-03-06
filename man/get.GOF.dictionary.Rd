% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GOF.control.R
\name{get.GOF.dictionary}
\alias{get.GOF.dictionary}
\alias{set.GOF.dictionary}
\alias{default.GOF.dictionary}
\alias{set.GOF.params}
\alias{get.GOF.params}
\alias{default.GOF.params}
\alias{set.ETA.labels}
\alias{set.script.name}
\alias{GOF.control}
\title{Get, set, and modify the global (or local) GOF parameters and GOF dictionary for labels}
\usage{
get.GOF.dictionary()

set.GOF.dictionary(...)

default.GOF.dictionary()

set.GOF.params(...)

get.GOF.params()

default.GOF.params()

set.ETA.labels(labels)

set.script.name(script.name)

GOF.control(...)
}
\arguments{
\item{...}{named list of GOF parameter or GOF dictionary for labels}

\item{labels}{vector of character strings with the labels}

\item{script.name}{character strings for script name to put at end of caption}
}
\description{
The current global GOF parameters and dictionary is automatically applied to every plot you draw.
Use `get.GOF.params` to get the current GOF parameters, and `set.GOF.params` to change
one or several parameters.
To change settings for one GOF only you can use GOF.control as input to the GOF function.
See details below for complete list of GOF parameters.

Similarly use `get.GOF.dictionary` and `set.GOF.dictionary` for the dictionary of labels.
`set.GOF.dictionary` also allows for adding new items to the dictionary.

Use `set.ETA.labels` to set labels for ETA's in all plots
}
\details{
Table below describe each of the global GOF parameters that can be modified by `set.GOF.params`
\tabular{lll}{
\strong{Parameter} \tab \strong{Default} \tab \strong{Description} \cr
col.data \tab "gray20" \tab  color for points in all GOFs  \cr
cex.data \tab 1.5 \tab       cex for points in all GOFs \cr
pch.data \tab 19 \tab        pch for points in all GOFs \cr
alpha.data \tab 0.5 \tab     alpha for points in all GOFs \cr
col.smooth \tab "#3366FF" \tab   color for smoothers  \cr
lty.smooth \tab 1 \tab   lty for smoothers \cr
lwd.smooth \tab 1 \tab   lwd for smoothers \cr
se.smooth \tab TRUE \tab show confidence interval for loess smoother (TRUE) or not (FALSE)  \cr
span.smooth \tab 0.6666667 \tab  span parameter for loess smothers  \cr
degree.smooth \tab 1 \tab    degree  parameter for loess smothers \cr
family.smooth \tab "symmetric" \tab  family parameter for loess smothers  \cr
col.refline \tab "red" \tab   color for reference lines in all GOFs \cr
lty.refline \tab 2 \tab    lty for reference lines in all GOFs \cr
lwd.refline \tab 1 \tab    lwd for reference lines in all GOFs \cr
fill.hist \tab "gray30" \tab  color for filling of histograms   \cr
col.hist \tab "transparent" \tab color for border of histograms   \cr
alpha.hist \tab 0.5 \tab alpha for fill area in histograms   \cr
fill.box \tab "steelblue" \tab  color for filling of box-plots   \cr
col.box \tab "black" \tab color for border and whisker of box-plots   \cr
alpha.box \tab 0.6 \tab alpha for fill area in box-plots   \cr
axis.labels \tab TRUE \tab   use dictionary for labels (TRUE) or just use column names for axis labels (FALSE) \cr
add.caption \tab TRUE \tab   add caption to GOF plots (true) or not (FALSE) \cr
size.caption \tab 8 \tab    size for caption text \cr
col.caption \tab 1 \tab    color for caption text \cr
caption.path.depth \tab 99 \tab  number of folders levels to include in caption  \cr
corr.fontface \tab 4 \tab    font face for correlation in eta.pairs.GOF \cr
eta.labels \tab NULL \tab    vector of labels for ETA's\cr
script.name \tab "" \tab     script name for caption\cr
}
}
\examples{
### Example setting ETA labels
set.ETA.labels(c("Ka","CL","V"))
### Example setting script name
set.script.name("MyScript.R")
}
