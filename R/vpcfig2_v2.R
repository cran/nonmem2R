############################################################################################################
############################################################################################################
#' Visual Predictive Check (VPC) based on Perl-speaks-NONMEM (PsN) generated VPC files (ggplot2-version).
#' @description
#' This function creates VPC using output files from the vpc command in Pearl Speaks NONMEM (PsN).
#' Graphs are generated using the ggplot2 package and the return object is an ggplot class and
#' can be further modified, see details below and documentation for ggplot2 for further details.
#' @param vpcdir
#' Path of directory of the VPC files
#' @param vpctab
#' Path to the vpctab-file
#' @param vpcresult
#' Path to the vpcresult file
#' @param use.model.path
#' Load file from a global defined model library (TRUE=default).
#' If so will look for a global character vector named \code{model.path}
#' @param strata.names
#' Character vector for strata names. Must have length equal to number of strata's in vpc files, otherwise ignored.
#' @param strata.subset
#' Vector specifying subset of strata to use. Either a vector of index, e.g.  strata.subset=c(1,3),
#' or a character vector naming which strata to use. See details.
#' @param median.only
#' Logical to plot only median and no quantiles for model and data (FALSE=default)
#' @param bin.idv
#' Method for computed idv value for plotting bin summary values.
#' Set to "median" to use median of independent variable in each bin on x-axis (default),
#' or set to "midpoint" to use midpoint of bins on x-axis.
#' @param percentile
#' percentile to use, default=10 will display 10% 50% and 90% percentile for model and data.
#' Percentile argument must match columns included in the vpcresult file.
#' @param xlab
#' label for x-axis, default is IDV as found in vpcresultfile, passed to labs.
#' @param ylab
#' label for y-axis, default is DV as found in vpcresultfile, passed to labs.
#' @param fy
#' transformation function for y-axis, default to identity function (f(y)=y).
#' @param fx
#' transformation function for x-axis, default to identity function (f(x)=x)
#' @param col.data
#' color for data points, passed to geom_point, default="gray20"
#' @param cex.data
#' cex for data points, passed to geom_point, default=1
#' @param pch.data
#' plot symbol (pch) for data points, passed to geom_point, default=19
#' @param alpha.data
#' alpha for plotting of data points, passed to geom_point, default=0.5
#' @param col.line
#' line color for low quartile, median, and high quartile of data,
#' passed to geom_line, default="gray20"
#' @param lwd.line
#' line width (lwd) for low quartile, median, and high quartile of data, passed to geom_line, default=1
#' @param lty.line
#' line type (lty) for low quartile, median, and high quartile of data, passed to geom_line,
#' should be vector of length 3, default=c(2,1,2)
#' @param col.segm
#' Color of CI regions (low, median and high) for model, passed to geom_polygon,
#' should be vector of length 3, default=c("steelblue", "gray50", "steelblue")
#' @param alpha.segm
#' alpha of CI regions (low, median and high) for model, passed to geom_polygon, default=0.6
#' @param type
#' type of VPC plot:
#'
#' 1=display model regions only,
#'
#' 2=as 1 + lines (low, median and high) of data,
#'
#' 3= as 2 + points for data.
#'
#' type=0 can also be used and then no graph is produced but instead a list with 2 dataframes is returned.
#' one for the vpcresult and on for the observed data in the vpctab file.
#' @param panel.height.censored
#' Height for BLQ ALQ panels. Height is relative to height of Un-cenored panel, default=0.25
#' @param censoring.labels
#' Character vector used as labels for panels when VPC have either BLQ or ALQ data
#' Default set to c("ALQ","BLQ"). NOTE: must be character vector of length 2
#' @param ignore.censoring
#' Ignore any censoring results in VPC result file (BLQ and ALQ), defult=FALSE
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @return
#' ggplot object of VPC plot
#' @details
#' The ggplot2 package is used for creating the VPC and vpcfig2 have functionality for e.g. modify
#' names of strata, order of and subset of strata's, and transform the x- and or y-axis before plotting.
#'
#' The data used is either specified by the directory of the PsN generated files,
#' or by specifying the file names of both the vpctab-file and the vpcresult file.
#' See examples 1 and 2 below.
#'
#' Names of strata can be changed with the strata.names argument, if strata.names
#' is NULL the names as specified in the vpcresult file are used.
#'
#' strata.subset can be used to select a subset of strata, and or to change the order of stratas.
#' See example 3.
#'
#' \strong{Strata.subset} is matched with strata.names unless strata.names=NULL.
#' If strata.names=NULL then strata.subset is matched to names as specified in vpcresult file.
#' Default is to use all strata's (strata.subset=NULL).
#'
#' The fy and fx arguments can be used to alter the scale of data plotted.
#' For example, using \code{fy=function(y){log(y+1)}}
#' is a convenient way to get log-scale for y-axis but with an off-set to show any values==0.
#' Proper y tick marks can then be set using the \code{scale_y_continuous} ggplot2 function.
#'
#' Since the returned objest is a ggplot-class object is can be further modified to e.g. to
#' log-scale for y.axis;
#'
#' \code{vpcfig2(...)+scale_y_log10()},
#'
#' adding/changing labels & titles;
#'
#' \code{vpcfig2(...)+labs(y="Modified y-label", title="New title")}.
#'
#' \strong{Axis-limits} are preferably set using the ggplot2 function
#' \code{coord_cartesian(...)}. This way data points outside the axis-limit are only hidden when plotting.
# Setting axis-limits using e.g. \code{scale_x_continuous}, \code{scale_x_log10}, or
# \code{xlim} may produce
# an incorrect vpcplot.
# This because data outside the axis-limits is deleted before plotting.
#'
#' \strong{Stratified} VPC's are created with \code{facet_wrap(~strata)} as deafult but can be
#' modified to use \code{facet_grid} for setting the panel grid.
#'
#' However faceting must be done with \code{~strata}.
#'
#' See examples below and documentation for ggplot2 for further details.
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 element_blank
#' @examples
#'
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "vpctab004.dat", package = "nonmem2R")
#' file2 <- system.file("extdata", "vpc_results.csv", package = "nonmem2R")
#'
#'
#' # Ex 1, produce VPC with default setting, here specifying both vpctab and vpcresult
#' vpcfig2(vpctab=file1,vpcresult=file2)
#'
#' \dontrun{
#' # Ex 2, produce VPC with default setting, here specifying only directory of vpc files
#' path1<-gsub("vpctab004.dat","",file1)
#' vpcfig2(vpcdir=path1)
#'}
#'
#' # Ex 3, produce VPC with i) modifies strata names, ii) strata in reverse order, and
#' #        iii) labels
#' strata.names<-c("Group A","Group B")
#' xlab<-"Time after dose (hrs)"
#' ylab<-"Plasma Conc(mmol/L)"
#' vpcfig2(vpctab=file1,vpcresult=file2,strata.names=strata.names,strata.subset=2:1,
#'        xlab=xlab,ylab=ylab)
#'

#'
#' # Example using the fy argument to transform y-axis setting y-ticks using scale_y_continuous(...)
#' tmp<-c(1,2,3,4,6)
#' yticks<-c(0.1,tmp,tmp*10,tmp*100,tmp*1000)
#' vpcfig2(vpctab=file1,vpcresult=file2,fy=function(y){log(y+1)})+
#'  scale_y_continuous(breaks=log(yticks+1),labels=yticks,minor_breaks=NULL)
#'
#' \dontrun{
#' # Example changing to slog-scale using the ggplot2 function scale_y_log10
#' vpcfig2(vpctab=file1,vpcresult=file2)+scale_y_log10()
#'}
#' # Example changing y-axis label and adding figure title using ggplot2 function labs(...).
#' vpcfig2(vpctab=file1,vpcresult=file2)+labs(y="Modified y-label", title="New title")
#'
#'

#################
vpcfig2<-function(vpcdir=NULL,
                  vpctab=NULL,
                  vpcresult=NULL,
                  use.model.path=TRUE,
                  strata.names=NULL,
                  strata.subset=NULL,
                  median.only=FALSE,
                  bin.idv=c("median","midpoint"),
                  percentile=10,
                  fy=function(y){y},fx=function(x){x},
                  xlab=NULL,
                  ylab=NULL,
                  col.data="gray20",cex.data=1,pch.data=19,alpha.data=0.5,
                  col.line='grey20',lwd.line=1,lty.line=c(2,1,2),
                  col.segm=c("steelblue", "gray50", "steelblue"),
                  alpha.segm=0.6,
                  type=3,
				  panel.height.censored=0.25,
				  censoring.labels=c("ALQ","BLQ"),
				  ignore.censoring=FALSE,
				  control=GOF.control())
{
	value<-ymin<-data.type<-NULL

    bin.idv <- match.arg(bin.idv)
    file.path <- get.model.path()
    group <- group2 <- NULL
    if ((is.null(vpctab) | is.null(vpcresult)) & is.null(vpcdir)) {
        stop("Use either both vpctab and vpcresult or vpcdir")
    }
    if (is.null(vpctab)) {
        vpctab <- list.files(path = paste(file.path, vpcdir,
            sep = ""), pattern = "vpctab", full.names = TRUE)
        vpctab <- setdiff(vpctab, vpctab[grep("old", substr(vpctab,
            nchar(vpctab) - 4, nchar(vpctab)))])
        if (length(vpctab) == 0) {
            if (file.path != "") {
                warning(paste("No vpctab file found in folder",
                  paste(file.path, vpcdir, sep = ""), ". Trying without using the specified model.path"))
                file.path <- ""
                vpctab <- list.files(path = paste(file.path,
                  vpcdir, sep = ""), pattern = "vpctab", full.names = TRUE)
            }
            if (length(vpctab) == 0) {
                stop(paste("No vpctab file found in folder",
                  paste(file.path, vpcdir, sep = "")))
            }
        }
    }
    if (is.null(vpcresult)) {
        vpcresult <- paste(file.path, vpcdir, "/vpc_results.csv",
            sep = "")
        vpcresult <- setdiff(vpcresult, vpcresult[grep("old",
            substr(vpcresult, nchar(vpcresult) - 4, nchar(vpcresult)))])
    }
    if (!file.exists(vpctab) | !file.exists(vpcresult)) {
        stop(paste("files", vpctab, "or", vpcresult, "does not exist"))
    }
    cat("VPC based on files:\n  ", vpctab, "\nand\n  ", vpcresult,
        "\n")
    dd1 <- read.table(file = vpctab, sep = ",", header = T)
    dd1$id <- as.numeric(factor(dd1$ID))
    dd2 <- readVpc(vpc.results = vpcresult)
    ii <- match(c(dd2$dv.var, dd2$idv.var), colnames(dd1))
    colnames(dd1)[ii] <- c("DV", "TIME")
    if (is.null(ylab)) {
        ylab <- dd2$dv.var
    }
    if (is.null(xlab)) {
        xlab <- dd2$idv.var
    }
    n.strata <- length(dd2$strata.names)
    if (n.strata == 0) {
        dd2$strata.names <- "dummyStrata"
        dd2$result.tables <- list(dd2$result.tables)
		if(length(dd2$result.tables.cen)>0){
			dd2$result.tables.cen <- list(dd2$result.tables.cen)
		}
        dd1$strata_no <- 1
    }
    n.strata <- length(dd2$strata.names)
    if (length(strata.names) != n.strata) {
        strata.names <- dd2$strata.names
    }
    if (length(strata.names) == n.strata) {
        dd2$strata.names <- strata.names
        dd1$strata <- strata.names[dd1$strata_no]
    }
    if (length(strata.names) != n.strata) {
        dd1$strata <- dd2$strata.names[dd1$strata_no]
    }
    strata.names <- dd2$strata.names
    if (is.null(strata.subset)) {
        strata.subset <- 1:n.strata
    }
    if (all(strata.subset %in% paste(1:n.strata))) {
        strata.subset <- strata.names[strata.subset]
    }
    else {
        if (!all(strata.subset %in% strata.names)) {
            strata.subset <- strata.names
            warning("strata.subset ignored. Subset should be subset of strata.names or subset of 1:",
                n.strata)
        }
    }
    dd2$strata.names
    n.strata.subset <- length(strata.subset)
    dd1 <- dd1[dd1$strata %in% strata.subset, ]
    dd1$strata <- factor(dd1$strata, levels = strata.subset)
    model0 <- NULL
    data0 <- NULL
    modelCens0 <- NULL
    dataCens0 <- NULL

    strata <- 1
    strata.indexs <- match(strata.subset, strata.names)
	data.type.labels<-c(ylab,censoring.labels)
    for (strata in strata.indexs) {
        Z <- dd2$result.tables[[strata]]
        colnames(Z) <- paste("X", colnames(Z), sep = "")
        ii <- is.na(Z$Xlower)
        Z$Xlower[ii] <- Z$Xupper
        n1 <- gsub("[123456789][123456789].CI.for", "", colnames(Z))
        iL <- c(grep(paste("X", percentile, "[.]", sep = ""),
            n1), grep(paste("X[.]", percentile, "[.]", sep = ""),
            n1))
        iU <- c(grep(paste("X", 100 - percentile, "[.]", sep = ""),
            n1), grep(paste("X[.]", 100 - percentile, "[.]",
            sep = ""), n1))
        iM <- grep(paste(50), n1)
        if (length(iL) < 4 | length(iU) < 4 | length(iM) < 4) {
            stop(paste("Column with percentile", percentile,
                ", 50, ", "or", 100 - percentile, "was not found in data file"))
        }
        n1[iL] <- gsub(paste(percentile), "L", n1[iL])
        n1[iM] <- gsub("50", "M", n1[iM])
        n1[iU] <- gsub(paste(100 - percentile), "U", n1[iU])
        colnames(Z) <- n1
        range.idv <- range(dd1$TIME[dd1$strata_no == strata])
        min.idv <- min(c(range.idv, Z$Xupper))
        max.idv <- max(c(range.idv, Z$Xlower))
        Z <- Z[c(1, 1:nrow(Z), nrow(Z)), ]
        Z$Xupper[1] <- Z$Xlower[1] <- min.idv
        Z$Xlower[nrow(Z)] <- Z$Xupper[nrow(Z)] <- max.idv
        xx <- (Z$Xupper + Z$Xlower)/2
        if (bin.idv == "median") {
            xx <- Z$Xnobs
            xx[1] <- Z$Xlower[1]
            xx[nrow(Z)] <- Z$Xupper[nrow(Z)]
        }
        datai <- data.frame(x = xx, low = Z$XL.real, med = Z$XM.real,
            upp = Z$XU.real)
        datai$strata <- dd2$strata.names[strata]
        modeli <- data.frame(x = xx, lowL = Z$X.L.from, lowU = Z$X.L.to,
            medL = Z$X.M.from, medU = Z$X.M.to, uppL = Z$X.U.from,
            uppU = Z$X.U.to)
        modeli$strata <- dd2$strata.names[strata]

        data0 <- rbind(data0, datai)
        model0 <- rbind(model0, modeli)

		## Censoring data, new part
		if(length(dd2$result.tables.cen)>0){
			Z <- dd2$result.tables.cen[[strata]]
			colnames(Z) <- paste("X", colnames(Z), sep = "")
			ii <- is.na(Z$Xlower)
			Z$Xlower[ii] <- Z$Xupper
			colnames(Z) <- gsub("[123456789][123456789].CI.for", "", colnames(Z))

			Z <- Z[c(1, 1:nrow(Z), nrow(Z)), ]
			Z$Xupper[1] <- Z$Xlower[1] <- min.idv
			Z$Xlower[nrow(Z)] <- Z$Xupper[nrow(Z)] <- max.idv

			dataCensi <- rbind(data.frame(x = xx,  value = Z$XReal.left.censored,group="Median or percent",data.type=data.type.labels[3]),
							 data.frame(x = xx,  value = Z$XReal.right.censored,group="Median or percent",data.type=data.type.labels[2]))
			dataCensi$strata <- dd2$strata.names[strata]
			modelCensi <- rbind(data.frame(x = xx, ymin = Z$X.left.censored.from, ymax = Z$X.left.censored.to,group="Median or percent",data.type=data.type.labels[3]),
							  data.frame(x = xx, ymin = Z$X.right.censored.from, ymax = Z$X.right.censored.to,group="Median or percent",data.type=data.type.labels[2]))
			modelCensi$strata <- dd2$strata.names[strata]
			dataCens0  <- rbind(dataCens0 ,subset(dataCensi,!is.na(value)))
			modelCens0 <- rbind(modelCens0,subset(modelCensi,!is.na(ymin)))
		}
    }

	data0$data.type <-data.type.labels[1]
	model0$data.type<-data.type.labels[1]
	dd1$data.type <-data.type.labels[1]

	data0$strata <- factor(data0$strata, levels = strata.subset)
    model0$strata <- factor(model0$strata, levels = strata.subset)
    if (type == 0) {
        pp <- list(model0 = model0, data0 = data0, dd1 = dd1)
    }
    if (type > 0) {

		### New part  #######################################
		### Set flags for left and or right censoring type of data exists
		has.left<-has.right<-FALSE
		if(!ignore.censoring & length(dataCens0)>0){
			if(nrow(subset(dataCens0,data.type==data.type.labels[3]))>0){	has.left<-TRUE}
			if(nrow(subset(dataCens0,data.type==data.type.labels[2]))>0){	has.right<-TRUE}
		}
		median.label<-ifelse(has.left | has.right,"Median or percent","Median")
		### End new part
        group.labs <- c(paste(percentile, "th percentile", sep = ""),
            median.label, paste(100 - percentile, "th percentile",
                sep = ""))

        to_thin <- function(x, labs = c("2.5th percentile??",
            "Median ?!?", "97.5th percentile")) {
            low <- x[, !(colnames(x) %in% c("med", "upp", "medU",
                "uppU", "medL", "uppL"))]
            med <- x[, !(colnames(x) %in% c("low", "upp", "lowU",
                "uppU", "lowL", "uppL"))]
            upp <- x[, !(colnames(x) %in% c("low", "med", "lowU",
                "medU", "lowL", "medL"))]
            low$group <- labs[1]
            med$group <- labs[2]
            upp$group <- labs[3]
            colnames(low)[colnames(low) == "low"] <- "value"
            colnames(med)[colnames(med) == "med"] <- "value"
            colnames(upp)[colnames(upp) == "upp"] <- "value"
            colnames(low)[colnames(low) == "lowL"] <- "ymin"
            colnames(med)[colnames(med) == "medL"] <- "ymin"
            colnames(upp)[colnames(upp) == "uppL"] <- "ymin"
            colnames(low)[colnames(low) == "lowU"] <- "ymax"
            colnames(med)[colnames(med) == "medU"] <- "ymax"
            colnames(upp)[colnames(upp) == "uppU"] <- "ymax"
            y <- rbind(low, med, upp)
            y$group <- factor(y$group, levels = rev(labs))
            y
        }
        model0 <- to_thin(model0, labs = group.labs)
        model0$group2 <- model0$group
		modelCens0$group2 <- modelCens0$group ### Added
        data0 <- to_thin(data0, labs = group.labs)


		### New part  #######################################
		### Drop NA's in data0 ( summary of observed)
		data0<-subset(data0,!is.na(value))

		### Apply fy where needed
		dd1$DV<-fy(dd1$DV)
		data0$value<-fy(data0$value)
		model0$ymin<-fy(model0$ymin)
		model0$ymax<-fy(model0$ymax)

		### Get Range for each data type before and after transformations
		range.data<-range(c(dd1$DV,data0$value,model0$ymin,model0$ymax),na.rm=TRUE)
		range.censL0<-range.censL<-range.censR0<-range.censR<-NULL
		if(has.left){
			ii.data<-dataCens0$data.type==data.type.labels[3]
			ii.model<-modelCens0$data.type==data.type.labels[3]
			range.censL0<-range(c(
			dataCens0$value[ii.data],
			modelCens0$ymin[ii.model],
			modelCens0$ymax[ii.model]),na.rm=TRUE)
			range.censL<-range.data[2]+c(1,2)*diff(range.data)*panel.height.censored
			f.range.censL<-function(x){
				range.censL[1]+(x-range.censL0[1])*diff(range.censL)/diff(range.censL0)
			}
			dataCens0$value[ii.data]<-f.range.censL(dataCens0$value[ii.data])
			modelCens0$ymin[ii.model]<-f.range.censL(modelCens0$ymin[ii.model])
			modelCens0$ymax[ii.model]<-f.range.censL(modelCens0$ymax[ii.model])
		}
		if(has.right){
			ii.data<-dataCens0$data.type==data.type.labels[2]
			ii.model<-modelCens0$data.type==data.type.labels[2]
			range.censR0<-range(c(
			dataCens0$value[ii.data],
			modelCens0$ymin[ii.model],
			modelCens0$ymax[ii.model]),na.rm=TRUE)
			range.censR<-range.data[2]+c(3,4)*diff(range.data)*panel.height.censored
			f.range.censR<-function(x){
				range.censR[1]+(x-range.censR0[1])*diff(range.censR)/diff(range.censR0)
			}
			dataCens0$value[ii.data]<-f.range.censR(dataCens0$value[ii.data])
			modelCens0$ymin[ii.model]<-f.range.censR(modelCens0$ymin[ii.model])
			modelCens0$ymax[ii.model]<-f.range.censR(modelCens0$ymax[ii.model])
		}
		### Combine the different data types and set order for data.types
		if(has.right | has.left){
			data0<-rbind(data0,dataCens0)
			model0<-rbind(model0,modelCens0)
			ylab=NULL
		}
		data0$data.type<-factor(data0$data.type,levels=data.type.labels)
		model0$data.type<-factor(model0$data.type,levels=data.type.labels)
		dd1$data.type<-factor(dd1$data.type,levels=data.type.labels)
		### end new part

        if (median.only) {
            data0 <- subset(data0, group    %in% c("Median","Median or percent"))
            model0 <- subset(model0, group2 %in% c("Median","Median or percent"))
            lty.line <- lty.line[2]
            col.segm <- col.segm[2]
        }
		facet.message<-""

		yticks<-pretty(range.data)
		yticks<-yticks[yticks>range.data[1] & yticks<range.data[2]]
		ylabels<-yticks
		if(has.left){
			tmp<-pretty(range.censL0*100,n=3)
			yticks<-c(yticks,f.range.censL(tmp/100))
			ylabels<-c(ylabels,tmp)
		}
		if(has.right){
			tmp<-pretty(range.censR0*100,n=3)
			yticks<-c(yticks,f.range.censR(tmp/100))
			ylabels<-c(ylabels,tmp)
		}
        pp <- ggplot(data = NULL)
        pp <- pp + geom_ribbon(aes_string(x = "fx(x)", ymin = "(ymin)",
            ymax = "(ymax)", group = "group2", fill = "group2"),
            data = model0, alpha = alpha.segm) + labs(fill = "Model")
        if (type > 1) {
            pp <- pp + geom_line(aes_string(x = "fx(x)", y = "(value)",
                group = "group", linetype = "group"), data = data0,
                lwd = lwd.line, color = col.line) + labs(linetype = "Data")
        }
        if (type > 2) {
            pp <- pp + geom_point(aes_string(x = "fx(TIME)",
                y = "(DV)"), data = dd1, color = col.data,
                alpha = alpha.data, cex = cex.data)
        }
        pp <- pp + labs(y = ylab, x = xlab) + scale_fill_manual(values = col.segm) +
            scale_linetype_manual(values = lty.line)+guides(fill = guide_legend(order=1))

        if (n.strata > 1 & length(strata.subset) > 1 & (has.right | has.left)) {
            pp <- pp + facet_grid(data.type~strata,scales="free_y",space="free_y",switch="y")+
						theme(strip.text.y = element_text(size=12),strip.background.y = element_blank(),strip.placement = "outside")
			facet.message<-"facet_grid(data.type~strata,scales=\"free_y\",space=\"free_y\",switch=\"y\")"
        }
        if (n.strata > 1 & length(strata.subset) > 1 & !has.right & !has.left) {
            pp <- pp + facet_wrap(~strata)
			facet.message<-"facet_wrap(~strata)"
        }
        if ((n.strata == 1 | length(strata.subset) == 1) & (has.right | has.left)) {
            pp <- pp + facet_grid(data.type~.,scales="free_y",space="free_y",switch="y")+
						theme(strip.text.y = element_text(size=12),strip.background.y = element_blank(),strip.placement = "outside")
			facet.message<-"facet_grid(data.type~.,scales=\"free_y\",space=\"free_y\",switch=\"y\")"
        }
        if (control$add.caption) {
            pp <- add.caption(pp, control)
        }
		if(facet.message!=""){   ### Show message on faceting
			cat("Facetting was set using:\n",facet.message,"\n")
		}
		if(has.left | has.right){ ### Set Yaxis and show message on y-scale if any censoring
			pp<-pp + scale_y_continuous(breaks=yticks,labels=ylabels)
			cat(paste("Y-scale was set using:\nscale_y_continuous(\n  breaks=c(",
				paste(yticks,collapse=","),"),\n  labels=c(",
				paste(ylabels,collapse=","),"))\n",sep=""))
		}
	}
    pp
}
