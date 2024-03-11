#------------------- * *  add.caption  * * ---------------------------------
#' Add caption to ggplot object
#' @description
#' Adds caption text as returned by get.caption to a ggplot object
#' @param p
#' ggplot object
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @export
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
add.caption<-function(p,control=GOF.control()){
  p+labs(caption=control$get.caption())+
    theme(plot.caption=element_text(size=control$size.caption,color=control$col.caption))
}



#------------------- * *  do.one.GOF  * * ---------------------------------
#' X-Y GOF-plot
#' @description
#' X-Y GOF plot with reflines and data smoother and with caption added as reurned by get.caption
#' @param data
#' data.frame to plot
#' @param x
#' character string with name of column for x
#' @param y
#' character string with name of column for y
#' @param color
#' data columns to set different colors in plot, interpreted as factor
#' @param add.loess
#' add loess smoother to plot (TRUE), or not (FALSE)
#' @param refline
#' add reference line with intercept=0, slope=1 (abline), horizontal at y=0( href0), horizontal at y=mean of y( hrefmean), horizontal at y=median of y( hrefmedian), or don't add reference line (none)
#' @param title
#' title
#' @param lines.by.id
#' connect subjects by lines (TRUE), or don't (FALSE)
#' @param id.column
#' column name that indicate subject identifier
#' @param fx
#' function for transformation of x before plotting
#' @param fy
#' function for transformation of y before plotting
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @examples
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
#' sdtab<-read.table(file=file1,skip=1,header=TRUE)
#' set.script.name("MyScript.R")
#' do.one.GOF(subset(sdtab,DV>0),"IPRED","DV")
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_smooth
do.one.GOF<-function(data,x,y,color="",add.loess=TRUE,refline=c("abline","href0","hrefmean","hrefmedian","none"),
title="",lines.by.id=FALSE,id.column="ID",fx=NULL,fy=NULL,control=GOF.control()){

	refline<-match.arg(refline)

	if(!is.null(fx)){
		data[,x]<-fx(data[,x])
	}
	if(!is.null(fy)){
		data[,y]<-fy(data[,y])
	}
	p1<-ggplot(data,aes_string(x=x,y=y))+labs(y=get.label(y,fy),x=get.label(x,fx))
	if(color==""){
		p1<-p1+geom_point(col=control$col.data,cex=control$cex.data,pch=control$pch.data,alpha=control$alpha.data)
	}
	else{
		p1<-p1+geom_point(aes_string(x=x,y=y,color=color),cex=control$cex.data,pch=control$pch.data,alpha=control$alpha.data)
	}

	if(lines.by.id){
		if(color==""){
			p1<-p1+	geom_line(aes_string(group=id.column),col=control$col.data,alpha=control$alpha.data)
		}
		else{
			p1<-p1+	geom_line(aes_string(group=id.column,color=color),alpha=control$alpha.data)
		}
	}
	if(refline=="abline"){
		p1<-p1+geom_abline(col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(refline=="href0"){
		p1<-p1+geom_hline(yintercept=0,col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(refline=="hrefmean"){
		p1<-p1+stat_hmean(col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(refline=="hrefmedian"){
		p1<-p1+stat_hmedian(col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	#,col=control$col.smooth,lty=control$lty.smooth,lwd=control$lwd.smooth,se=control$se.smooth
	if(add.loess){
		p1<-p1+geom_smooth(method="loess",span=control$span.smooth,method.args = list(degree=control$degree.smooth,family=control$family.smooth),
							col=control$col.smooth,lty=control$lty.smooth,lwd=control$lwd.smooth,se=control$se.smooth)
	}
	if(control$add.caption){
		p1<-add.caption(p1,control)
	}
	if(!(title=="" | is.null(title))){
		p1<-p1+labs(title=title)
	}
	p1

}

#------------------- * *  do.cat.GOF  * * ---------------------------------
#' Y vs categorical X GOF-plot
#' @description
#' GOF plot with boxplots of Y grouped by categorical data in X, with refline and with caption added as reurned by get.caption
#' @param data
#' data.frame to plot
#' @param x
#' character string with name of column for x
#' @param y
#' character string with name of column for y
#' @param color
#' data columns to set different colors in plot, interpreted as factor
#' @param add.points
#' add jittered points of data (TRUE), or not (FALSE)
#' @param refline
#' add reference line with intercept=0, slope=1 (abline), horizontal at y=0( href0), horizontal at y=mean of y( hrefmean), horizontal at y=median of y( hrefmedian), or don't add reference line (none)
#' @param title
#' title
#' @param lines.by.id
#' connect subjects by lines (TRUE), or don't (FALSE)
#' @param id.column
#' column name that indicate subject identifier
#' @param fx
#' function for transformation of x before plotting
#' @param fy
#' function for transformation of y before plotting
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 geom_hline
do.cat.GOF<-function(data,x,y,color="",add.points=TRUE,refline=c("abline","href0","hrefmean","hrefmedian","none"),
title="",lines.by.id=FALSE,id.column="ID",fx=NULL,fy=NULL,control=GOF.control()){

	refline<-match.arg(refline)

	if(!is.null(fy)){
		data[,y]<-fy(data[,y])
	}
	p1<-ggplot(data,aes_string(x=x,y=y))+labs(y=get.label(y,fy),x=get.label(x,fx))
	if(color==""){
	  if(add.points){
	    p1<-p1+geom_jitter(col=control$col.data,cex=control$cex.data,pch=control$pch.data,alpha=control$alpha.data)
	  }
		p1<-p1+geom_boxplot(fill=control$fill.box,alpha=control$alpha.box,col=control$col.box)
  }
	else{
	  if(add.points){
	    p1<-p1+geom_jitter(aes_string(x=x,y=y,color=color),cex=control$cex.data,pch=control$pch.data,alpha=control$alpha.data)
	  }
		p1<-p1+geom_boxplot(aes_string(x=x,y=y,color=color,fill=color),alpha=control$alpha.box)
	}

	if(refline=="href0"){
		p1<-p1+geom_hline(yintercept=0,col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(refline=="hrefmean"){
		p1<-p1+stat_hmean(col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(refline=="hrefmedian"){
		p1<-p1+stat_hmedian(col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(control$add.caption){
		p1<-add.caption(p1,control)
	}
	if(!(title=="" | is.null(title))){
		p1<-p1+labs(title=title)
	}
	p1

}

#------------------- * *  do.multi.GOF  * * ---------------------------------
#' Multiple Y vs single X GOF-plot
#' @description
#' X-Y GOF plots for multiple Y's and shared X with reflines and data smoother and with caption
#' added as reurned by get.caption
#' @param data
#' data.frame to plot
#' @param x
#' character string with name of column for x
#' @param y
#' character string with name of column for y
#' @param color
#' data columns to set different colors in plot, interpreted as factor
#' @param add.loess
#' add loess smoother to plot (TRUE), or not (FALSE)
#' @param refline
#' add reference line with intercept=0, slope=1 (abline), horizontal at y=0( href0), horizontal at y=mean of y( hrefmean), horizontal at y=median of y( hrefmedian), or don't add reference line (none)
#' @param title
#' title
#' @param lines.by.id
#' connect subjects by lines (TRUE), or don't (FALSE)
#' @param id.column
#' column name that indicate subject identifier
#' @param fx
#' function for transformation of x before plotting
#' @param fy
#' function for transformation of y before plotting
#' @param ylab
#' y axis label
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_smooth
#' @importFrom reshape2 melt
do.multi.GOF<-function(data,x,y,color="",add.loess=TRUE,refline=c("none","abline","href0","hrefmean","hrefmedian"),title="",
	lines.by.id=FALSE,id.column="ID",fx=NULL,fy=NULL,ylab="Observations/Predictions",control=GOF.control()){

	refline<-match.arg(refline)
	t1<-melt(data,measure.vars=y)
	Ps<-get.label(y,fy)
	t1$variable<-factor(t1$variable,levels=y)
	t1$variable<-factor(Ps[as.numeric(t1$variable)],levels=Ps)

	p1<-ggplot(t1,aes_string(x=x,y="value"))+
	labs(y=ylab,x=get.label(x,fx))+
	facet_wrap(~variable)

	if(color==""){
		p1<-p1+geom_point(col=control$col.data,cex=control$cex.data,pch=control$pch.data,alpha=control$alpha.data)
	}
	else{
		p1<-p1+geom_point(aes_string(x=x,y=y,color=color),cex=control$cex.data,pch=control$pch.data,alpha=control$alpha.data)
	}

	if(lines.by.id){
		if(color==""){
			p1<-p1+	geom_line(aes_string(group=id.column),col=control$col.data,alpha=control$alpha.data)
		}
		else{
			p1<-p1+	geom_line(aes_string(group=id.column,color=color),alpha=control$alpha.data)
		}
	}

	if(refline=="abline"){
		p1<-p1+geom_abline(col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(refline=="href0"){
		p1<-p1+geom_hline(yintercept=0,col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(refline=="hrefmean"){
				p1<-p1+stat_hmean(col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(refline=="hrefmedian"){
				p1<-p1+stat_hmedian(col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
	}
	if(add.loess){
		p1<-p1+geom_smooth(method="loess",span=control$span.smooth,method.args = list(degree=control$degree.smooth,family=control$family.smooth),
							col=control$col.smooth,lty=control$lty.smooth,lwd=control$lwd.smooth,se=control$se.smooth)
	}
	if(control$add.caption){
		p1<-add.caption(p1,control)
	}
	if(!(title=="" | is.null(title))){
		p1<-p1+labs(title=title)
	}
	p1

}

#------------------- * *  do.indivudual.GOF  * * ---------------------------------
#' Individual GOF-plots
#' @description
#' DV, IPRED and PRED (default) individual GOF plots with one panel per subject.
#' Caption added as reurned by get.caption
#' @param data
#' data.frame to plot
#' @param x
#' character string with name of column for x
#' @param y
#' character string vector with names of columns for y, default = c(DV, IPRED , PRED)
#' @param type
#' plot symbol or line for each y variable, length of type must match length of y
#' @param title
#' title
#' @param per.page
#' number of subjects/panels on each page
#' @param fx
#' function for transformation of x before plotting
#' @param fy
#' function for transformation of y before plotting
#' @param ylab
#' y axis label
#' @param equal.lims
#' use same x- and y-limits on all pages and panels (TRUE), or use allow to vary across pages (FALSE)
#' @param global.ggplot.options
#' ggplot option added to  each ggplot before plotting/returning object
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @examples
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
#' sdtab<-read.table(file=file1,skip=1,header=TRUE)
#' set.script.name("MyScript.R")
#' do.individual.GOF(subset(sdtab,DV>0 & ID<13))
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 lims
#' @importFrom reshape2 melt
do.individual.GOF<-function(data,x="TIME",y=c("DV","IPRED","PRED"),type=c("point","line","line"),title="",
	per.page=20,fx=NULL,fy=NULL,ylab="Observations/Predictions",equal.lims=TRUE,global.ggplot.options=NULL,control=GOF.control()){
	t1<-melt(data,measure.vars=y)

	ID<-value<-P2<-P<-NULL

	Ps<-get.label(y,fy)
	t1$variable<-factor(t1$variable,levels=y)
	t1$P<-factor(Ps[as.numeric(t1$variable)],levels=Ps)
	t1$P2<-t1$P
	t1$type<-type[as.numeric(t1$variable)]
	t1$x<-t1[,x]

	ids<-sort(unique(data$ID))
	n.ids<-length(ids)
	n.page<-ceiling(n.ids/per.page)
	ylim<-range(t1$value)
	xlim<-range(t1[,x])

	for(i in 1:n.page){
		ii<-ids[(1:per.page)+per.page*(i-1)]
		ii<-ii[!is.na(ii)]
		pp <- ggplot(data = NULL)
		tmp<-subset(t1,type=="point" & ID %in% ii)
		pp <- pp + geom_point(aes(x = x, y = value,group=P2,shape=P2),data=tmp,
					col=control$col.data,cex=control$cex.data,alpha=control$alpha.data)
		tmp<-subset(t1,type=="line" & ID %in% ii)
		pp <- pp + geom_line(aes(x = x, y = value,group=P,color=P),data=tmp)  ## Add input from control
		pp<-pp+	facet_wrap(~ID) + labs(color="",linetype="",shape="",y=ylab,x=get.label(x,fx))
		pp<-pp+theme(legend.position="top")
		if(equal.lims){
			pp<-pp+lims(x=xlim,y=ylim)
		}
		if(control$add.caption){
			pp<-add.caption(pp,control)
		}
		if(!(title=="" | is.null(title))){
			pp<-pp+labs(title=title)
		}
		print(pp+global.ggplot.options)
	}
	invisible(pp+global.ggplot.options)
}


#------------------- * *  histGOF  * * ---------------------------------
#' Histogram GOF
#' @description
#' Histogram GOF showing histogram, smooth density, and normal density as reference line
#' and caption added as reurned by get.caption
#' @param data
#' data.frame to plot
#' @param x
#' character string with name of column for x
#' @param title
#' title
#' @param color
#' data columns to set different colors in plot, interpreted as factor
#' @param fx
#' function for transformation of x before plotting
#' @param bins
#' number of histogram bins, if NA, bins are set using Sturges' formula
#' @param refline
#' add reference line (default =TRUE) or not (FALSE)
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_histogram
#' @examples
#' dd<-data.frame(CWRES=rnorm(100),gr=rep(LETTERS[1:5],20))
#' histGOF(dd,"CWRES",color="gr")
#'
histGOF<-function(data,x,title="",color="",fx=NULL,bins=NA,refline=TRUE,control=GOF.control()){
	if(!is.null(fx)){
		data[,x]<-fx(data[,x])
	}
	xlab<-get.label(x,fx)
	if(is.null(fx)){
		fx<-identity
	}
	if(is.na(bins)){
		bins=ceiling(1+log2(length(data[,x])))
	}
	ncolor=1
	pp<-ggplot(data,aes_string(x=x, y="stat(density)"))+
		labs(color="",x=xlab,y="Density")
	if(color==""){
		pp<-pp+geom_histogram(fill=control$fill.hist,col=control$col.hist,alpha=control$alpha.hist,bins=bins)
	}else{
		ncolor=length(unique(data[,color]))
		pp<-pp+geom_histogram(aes_string(fill=color),col=control$col.hist,alpha=control$alpha.hist,bins=bins)
	}
	pp<-pp+	geom_line(stat = 'density',col=control$col.smooth,lty=control$lty.smooth,lwd=control$lwd.smooth,position = position_scale(y = ncolor))
	if(refline){
		pp<-pp + stat_dnorm(aes_string(x=x),inherit.aes=F,col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline,position = position_scale(y = ncolor))
	}
	if(control$add.caption){
		pp<-add.caption(pp,control)
	}
	if(!(title=="" | is.null(title))){
		pp<-pp+labs(title=title)
	}
	pp
}

#------------------- * *  qqnormGOF  * * ---------------------------------
#' QQ-norm GOF
#' @description
#' QQ-norm GOF qqnorm plots with reference line
#' and caption added as reurned by get.caption
#' @param data
#' data.frame to plot
#' @param sample
#' character string with name of column for x
#' @param title
#' title
#' @param color
#' data columns to set different colors in plot, interpreted as factor
#' @param fx
#' function for transformation of x before plotting
#' @param refline
#' add reference line (default =TRUE) or not (FALSE)
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @export
#' @importFrom ggplot2 ggplot stat_qq_line stat_qq
#' @examples
#' dd<-data.frame(CWRES=rnorm(100))
#' qqnormGOF(dd,"CWRES")
qqnormGOF<-function(data,sample,title="",color="",fx=NULL,refline=TRUE,control=GOF.control()){
  if(!is.null(fx)){
    data[,sample]<-fx(data[,sample])
  }
  xlab<-get.label(sample,fx)
  if(is.null(fx)){
    fx<-identity
  }

  #### Re code using geom for qqnorm (in nonmem2R) AND add refline for the nonmem2R function
  if(color==""){
    pp<-ggplot(data,aes_string(sample=sample)) +
      stat_qq()+
      labs(color="",y=xlab,x="Theoretical Quantiles")
  }
  else{
    pp<-ggplot(data,aes_string(sample=sample,color=color)) +
      stat_qq()+
      labs(color="",y=xlab,x="Theoretical Quantiles")
  }

  if(refline){
    if(color==""){
      pp<-pp+	stat_qq_line(col=control$col.refline,lty=control$lty.refline,lwd=control$lwd.refline)
    }
    else{
      pp<-pp+	stat_qq_line(lty=control$lty.refline,lwd=control$lwd.refline)
    }
  }
  if(control$add.caption){
    pp<-add.caption(pp,control)
  }
  if(!(title=="" | is.null(title))){
    pp<-pp+labs(title=title)
  }
  pp
}


#------------------- * *  merge2GOF  * * ---------------------------------
#' Merging 2, 4, or 6 GOF's into one graph
#' @description
#' Merging multiple GOFs (ggplot objects) into one graph with
#' caption added as reurned by get.caption. Any legend present in first GOF is added to the combined graph.
#' @param p1
#' first ggplot object
#' @param p2
#' second ggplot object
#' @param p3
#' third ggplot object
#' @param p4
#' 4th ggplot object
#' @param p5
#' 5th ggplot object
#' @param p6
#' 6th ggplot object
#' @param byrow
#' combine plots side by side (TRUE) or one above the other (FALSE)
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplotGrob
#' @importFrom ggplot2 theme
#' @importFrom gridExtra grid.arrange
#' @importFrom gridExtra gtable_cbind
#' @importFrom gridExtra gtable_rbind
merge2GOF<-function(p1,p2,byrow=TRUE){
	g <- ggplotGrob(p1 + theme(legend.position="right"))$grobs
	ii<-which(sapply(g, function(x) x$name) == "guide-box")
	legend<-NULL
	if(length(ii)>0){
		legend<-g[[ii]]
	}
	if(is.null(legend)){
		if(byrow){
				grid.arrange(
					gtable_cbind(ggplotGrob(p1), ggplotGrob(p2)))
		}else{
				grid.arrange(
					gtable_rbind(ggplotGrob(p1), ggplotGrob(p2)))
		}
	}else{
		if(byrow){
				grid.arrange(
					gtable_cbind(ggplotGrob(p1+theme(legend.position="none")), ggplotGrob(p2+theme(legend.position="none"))),right=legend)
		}else{
				grid.arrange(
					gtable_rbind(ggplotGrob(p1+theme(legend.position="none")), ggplotGrob(p2+theme(legend.position="none"))),right=legend)
		}
	}
}

#####################################################################################
#' @rdname merge2GOF
#' @export
merge4GOF<-function(p1,p2,p3,p4){
	g <- ggplotGrob(p1 + theme(legend.position="right"))$grobs
	ii<-which(sapply(g, function(x) x$name) == "guide-box")
	legend<-NULL
	if(length(ii)>0){
		legend<-g[[ii]]
	}
	if(is.null(legend)){
		grid.arrange(
			gtable_rbind(	gtable_cbind(ggplotGrob(p1), ggplotGrob(p2)),
							gtable_cbind(ggplotGrob(p3), ggplotGrob(p4))))
	}else{
		grid.arrange(
			gtable_rbind(	gtable_cbind(ggplotGrob(p1+theme(legend.position="none")), ggplotGrob(p2+theme(legend.position="none"))),
							gtable_cbind(ggplotGrob(p3+theme(legend.position="none")), ggplotGrob(p4+theme(legend.position="none")))),right=legend)
	}
}



#####################################################################################
#' @rdname merge2GOF
#' @export
merge6GOF<-function(p1,p2,p3,p4,p5,p6){
	g <- ggplotGrob(p1 + theme(legend.position="right"))$grobs
	ii<-which(sapply(g, function(x) x$name) == "guide-box")
	legend<-NULL
	if(length(ii)>0){
		legend<-g[[ii]]
	}
	if(is.null(legend)){
		grid.arrange(
			gtable_rbind(	gtable_cbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3)),
							gtable_cbind(ggplotGrob(p4), ggplotGrob(p5), ggplotGrob(p6))))
	}else{
		grid.arrange(
			gtable_rbind(	gtable_cbind(ggplotGrob(p1+theme(legend.position="none")), ggplotGrob(p2+theme(legend.position="none")), ggplotGrob(p3+theme(legend.position="none"))),
							gtable_cbind(ggplotGrob(p4+theme(legend.position="none")), ggplotGrob(p5+theme(legend.position="none")), ggplotGrob(p6+theme(legend.position="none")))),right=legend)
	}
}
#------------------- * *  basic.GOF4  * * ---------------------------------
#' Basic 4- and 6-panel GOF
#' @description
#' basic.GOF4 provides a 4-panel GOF plot showing \cr
#' i) Observations (DV) vs population predictions (PRED) \cr
#' ii) Observations (DV) vs individual predictions (IPRED) \cr
#' iii) sqrt(abs(CWRES)) vs individual predictions (IPRED), and \cr
#' iV) CWRES vs TIME or other columns as set by idv \cr
#'
#' basic.GOF6 provides 2 additional GOF's showing histogram and qqnorm GOF's for CWRES.
#'
#' Caption is added as reurned by get.caption
#'
#' @param data
#' data.frame to plot
#' @param residual
#' column name for residuals, default="CWRES"
#' @param idv1
#' independent variable for plot of sqrt of absolute CWRES
#' @param idv2
#' independent variable for plot of CWRES
#' @param title
#' title
#' @param color
#' data columns to set different colors in plot, interpreted as factor
#' @param log.scale
#' use log scale for DV, IPRED and PRED (TRUE) or normal scale (FALSE)
#' @param global.ggplot.options
#' ggplot option added to  each ggplot before plotting/returning object
#' @param refline
#' add reference line (default =TRUE) or not (FALSE)
#' @param add.loess
#' add loess smoother to plot (TRUE), or not (FALSE)
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_x_log10
#' @importFrom ggplot2 scale_y_log10
#' @examples
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
#' sdtab<-read.table(file=file1,skip=1,header=TRUE)
#' set.script.name("MyScript.R")
#' ## Example 4 panel basic GOF
#' basic.GOF4(subset(sdtab,DV>0),idv2="TAPD")
#' ## Example 6 panel basic GOF
#' basic.GOF6(subset(sdtab,DV>0),idv1="TAPD",idv2="PRED")
basic.GOF4<-function(data,residual="CWRES",idv1="IPRED",idv2="TIME",title="",color="",log.scale=FALSE,
                     global.ggplot.options=NULL,refline=TRUE,add.loess=TRUE,control=GOF.control()){
	sqrt.abs<-function(x){sqrt(abs(x))}
	control2<-control
	control2$add.caption<-FALSE

	p1<-do.one.GOF(data,x="PRED",y="DV",color=color,refline=ifelse(refline,"abline","none"),control=control2,add.loess=add.loess)+global.ggplot.options
	p2<-do.one.GOF(data,x="IPRED",y="DV",color=color,refline=ifelse(refline,"abline","none"),control=control2,add.loess=add.loess)+global.ggplot.options
	cwres.label<-get.label(residual)
	eval(parse(text=  paste("cwres.label<-expression(sqrt('|",cwres.label,"|'","))",sep="")  ))
	p3<-do.one.GOF(data,x=idv1,y=residual,color=color,fy=sqrt.abs,refline=ifelse(refline,"hrefmedian","none"),control=control2,add.loess=add.loess)+labs(y=cwres.label)+global.ggplot.options
	p4<-do.one.GOF(data,x=idv2,y=residual,color=color,ifelse(refline,"href0","none"),control=control,add.loess=add.loess)+global.ggplot.options
	if(log.scale){
	  p1<-p1+scale_x_log10()+scale_y_log10()
	  p2<-p2+scale_x_log10()+scale_y_log10()
	  p3<-p3+scale_x_log10()
	}
	if(!(title=="" | is.null(title))){
		p1<-p1+labs(title=title)
	}
	merge4GOF(p1,p2,p3,p4)
}


#------------------- * *  basic.GOF6  * * ---------------------------------
#' @rdname basic.GOF4
#' @param bins
#' number of histogram bins, if NA, bins are set using Sturges' formula
#' @export
basic.GOF6<-function(data,residual="CWRES",idv1="IPRED",idv2="TIME",title="",color="",log.scale=FALSE,global.ggplot.options=NULL,refline=TRUE,add.loess=TRUE,bins=NA,control=GOF.control()){
	sqrt.abs<-function(x){sqrt(abs(x))}
	control2<-control
	control2$add.caption<-FALSE

	p1<-do.one.GOF(data,x="PRED",y="DV",color=color,refline=ifelse(refline,"abline","none"),control=control2,add.loess=add.loess)+global.ggplot.options
	p2<-do.one.GOF(data,x="IPRED",y="DV",color=color,refline=ifelse(refline,"abline","none"),control=control2,add.loess=add.loess)+global.ggplot.options
	cwres.label<-get.label(residual)
	eval(parse(text=  paste("cwres.label<-expression(sqrt('|",cwres.label,"|'","))",sep="")  ))
	p3<-do.one.GOF(data,x=idv1,y=residual,color=color,fy=sqrt.abs,refline=ifelse(refline,"hrefmedian","none"),control=control2,add.loess=add.loess)+labs(y=cwres.label)+global.ggplot.options
	p4<-do.one.GOF(data,x=idv2,y=residual,color=color,ifelse(refline,"href0","none"),control=control2,add.loess=add.loess)+global.ggplot.options
	p5<-histGOF(data,residual,color=color,bins=bins,control=control2,refline=refline)+global.ggplot.options
	p6<-qqnormGOF(data,residual,color=color,control=control,refline=refline)+global.ggplot.options
	if(log.scale){
	  p1<-p1+scale_x_log10()+scale_y_log10()
	  p2<-p2+scale_x_log10()+scale_y_log10()
	  p3<-p3+scale_x_log10()
	}
	if(!(title=="" | is.null(title))){
		p1<-p1+labs(title=title)
	}
	merge6GOF(p1,p2,p5,p3,p4,p6)
}

#------------------- * *  basic.eta.GOF  * * ---------------------------------
#' Histogram and/or QQ-norm GOF for ETA's
#' @description
#' Provides histogram and/or QQ-norm GOF for all ETA's included in the input data.frame with
#' caption added as reurned by get.caption
#' @param data
#' data.frame to plot
#' @param title
#' title
#' @param global.ggplot.options
#' ggplot option added to  each ggplot before plotting/returning object
#' @param type
#' do only qq-norm plots (qqnorm), only histograms (hist), or do both (both)
#' @param ETA.subset
#' index for subset of ETA's to plot. If NULL (default) all ETA's are plotted.
#' @param refline
#' add reference line (default =TRUE) or not (FALSE)
#' @param drop.fixed
#' drop ETA's that are fixed, i.e. any ETA with the same value in all subjects
#' @param id.column
#' column name that indicate subject identifier
#' @param standardize
#' Standardize all ETA's  (e.g. scale to unit variance)
#' @param bins
#' number of histogram bins, if NA, bins are set using Sturges' formula
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @examples
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
#' sdtab<-read.table(file=file1,skip=1,header=TRUE)
#' set.script.name("MyScript.R")
#' basic.eta.GOF(sdtab)
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom reshape2 melt
basic.eta.GOF<-function(data,title="",global.ggplot.options=NULL,type=c("both","qqnorm","hist"),
                        ETA.subset=NULL,refline=TRUE,drop.fixed=TRUE,id.column="ID",standardize=TRUE,bins=NA,control=GOF.control()){
	data<-data[!duplicated(data[,id.column]),]
	if(is.na(bins)){
		bins=ceiling(1+log2(nrow(data)))
	}
	ii<-which(colnames(data) %in% c(paste("ETA",1:9,sep=""),paste("ET",10:99,sep="")))
	if(!(is.null(ETA.subset))){
	  ii<-which(colnames(data) %in% c(paste("ETA",ETA.subset,sep=""),paste("ET",ETA.subset,sep="")))
	}
	if(length(ii)==0){
		stop("No ETA's found. Input data must have at least one column named ETA1,ETA2,..")
	}
	### Get eta.labels and make sure is of same length as number of ETA's found =length(ii)
	eta.labels<-control$eta.labels
	kk<-(1:length(ii))
	if(length(eta.labels)>0){
	  if(is.null(ETA.subset)){
		  eta.labels<-c(eta.labels,rep("",999))[1:length(ii)]
	  }
	  else{
	    eta.labels<-c(eta.labels,rep("",999))[sort(ETA.subset)][1:length(ii)]
	  }
	}

	if(drop.fixed){
		if(length(ii)==1){
			jj<-which(sd(data[,ii])>0)
		}
		if(length(ii)>1){
			jj<-which(apply(data[,ii],2,sd)>0)
		}
		ii<-ii[jj]
		kk<-kk[jj]
	}
	if(length(ii)==0){
		stop("All ETA's have zero variance. Try adding option drop.fixed=FALSE")
	}
	ylab="ETA value"
	if(standardize){
		ylab="Standardized ETA value"
		for(i in ii){
			sdi<-sd(data[,i])
			if(sdi>0){
				data[,i]<-data[,i]/sdi
			}
		}
	}
	tmp<-melt(data,id.vars=colnames(data)[-ii])
	p1<-NULL
	p2<-NULL
	res<-NULL
	type<-match.arg(type)

	if(length(eta.labels)>0){
		eta.levs<-paste(levels(tmp$variable),"(",eta.labels[kk],")",sep="")
		tmp$variable<-factor(eta.levs[as.numeric(tmp$variable)],levels=eta.levs)
	}

	if(type=="hist"){
		res<-histGOF(tmp,"value",refline=refline,bins=bins,control=control)+labs(x=ylab,title=title)+facet_wrap(~variable,scales="free")+global.ggplot.options
		return(res)
	}
	if(type== "qqnorm"){
		res<-qqnormGOF(tmp,"value",refline=refline,control=control)+labs(y=ylab,title=title)+facet_wrap(~variable,scales="free")+global.ggplot.options
		return(res)
	}
	if(type=="both"){
		control2<-control
		control2$add.caption<-FALSE
		p1<-histGOF(tmp,"value",refline=refline,bins=bins,control=control2)+labs(x=ylab)+facet_wrap(~variable,scales="free",nrow=1)+global.ggplot.options
		p2<-qqnormGOF(tmp,"value",refline=refline,control=control)+labs(y=ylab)+facet_wrap(~variable,scales="free",nrow=1)+global.ggplot.options

		if(!(title=="" | is.null(title))){
			p1<-p1+labs(title=title)
		}
		res<-merge2GOF(p1,p2,byrow=FALSE)
	}
}


#####################################################################################
#' @rdname basic.eta.GOF
#' @export
eta.hist.GOF<-function(data,title="",drop.fixed=TRUE,refline=TRUE,id.column="ID",standardize=TRUE,bins=NA,control=GOF.control()){
	basic.eta.GOF(data,title=title,global.ggplot.options=NULL,type="hist",refline=refline,control=control,drop.fixed=drop.fixed,id.column=id.column,standardize=standardize,bins=bins)
}

#####################################################################################
#' @rdname basic.eta.GOF
#' @export
eta.qqnorm.GOF<-function(data,title="",drop.fixed=TRUE,refline=TRUE,id.column="ID",standardize=TRUE,control=GOF.control()){
	basic.eta.GOF(data,title=title,global.ggplot.options=NULL,type="qqnorm",refline=refline,control=control,drop.fixed=drop.fixed,id.column=id.column,standardize=standardize)
}

#------------------- * *  eta.cov.GOF  * * ---------------------------------
#' Covariate and pairs ETA GOFs
#' @description
#' ETA's vs numerical covariates (eta.cov.GOF) and ETA's vs categorical covariates GOF plots on multiple
#' or single pages with reflines, data smoother, and caption added as reurned by get.caption.
#'
#' eta.pairs.GOF provides a pairs plot of all ETA's.
#' @param data
#' data.frame to plot
#' @param covariates
#' covariates, list of character strings
#' @param ETA.subset
#' index for subset of ETA's to plot. If NULL (default) all ETA's are plotted.
#' @param exclude.zero.ETA
#' If set to TRUE any ETA==0 is excluded before plotting. This option is useful if the model have full shrinkage for subset of individuals e.g. for ETA on ED50 in patents on placebo or on dose==0.
#' @param title
#' title
#' @param drop.fixed
#' drop ETA's that are fixed, i.e. any ETA with the same value in all subjects
#' @param id.column
#' column name that indicate subject identifier
#' @param standardize
#' Standardize all ETA's  (e.g. scale to unit variance)
#' @param refline
#' add reference line (default =TRUE) or not (FALSE)
#' @param type
#' do all ETA's and covariates on one page (all-in-one), one page for each covariate (covariate-by-page), or one page for each ETA (eta-by-page)
#' @param layout
#' Layout for ETA's and covariates
#' @param add.loess
#' add loess smoother to plot (TRUE), or not (FALSE)
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @examples
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
#' sdtab<-read.table(file=file1,skip=1,header=TRUE)
#' set.script.name("MyScript.R")
#' eta.cov.GOF(sdtab,covariates=c("AGE","BWT"))
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_density_2d
#' @importFrom reshape2 melt
eta.cov.GOF<-function(data,covariates=c("AGE","BWT"),ETA.subset=NULL,exclude.zero.ETA=FALSE,title="",drop.fixed=TRUE,id.column="ID",
	standardize=TRUE,refline=TRUE,type=c("all-in-one","covariate-by-page","eta-by-page"),
	layout=c("ETAbyROW","ETAbyCOL"),add.loess=TRUE,control=GOF.control()){

	layout<-match.arg(layout)
	type  <-match.arg(type)

	variable<-ETAid<-value<-NULL

	covariates=intersect(colnames(data),covariates)
	if(length(covariates)==0){
		stop("Covariates not found in data")
	}

	refline<-ifelse(refline,"href0","none")

	data<-data[!duplicated(data[,id.column]),]


	ii<-which(colnames(data) %in% c(paste("ETA",1:9,sep=""),paste("ET",10:99,sep="")))
	if(!(is.null(ETA.subset))){
	  ii<-which(colnames(data) %in% c(paste("ETA",ETA.subset,sep=""),paste("ET",ETA.subset,sep="")))
	}
	if(length(ii)==0){
		stop("No ETA's found in data")
	}
	### Get eta.labels and make sure is of same length as number of ETA's found =length(ii)
	eta.labels<-control$eta.labels
	kk<-(1:length(ii))
	if(length(eta.labels)>0){
	  if(is.null(ETA.subset)){
	    eta.labels<-c(eta.labels,rep("",999))[1:length(ii)]
	  }
	  else{
	    eta.labels<-c(eta.labels,rep("",999))[sort(ETA.subset)][1:length(ii)]
	  }
	}
	if(drop.fixed){
		if(length(ii)>1){
		  jj <- which(apply(data[, ii], 2, sd,na.rm=TRUE) > 0)
		}
		if(length(ii)==1){
		  jj <- sd(data[, ii],na.rm=TRUE) > 0
		}
		ii<-ii[jj]
		kk<-kk[jj]
	}
	if(length(ii)==0){
		stop("All ETA's found had zero variability")
	}

	ylab="ETA value"
	ylab2=""
	if(standardize){
		ylab="Standardized ETA value"
		ylab2="Standardized "
		for(i in ii){
		  sdi <- sd(data[, i],na.rm=TRUE)
		  if ((sum(!is.na(data[, i]))>1) & !is.na(sdi) & sdi > 0) {   #new
		    data[, i] <- data[, i]/sdi
		  }
		}
	}
	n_eta<-length(ii)
	tmp<-melt(data,id.vars=colnames(data)[-ii])
	if(exclude.zero.ETA){ #new
	  tmp<-subset(tmp,value!=0)  #new
	}                           #new
	if(length(eta.labels)>0){
		eta.levs<-paste(levels(tmp$variable),"(",eta.labels[kk],")",sep="")
		tmp$variable<-factor(eta.levs[as.numeric(tmp$variable)],levels=eta.levs)
	}


	ii<-match(c("variable","value"),colnames(tmp))
	colnames(tmp)[ii]<-c("ETAid","ETAvalue")
	tmp2<-melt(tmp,id.vars=setdiff(colnames(tmp),covariates))

	etas<-unique(as.character(tmp2$ETAid))

	if(type=="all-in-one" & length(covariates)>1 & n_eta>1){
		p<-do.one.GOF(tmp2,"value","ETAvalue",refline=refline,control=control,add.loess=add.loess)+
		labs(y=ylab,x="Covariate value",title=title)
		if(layout=="ETAbyROW"){
			p<-p+facet_grid(ETAid~variable,scales="free_x")
		}
		if(layout=="ETAbyCOL"){
			p<-p+facet_grid(variable~ETAid,scales="free_y")+coord_flip()
		}
	}
	if((type=="covariate-by-page" | length(covariates)==1) & n_eta>1){
		for(i in 1:length(covariates)){
			tmp3<-subset(tmp2,variable==covariates[i])
			p<-do.one.GOF(tmp3,"value","ETAvalue",refline=refline,control=control,add.loess=add.loess)+
			labs(y=ylab,x=covariates[i],title=title)+
			facet_wrap(~ETAid,scales="free_y")
			if(i<length(covariates)){
				print(p)
			}
		}
	}
	if((type=="eta-by-page" | n_eta==1) & length(covariates)>1){
		for(i in 1:n_eta){
			ylab<-paste(ylab2,etas[i],sep="")
			tmp3<-subset(tmp2,ETAid==etas[i])
			p<-do.one.GOF(tmp3,"value","ETAvalue",refline=refline,control=control,add.loess=add.loess)+
			labs(y=ylab,x="Covariate value",title=title)+
			facet_wrap(~variable,scales="free_x")
			if(i<n_eta){
				print(p)
			}
		}
	}
	if((n_eta==1 & length(covariates)==1)){
		ylab<-paste(ylab2,etas,sep="")
		p<-do.one.GOF(tmp2,"value","ETAvalue",refline=refline,control=control,add.loess=add.loess)+
		labs(y=ylab,x=covariates,title=title)
	}
	p
}

#####################################################################################
#' @rdname eta.cov.GOF
#' @param add.points
#' add jittered points of data (TRUE), or not (FALSE)
#' @examples
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
#' sdtab<-read.table(file=file1,skip=1,header=TRUE)
#' set.script.name("MyScript.R")
#' eta.cat.GOF(sdtab,covariates=c("SEXM"))
#' @export
eta.cat.GOF<-function(data,covariates=c("SEXM"),ETA.subset=NULL,exclude.zero.ETA=FALSE,title="",drop.fixed=TRUE,id.column="ID",
	standardize=TRUE,refline=TRUE,type=c("all-in-one","covariate-by-page","eta-by-page"),
	layout=c("ETAbyROW","ETAbyCOL"),add.points=TRUE,control=GOF.control()){

	layout<-match.arg(layout)
	type  <-match.arg(type)
	variable<-ETAid<-value<-NULL

	covariates=intersect(colnames(data),covariates)
	if(length(covariates)==0){
		stop("Covariates not found in data")
	}

	refline<-ifelse(refline,"href0","none")

	data<-data[!duplicated(data[,id.column]),]


	ii<-which(colnames(data) %in% c(paste("ETA",1:9,sep=""),paste("ET",10:99,sep="")))
	if(!(is.null(ETA.subset))){
	  ii<-which(colnames(data) %in% c(paste("ETA",ETA.subset,sep=""),paste("ET",ETA.subset,sep="")))
	}
	if(length(ii)==0){
		stop("No ETA's found in data")
	}
	### Get eta.labels and make sure is of same length as number of ETA's found =length(ii)
	eta.labels<-control$eta.labels
	kk<-(1:length(ii))
	if(length(eta.labels)>0){
	  if(is.null(ETA.subset)){
	    eta.labels<-c(eta.labels,rep("",999))[1:length(ii)]
	  }
	  else{
	    eta.labels<-c(eta.labels,rep("",999))[sort(ETA.subset)][1:length(ii)]
	  }
	}

	if(drop.fixed){
	  if (length(ii) > 1) {
	    jj <- which(apply(data[, ii], 2, sd,na.rm=TRUE) > 0)  ## new
	  }
	  if (length(ii) == 1) {
	    jj <- sd(data[, ii],na.rm=TRUE) > 0    ## new
	  }
		ii<-ii[jj]
		kk<-kk[jj]
	}
	if(length(ii)==0){
		stop("All ETA's found had zero variability")
	}

	ylab="ETA value"
	ylab2=""
	if(standardize){
		ylab="Standardized ETA value"
		ylab2="Standardized "
		for(i in ii){
		  sdi <- sd(data[, i],na.rm=TRUE)  ##new
		  if ((sum(!is.na(data[, i]))>1) & !is.na(sdi) & sdi > 0) {   ##new
		    data[, i] <- data[, i]/sdi
		  }
		}
	}
	n_eta<-length(ii)
	tmp<-melt(data,id.vars=colnames(data)[-ii])
	if(exclude.zero.ETA){ #new
	  tmp<-subset(tmp,value!=0)  #new
	}                           #new
	if(length(eta.labels)>0){
		eta.levs<-paste(levels(tmp$variable),"(",eta.labels[kk],")",sep="")
		tmp$variable<-factor(eta.levs[as.numeric(tmp$variable)],levels=eta.levs)
	}

	ii<-match(c("variable","value"),colnames(tmp))
	colnames(tmp)[ii]<-c("ETAid","ETAvalue")

	cov.levs<-NULL
	for(covi in covariates){
		if(is.factor(tmp[,covi])){
			cov.levs<-c(cov.levs,levels(tmp[,covi]))
		}
		else{
			cov.levs<-c(cov.levs,sort(unique(as.character(tmp[,covi]))))
		}
		tmp[,covi]<-as.character(tmp[,covi])
	}
	tmp2<-melt(tmp,id.vars=setdiff(colnames(tmp),covariates))
	tmp2$value<-factor(tmp2$value,levels=unique(cov.levs))

	etas<-unique(as.character(tmp2$ETAid))

	if(type=="all-in-one" & length(covariates)>1 & n_eta>1){
		p<-do.cat.GOF(tmp2,"value","ETAvalue",refline=refline,control=control,add.points=add.points)+
		labs(y=ylab,x="Covariate value",title=title)
		if(layout=="ETAbyROW"){
			p<-p+facet_grid(ETAid~variable,scales="free_x")
		}
		if(layout=="ETAbyCOL"){
			p<-p+facet_grid(variable~ETAid,scales="free_y")+coord_flip()
		}
	}
	if((type=="covariate-by-page" | length(covariates)==1) & n_eta>1){
		for(i in 1:length(covariates)){
			tmp3<-subset(tmp2,variable==covariates[i])
			p<-do.cat.GOF(tmp3,"value","ETAvalue",refline=refline,control=control,add.points=add.points)+
			labs(y=ylab,x=covariates[i],title=title)+
			facet_wrap(~ETAid,scales="free_y")
			if(i<length(covariates)){
				print(p)
			}
		}
	}
	if((type=="eta-by-page" | n_eta==1) & length(covariates)>1){
		for(i in 1:n_eta){
			ylab<-paste(ylab2,etas[i],sep="")
			tmp3<-subset(tmp2,ETAid==etas[i])
			p<-do.cat.GOF(tmp3,"value","ETAvalue",refline=refline,control=control,add.points=add.points)+
			labs(y=ylab,x="Covariate value",title=title)+
			facet_wrap(~variable,scales="free_x")
			if(i<n_eta){
				print(p)
			}
		}
	}
	if((n_eta==1 & length(covariates)==1)){
		ylab<-paste(ylab2,etas,sep="")
		p<-do.cat.GOF(tmp2,"value","ETAvalue",refline=refline,control=control,add.points=add.points)+
		labs(y=ylab,x=covariates,title=title)
	}
	p
}


#####################################################################################
#' @rdname eta.cov.GOF
#' @param density2D
#' add 2D-density above (upper) or below (lower), or don't add (none)
#' @examples
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
#' sdtab<-read.table(file=file1,skip=1,header=TRUE)
#' set.script.name("MyScript.R")
#' eta.pairs.GOF(sdtab)
#' @export
eta.pairs.GOF<-function(data,ETA.subset=NULL,title="",drop.fixed=TRUE,id.column="ID",
	density2D=c("none","upper","lower"),
	standardize=TRUE,control=GOF.control()){

	density2D<-match.arg(density2D)

	data<-data[!duplicated(data[,id.column]),]

	value1<-value2<-id1<-id2<-NULL

	ii<-which(colnames(data) %in% c(paste("ETA",1:9,sep=""),paste("ET",10:99,sep="")))
	if(!(is.null(ETA.subset))){
	  ii<-which(colnames(data) %in% c(paste("ETA",ETA.subset,sep=""),paste("ET",ETA.subset,sep="")))
	}

	if(length(ii)==0){
		stop("No ETA's found. Input data must have at least one column named ETA1,ETA2,..")
	}
	### Get eta.labels and make sure is of same length as number of ETA's found =length(ii)
	eta.labels<-control$eta.labels
	kk<-(1:length(ii))
	if(length(eta.labels)>0){
	  if(is.null(ETA.subset)){
	    eta.labels<-c(eta.labels,rep("",999))[1:length(ii)]
	  }
	  else{
	    eta.labels<-c(eta.labels,rep("",999))[sort(ETA.subset)][1:length(ii)]
	  }
	}
	if(drop.fixed){
		if(length(ii)==1){
			jj<-which(sd(data[,ii])>0)
		}
		if(length(ii)>1){
			jj<-which(apply(data[,ii],2,sd)>0)
		}
		ii<-ii[jj]
		kk<-kk[jj]
	}
	if(length(ii)==0){
		stop("All ETA's have zero variance. Try adding option drop.fixed=FALSE")
	}
	ylab="ETA value"
	if(standardize){
		ylab="Standardized ETA value"
		for(i in ii){
			sdi<-sd(data[,i])
			if(sdi>0){
				data[,i]<-data[,i]/sdi
			}
		}
	}

	ETAs2<-data[,ii]
	colnames(ETAs2)<-paste(colnames(ETAs2),"_2",sep="")
	data<-cbind(data,ETAs2)
	neta<-length(ii)

	tmp1<-melt(data,measure.vars=colnames(ETAs2),variable.name = "variable1",value.name = "value1")
	tmp2<-melt(tmp1,measure.vars=gsub("_2","",colnames(ETAs2)),variable.name = "variable2",value.name = "value2")
	tmp2$id1<-as.numeric(tmp2$variable1)
	tmp2$id2<-as.numeric(tmp2$variable2)
	tmp2$variable1<-factor(levels(tmp2$variable2)[as.numeric(tmp2$variable1)],levels=levels(tmp2$variable2))

	if(length(eta.labels)>0){
		eta.levs<-paste(levels(tmp2$variable1),"(",eta.labels[kk],")",sep="")
		tmp2$variable1<-factor(eta.levs[as.numeric(tmp2$variable1)],levels=eta.levs)
		tmp2$variable2<-factor(eta.levs[as.numeric(tmp2$variable2)],levels=eta.levs)
	}

	tmp2$pval1<-NA
	tmp2$pval2<-NA
	ii<-with(tmp2,id1<id2)
	tmp2$pval1[ii]<-tmp2$value1[ii]
	tmp2$pval2[ii]<-tmp2$value2[ii]

	pp<-ggplot(tmp2,aes(value1,value2))+
	geom_point(aes(value1,value2),data=subset(tmp2,id1<id2),col = control$col.data,cex = control$cex.data,pch = control$pch.data,alpha = control$alpha.data)+
	stat_denx(aes(value1,value2),data=subset(tmp2,id1==id2),col=control$col.hist,fill=control$fill.hist,alpha=control$alpha.hist)
	if(density2D=="upper"){
		pp<-pp+geom_density_2d(aes(value1,value2),data=subset(tmp2,id1>id2),col=control$col.smooth)
	}
	if(density2D=="lower"){
		pp<-pp+geom_density_2d(aes(value1,value2),data=subset(tmp2,id1<id2),col=control$col.smooth)
	}
	pp<-pp+
	stat_corr(aes(value1,value2),data=subset(tmp2,id1>id2),fontface=control$corr.fontface)+
	facet_grid(variable2~variable1,scales="free")+labs(y=ylab,x=ylab,title=title)

	if(control$add.caption){
		pp<-add.caption(pp,control)
	}

	pp
}
