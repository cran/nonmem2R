#' Visual Predictive Check (VPC) based on raw Perl-speaks-NONMEM (PsN) generated VPC files.
#' @description
#' This function compile VPC simulated data from the vpc command in Pearl Speaks NONMEM (PsN).
#' The complied data is then plotted (unless type=0) and returned (ggplot-object) and can then be
#' further modifed.
#' @param sdtab
#' file path to sdtab output from model, ALTERNATIVELY a data.fram of the loaded sdtab file. See details.
#' @param vpcdir
#' Path of directory of the VPC files, ALTERNATIVELY a data.frame of the loaded matrix file. See details.
#' @param use.model.path
#' Load files from a global defined model library (TRUE=default).
#' If so will look for a global character vector named \code{model.path}
#' @param dv
#' Dependent variable (y-axis)
#' @param idvs
#' Independent variable (x-axis)
#' @param numerical
#' Logical indicator if idvs is numerical variable(TRUE) or catacorical (FALSE), default=TRUE
#' NOTE: must be of same length as idvs
#' @param strata
#' Stratification column of sdtab, NOTE must be a columns in the sdtab
#' @param method
#' either "loess" (deault) or "spline" for continuous IDV's
#' @param pred.corr
#' Set to 	"pred-corr-prop" or "pred-corr-add" to preform prediction corrected VPC or set to
#' "none" (default) otherwhise.  With "pred-corr-prop" correction is recomened for stricly
#' positive data, use "pred-corr-add" if some data point are negative,
#' @param maxNsim
#' maximum number of simulations used to use for computing VPC.
#' Default=NULL, meaning all simulations in the input datafile/data.frame will be used.
#' @param knots
#' number of knots (bins) for method spline. See details.
#' @param minobs
#' minimum number of observations per number of knots(bins) for method spline. See details.
# @param strata.names
# Character vector for strata names. Must have length equal to number of strata's in vpc files, otherwise ignored.
# @param strata.subset
# Vector specifying subset of strata to use. Either a vector of index, e.g.  strata.subset=c(1,3),
# or a character vector naming which strata to use. See details.
# @param percentile
# percentile to use, default=10 will display 10% 50% and 90% percentile for model and data.
# Percentile argument must match columns included in the vpcresult file.
# @param xlab
# label for x-axis, default is IDV as found in vpcresultfile, passed to labs.
# @param ylab
# label for y-axis, default is DV as found in vpcresultfile, passed to labs.
# @param fy
# transformation function for y-axis, default to identity function (f(y)=y).
# @param fx
# transformation function for x-axis, default to identity function (f(x)=x)
#' @param col.data
#' color for data points, passed to geom_point, default="gray20"
#' @param cex.data
#' cex for data points, passed to geom_point, default=1
#' @param pch.data
#' plot symbol (pch) for data points, passed to geom_point, default=19
#' @param alpha.data
#' alpha for plotting of data points, passed to geom_point, default=0.5
#' @param col.line
#' line color for mean of data, passed to geom_line, default="gray20"
#' @param lwd.line
#' line width (lwd) for mean of data, passed to geom_line, default=1
#' @param lty.line
#' line type (lty) for mean of data, passed to geom_line, default=1
#' @param col.segm
#' Color of CI region for model, passed to geom_ribbon, default="steelblue"
#' @param alpha.segm
#' alpha of CI region for model, passed to geom_polygon, default=0.6
#' @param type
#' type of VPC plot:
#'
#' 1=display model regions only,
#'
#' 2=as 1 + mean of data,
#'
#' 3= as 2 + points for data.
#'
#' type=0 can also be used and then no graph is produced but instead a list with 2 dataframes is returned.
#' one for the vpcresult and on for the observed data in the vpctab file.
#' @param control
#' an optional list of control settings. See GOF.control for the names of the settable control values and their effect.
#' @param ...
#' Additional named arguments (e.g. span and degree) are passed to loess.smooth.
#' @return
#' A ggplot object if type>0, or if type==0 a list of 3 data.frames, D1=individual data, D3=mean data, D5=mean model with confidence intervals
#' @details
#' The sdtab and vpcdir can either be file name of the sdtab file and the folder name of the psn generated VPC,
#' or can be data.frames of sdtab and simulation data loaded outside vpcfig3, see example below for the latter case.
#'
#' Loading the sdtab and matrix file outside vpcfig2 is convenient when dosing multipe VPC plots or for VPC
#' using new strata ( or idv) variables not included in the sdtab file, see example.
#'
#' If vpcdir is given as a folder names, then a matrix.csv file is loaed from the /m1 subfolder of vpcdir.
#'
#' The raw simulated results from the matrix file is loaded to compute mean of data with confidence intervals
#' For numerical idv variables loess-smooth or linear spline is used on observed actual data and on simulated data.
#' For categorical idv mean is computed for each unique value if idv
#'
#' For method=spline, the actual number of knots(bins) is set as min(knots,N/minobs) where N is the number of observations.
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 facet_grid
#' @importFrom stats aggregate
#' @importFrom stats loess.smooth
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats lm
#' @importFrom stats spline
#' @importFrom splines2 bSpline
#'
#' @examples
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "sdtab", package = "nonmem2R")
#' file2 <- system.file("extdata", "DV_matrix.csv", package = "nonmem2R")
#'
#' #load sdtab and matrix file
#' sdtab<-read.table(file=file1,skip=1,header=TRUE)
#' vpc<-read.table(file=file2,header=FALSE,sep=",")
#'
#' # VPC stratified by gender
#' vpcfig3(sdtab,vpc,dv="DV",idvs="TAD",strata="SEX")
#'
#' # create new strata variable and do VPC
#' sdtab$age.group<-cut(sdtab$AGE,c(22,30,50))
#' vpcfig3(sdtab,vpc,dv="DV",idvs="TAD",strata="age.group")
#################
vpcfig3<-function(sdtab,
                  vpcdir,
                  use.model.path=TRUE,
                  dv="DV",
                  idvs="TIME",
                  numerical=rep(TRUE,length(idvs)),
                  strata=NULL,
                  method=c("loess","spline"),
				  pred.corr=c("none","pred-corr-pro p","pred-corr-add"),
                  maxNsim=NULL,
                  knots=5, minobs=8,
                  col.data="gray20",cex.data=1,pch.data=19,alpha.data=0.5,
                  col.line='grey20',lwd.line=1,lty.line=1,
                  col.segm="steelblue",
                  alpha.segm=0.6,
                  type=3,
				          control=GOF.control(),...)
{
  #### Check for global model.path
  #  file.path<-""
  #  model.path.ok<-FALSE
  #  if(use.model.path & exists("model.path")){
  #    eval(parse(text="model.path.ok<-dir.exists(model.path)"))
  #    if(model.path.ok){
  #      eval(parse(text="file.path<-model.path"))
  #    }
  #  }
  file.path<-get.model.path()

	method <- match.arg(method)
	pred.corr<-match.arg(pred.corr)

	x<-y<-MDV<-g_g<-group<-group2<-q5<-q95<-x_x_x<-y_y_y<-NULL

	d10<-sdtab
	if(inherits(sdtab,"character")){
		file1<-paste(file.path,sdtab,sep="")
		cat("loading sdtab data from ",file1,"\n")
		#### 1) load the sdtab file and remove any DV=1 data, rename DV variable to y_y_y, and idv variable to x_x_x
		d10<-read.table(file=file1,skip=1,header=T)
	}
	d10<-subset(d10,MDV==0)

	d20<-vpcdir
	if(inherits(vpcdir,"character")){
		#### 2) load the matrix file
		file1<-paste(file.path,vpcdir,"/m1/",dv,"_matrix.csv",sep="")
		cat("loading simulation data from ",file1,"\n")
		d20<-read.table(file=file1,sep=",",header=F)
	}

	#### 3) check d1 and d2 have same number of rows, if not abort
	if(nrow(d10)!=nrow(d20)){
		cat("Ups!, sdtab data(with mdv==0) and simulation file don't match in size ( number of rows)\n")
		stop("Aborting\n")
	}

	if(!is.null(strata)){
		if(!(strata %in% colnames(d10))){
			cat("Ups, strata variable",strata,"is not among columns of",sdtab,"\n" )
			stop("Aborting")
		}
	}

	if(!is.null(strata)){
		if((strata %in% idvs)){
			cat("Ups, strata variable",strata,"is one of you idvs. Suggest you remove",strata,"from your list of idvs\n" )
			stop("Aborting")
		}
	}

	#### 3) Handle strata's
	if(is.null(strata)){
		d10$s_trata_s<-"S0"
	}
	if(!is.null(strata)){
		colnames(d10)[colnames(d10)==strata]<-"s_trata_s"
	}
	colnames(d10)[colnames(d10)=="s_trata_s"]<-"strata"
	d10$strata<-factor(d10$strata)
	strata.levs<-levels(d10$strata)


	#### Check for columnd PRED if doing pred.corr
	if(pred.corr!="none" & !("PRED" %in% colnames(d10))){
			cat("PRED column must be present in vpctab for option pred.corr=",pred.corr,"\n" )
			stop("Aborting")
	}

	#### maxNsim
	if(is.null(maxNsim)){
		maxNsim=ncol(d20)
	}
	if(maxNsim<25){
		message("Using maxNsim<25 is not adviced\n")
	}
	if(maxNsim>ncol(d20)){
		message("maxNsim > number of simulations in input. maxNsim ignored\n")
		maxNsim=ncol(d20)
	}


	#### Placeholders for results
	D1<-NULL
	D3<-NULL
	D5<-NULL

	#### Then loop across idvs
	for(k in 1:length(idvs)){

		idv<-idvs[k]

		#### 3) rename DV variable to y_y_y, and idv variable to x_x_x
		d1<-d10
		colnames(d1)[colnames(d1)==dv]<-"y_y_y"
		colnames(d1)[colnames(d1)==idv]<-"x_x_x"
		d1<-d1[,c("y_y_y","x_x_x","strata")]
		if(pred.corr!="none"){
			d1<-d1[,c("y_y_y","x_x_x","strata","PRED")]
		}

		#### 4) load the matrix file
		d2<-d20[,1:maxNsim]

		##### Categorical covariate
		if(!numerical[k]){

			d3<-d5<-NULL
			for(ss in strata.levs){
				ii<-d1$strata==ss
#				##observed
#				d3i<-aggregate(y_y_y~x_x_x*strata,data=d1[ii,],mean)
#				##simulated
#				f<-function(x){aggregate(x~d1$x_x_x[ii],FUN=mean)[,2]}
#				tmp<-apply(d2[ii,-1],2,f)
#				## Get mean and quantiles of loess curves simulation
#				d5i<-d3i
#				if(NCOL(tmp)==1){tmp<-matrix(tmp,nrow=1)}
#				d5i$y_y_y<-apply(tmp,1,mean)
#				d5i$q5<-apply(tmp,1,quantile,0.05)
#				d5i$q95<-apply(tmp,1,quantile,0.95)
#				d3<-rbind(d3,d3i)
#				d5<-rbind(d5,d5i)
				tmp<-categorical_vpc_sub(d1[ii,],d2[ii,],ss,pred.corr,...)
				d3<-rbind(d3,tmp[[1]])
				d5<-rbind(d5,tmp[[2]])
				d1$y_y_y[ii]<-tmp[[3]]
			}

			xxxvals<-sort(unique(d3$x_x_x))
			dxx<-min(diff(xxxvals))

			#### iv) Expand around unique covariate values
			d6<-data.frame(w=c(-1/6,1/6))*1.5
			d3<-merge(d6,d3)
			d5<-merge(d6,d5)

			d1$x_x_x<-d1$x_x_x+runif(nrow(d1),-dxx/6,dxx/6)

			d3$group<-"Mean"
			d5$group2<-"Mean with CI"

			d3$g_g<-d3$x_x_x
			d5$g_g<-d5$x_x_x

			d3$x_x_x<-d3$x_x_x+d3$w
			d5$x_x_x<-d5$x_x_x+d5$w

		}
		##### Numerical covariate
		if(numerical[k]){
			d3<-d5<-NULL
			for(ss in strata.levs){
				ii<-ii<-d1$strata==ss
				if(method=="loess" & pred.corr=="none"){
					tmp<-loess_vpc_sub(d1[ii,],d2[ii,],ss,...)
				}
				if(method=="loess"  & pred.corr!="none"){
					tmp<-loess_vpc_sub_predcorr(d1[ii,],d2[ii,],ss,pred.corr,...)
				}
				if(method=="spline"){
					tmp<-loess_spline_sub(d1[ii,],d2[ii,],ss,minobs,knots,...)
				}
				d3<-rbind(d3,tmp[[1]])
				d5<-rbind(d5,tmp[[2]])
				d1$y_y_y[ii]<-tmp[[3]]
			}

			d3$group<-"Mean"
			d5$group2<-"Mean with CI"

			d3$w<-0
			d5$w<-0

			d3$g_g<-1#dummy grouping, just to be able to rbind with categorical data
			d5$g_g<-1
		}

		### Add ids label
		d3$v_v<-idv
		d5$v_v<-idv
		d1$v_v<-idv

		### Accumulate results
		D1<-rbind(D1,d1)
		D3<-rbind(D3,d3)
		D5<-rbind(D5,d5)
	}

	D1$v_v<-factor(D1$v_v,idvs)
	D3$v_v<-factor(D3$v_v,idvs)
	D5$v_v<-factor(D5$v_v,idvs)

	### Rename columns for more understable output
	oldn<-c("x_x_x","y_y_y","v_v")
	newn<-c("x","y","idv")
	colnames(D1)[match(oldn,colnames(D1))]<-newn
	colnames(D3)[match(oldn,colnames(D3))]<-newn
	colnames(D5)[match(oldn,colnames(D5))]<-newn


	##### iv) Plot result
	p1<-ggplot(D3)
	if(type>2){
		## Plot data points
		p1<-p1+geom_point(aes(x,y),data=D1,color = col.data,alpha = alpha.data, cex = cex.data)
	}
	## Plot model
	if(type>0){
		p1<-p1+geom_ribbon(aes(x,ymin=q5,ymax=q95,group=g_g,fill=group2),data=D5,alpha = alpha.segm)+
		geom_line(aes(x,y,group=g_g,linetype=group2),data=D5,lwd = lwd.line,color=col.segm)+
			labs(x="",y=dv,fill="Model",linetype="Model")
	}
	if(type>1){
		## Plot mean data
		p1<-p1+geom_line(aes(x,y,color=group,group=g_g),lwd = lwd.line)+
			labs(x="",y=dv,fill="Model",linetype="Model",color="Data")
	}
	p1<-p1+
		scale_fill_manual(values=col.segm)+
		scale_linetype_manual(values=lty.line)+
		scale_color_manual(values=col.line)
	if(length(strata.levs)>1 & length(idvs)>1){
		p1<-p1+facet_grid(strata~idv,scales="free_x")
	}
	if(length(strata.levs)==1 & length(idvs)>1){
		p1<-p1+facet_wrap(~idv,scales="free_x")
	}
	if(length(strata.levs)>1 & length(idvs)==1){
		p1<-p1+facet_wrap(~strata,scales="free_x")
	}
	if(length(idvs)==1){
		p1<-p1+labs(x=idvs)
	}
	if(control$add.caption){
	  p1<-add.caption(p1,control)
	}
	res<-list(data=D1,mean.data=D3,model=D5)
	if(type>0){res<-p1}
	res

}
