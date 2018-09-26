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
                  maxNsim=NULL,
                  knots=5, minobs=8,
                  col.data="gray20",cex.data=1,pch.data=19,alpha.data=0.5,
                  col.line='grey20',lwd.line=1,lty.line=1,
                  col.segm="steelblue",
                  alpha.segm=0.6,
                  type=3)
{
  #### Check for global model.path
  file.path<-""
  model.path.ok<-FALSE
  if(use.model.path & exists("model.path")){
    eval(parse(text="model.path.ok<-dir.exists(model.path)"))
    if(model.path.ok){
      eval(parse(text="file.path<-model.path"))
    }
  }

  method <- match.arg(method)

  x<-y<-MDV<-g_g<-group<-group2<-q5<-q95<-x_x_x<-y_y_y<-NULL

  d10<-sdtab
  if(class(sdtab)=="character"){
    file1<-paste(file.path,sdtab,sep="")
    cat("loading sdtab data from ",file1,"\n")
    #### 1) load the sdtab file and remove any DV=1 data, rename DV variable to y_y_y, and idv variable to x_x_x
    d10<-read.table(file=file1,skip=1,header=T)
  }
  d10<-subset(d10,MDV==0)

  d20<-vpcdir
  if(class(vpcdir)=="character"){
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

    #### 4) load the matrix file
    d2<-d20[,1:maxNsim]

    ##### Categorical covariate
    if(!numerical[k]){

      d3<-d5<-NULL
      for(ss in strata.levs){
        ii<-d1$strata==ss
        ##observed
        d3i<-aggregate(y_y_y~x_x_x*strata,data=d1[ii,],mean)
        ##simulated
        f<-function(x){aggregate(x~d1$x_x_x[ii],FUN=mean)[,2]}
        tmp<-apply(d2[ii,-1],2,f)
        ## Get mean and quantiles of loess curves simulation
        d5i<-d3i
        if(NCOL(tmp)==1){tmp<-matrix(tmp,nrow=1)}
        d5i$y_y_y<-apply(tmp,1,mean)
        d5i$q5<-apply(tmp,1,quantile,0.05)
        d5i$q95<-apply(tmp,1,quantile,0.95)
        d3<-rbind(d3,d3i)
        d5<-rbind(d5,d5i)
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

      #### i) Do loess on observed data and simulated data
      d3<-d5<-NULL
      if(method=="loess"){
        for(ss in strata.levs){
          ii<-d1$strata==ss
          ##observed
          tmp<-loess.smooth(d1$x_x_x[ii],d1$y_y_y[ii])
          d3i<-data.frame(x_x_x=tmp$x,y_y_y=tmp$y,strata=ss)
          ##simulated
          f<-function(x){loess.smooth(d1$x_x_x[ii],x)$y}
          #f<-function(x){loess.smooth.predict(d1$x_x_x[ii],x,d3i$x_x_x)$y}
          tmp<-apply(d2[ii,-1],2,f)
          ## Get mean and quantiles of loess curves simulation
          d5i<-d3i
          d5i$y_y_y<-apply(tmp,1,mean)
          d5i$q5<-apply(tmp,1,quantile,0.05)
          d5i$q95<-apply(tmp,1,quantile,0.95)
          d3<-rbind(d3,d3i)
          d5<-rbind(d5,d5i)
        }
      }
      if(method=="spline"){
        for(ss in strata.levs){
          ii<-d1$strata==ss
          knots2<-min(floor(sum(ii)/minobs),knots)
          #print(c(sum(ii),knots,knots2))
          bsi<-bSpline(d1$x_x_x[ii],degree=1,df=knots2+1,intercept=TRUE)
          x.new<-sort(c(attr(bsi,"knots"),attr(bsi,"Boundary.knots")))
          bsp<-bSpline(x.new,degree=1,knots=attr(bsi,"knots"),intercept=TRUE)
          ##observed
          mi<-lm(d1$y_y_y[ii]~bsi-1)
          y.new<-(bsp%*%mi$coef)[,1]
          d3i<-data.frame(x_x_x=x.new,y_y_y=y.new,strata=ss)
          ##simulated
          f<-function(x){	mi<-(bsp%*%lm(x~bsi-1)$coef)[,1]}
          tmp<-apply(d2[ii,-1],2,f)
          ## Get mean and quantiles of loess curves simulation
          d5i<-d3i
          d5i$y_y_y<-apply(tmp,1,mean)
          d5i$q5<-apply(tmp,1,quantile,0.05)
          d5i$q95<-apply(tmp,1,quantile,0.95)
          d3<-rbind(d3,d3i)
          d5<-rbind(d5,d5i)
        }
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
  res<-list(data=D1,mean.data=D3,model=D5)
  if(type>0){res<-p1}
  res

}



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
                 percentile=10,
                 fy=function(y){y},fx=function(x){x},
                 xlab=NULL,
                 ylab=NULL,
                 col.data="gray20",cex.data=1,pch.data=19,alpha.data=0.5,
                 col.line='grey20',lwd.line=1,lty.line=c(2,1,2),
                 col.segm=c("steelblue", "gray50", "steelblue"),
                 alpha.segm=0.6,
                 type=3)
{
  #### Check for global model.path
  file.path<-""
  model.path.ok<-FALSE
  if(use.model.path & exists("model.path")){
    eval(parse(text="model.path.ok<-dir.exists(model.path)"))
    if(model.path.ok){
      eval(parse(text="file.path<-model.path"))
    }
  }

  if((is.null(vpctab) | is.null(vpcresult)) & is.null(vpcdir)){
    stop("Use either both vpctab and vpcresult or vpcdir")
  }
  if(is.null(vpctab)){
    vpctab<-list.files(path=paste(file.path,vpcdir,sep=""),pattern="vpctab",full.names=TRUE)
    if(length(vpctab)==0){
      if(file.path!=""){
        warning(paste("No vpctab file found in folder",paste(file.path,vpcdir,sep=""),". Trying without using the specified model.path"))
        file.path<-""
        vpctab<-list.files(path=paste(file.path,vpcdir,sep=""),pattern="vpctab",full.names=TRUE)
      }
      if(length(vpctab)==0){
        stop(paste("No vpctab file found in folder",paste(file.path,vpcdir,sep="")))
      }
    }
  }
  if(is.null(vpcresult)){
    vpcresult<-paste(file.path,vpcdir,"/vpc_results.csv",sep="")
  }

  if(!file.exists(vpctab) | !file.exists(vpcresult)){
    stop(paste("files",vpctab,"or",vpcresult,"does not exist"))
  }
  cat("VPC based on files:\n  ",vpctab,"\nand\n  ",vpcresult,"\n")

  dd1<-read.table(file=vpctab,sep=",",header=T)
  dd1$id<-as.numeric(factor(dd1$ID))

  dd2<-readVpc(vpc.results = vpcresult)              # Alt1

  ## Change names for DV and TIME in the raw data dataframe dd1
  ii<-match(c(dd2$dv.var,dd2$idv.var),colnames(dd1))
  colnames(dd1)[ii]<-c("DV","TIME")

  if(is.null(ylab)){ylab<-dd2$dv.var}
  if(is.null(xlab)){xlab<-dd2$idv.var}

  ## Check for stratas, if none, create dummy strata
  n.strata<-length(dd2$strata.names)
  if(n.strata==0){
    dd2$strata.names<-"dummyStrata"
    dd2$result.tables<-list(dd2$result.tables)
    dd1$strata_no<-1
  }


  ## Change strata names if the length of specified new names match actual number of strata
  n.strata<-length(dd2$strata.names)
  if(length(strata.names)!=n.strata){
    strata.names<-dd2$strata.names
  }

  if(length(strata.names)==n.strata){
    dd2$strata.names<-strata.names
    dd1$strata<-strata.names[dd1$strata_no]
  }
  if(length(strata.names)!=n.strata){
    dd1$strata<-dd2$strata.names[dd1$strata_no]
  }
  strata.names<-dd2$strata.names

  ## Set strata subset
  if(is.null(strata.subset)){
    strata.subset<-1:n.strata
  }

  if(all(strata.subset %in% paste(1:n.strata))){
    strata.subset<-strata.names[strata.subset]
  }
  else{
    if(!all(strata.subset %in% strata.names)){
      strata.subset<-strata.names
      warning("strata.subset ignored. Subset should be subset of strata.names or subset of 1:",n.strata)
    }
  }
  dd2$strata.names
  n.strata.subset<-length(strata.subset)

  ## Select strata.subset from observation dataframe
  dd1<-dd1[dd1$strata %in% strata.subset,]
  dd1$strata<-factor(dd1$strata,levels=strata.subset)


  #### Put toghter data frames suited for xyplot with panels=strata
  model0<-NULL
  data0<-NULL
  strata<-1

  strata.indexs<-match(strata.subset,strata.names)
  for(strata in strata.indexs){
    Z<-dd2$result.tables[[strata]]
    colnames(Z)<-paste("X",colnames(Z),sep="")

    ## Handle case when Xlower is NA; set to Xupper
    ii<-is.na(Z$Xlower)
    Z$Xlower[ii]<-Z$Xupper

    #1 remove XX.CI from column names
    n1<-gsub("[123456789][123456789].CI.for","",colnames(Z))
    #2 find right columns for percentile, & rename
    iL<-c(grep(paste("X",percentile,"[.]",sep=""),n1),
          grep(paste("X[.]",percentile,"[.]",sep=""),n1))

    iU<-c(grep(paste("X",100-percentile,"[.]",sep=""),n1),
          grep(paste("X[.]",100-percentile,"[.]",sep=""),n1))
    iM<-grep(paste(50),n1)

    #3 find columns found, if not all found, abort
    if(length(iL)<4 |  length(iU)<4 | length(iM)<4){
      stop(paste("Column with percentile",percentile,", 50, ","or",100-percentile,"was not found in data file"))
    }

    n1[iL]<-gsub(paste(percentile),"L",n1[iL])
    n1[iM]<-gsub("50","M",n1[iM])
    n1[iU]<-gsub(paste(100-percentile),"U",n1[iU])

    colnames(Z)<-n1

    ### Add the last and first row once again with endpints on x-axis
    Z<-Z[c(1,1:nrow(Z),nrow(Z)),]
    Z$Xupper[1]<-Z$Xlower[1]
    Z$Xlower[nrow(Z)]<-Z$Xupper[nrow(Z)]


    datai<-data.frame(  x=(Z$Xupper+Z$Xlower)/2,
                       low=Z$XL.real,
                       med=Z$XM.real,
                       upp=Z$XU.real)
    datai$strata<-dd2$strata.names[strata]
    data0<-rbind(data0,datai)

    ### Add the last and first row once again with endpints on x-axis
    ## here old code for using polygon
    #  x1<-(Z$Xupper+Z$Xlower)/2
    #  x1<-c(x1,rev(x1))
    #  modeli<-data.frame(x=x1,
    #                   low=c(   Z$X.L.from,
    #                          rev(Z$X.L.to)),
    #                   med=c(   Z$X.M.from,
    #                          rev(Z$X.M.to)),
    #                   upp=c(   Z$X.U.from,
    #                          rev(Z$X.U.to))
    #  )

    ## here now code for using ribbon
    x1<-(Z$Xupper+Z$Xlower)/2
    modeli<-data.frame(x=x1,
                          lowL=Z$X.L.from,
                          lowU=Z$X.L.to,
                          medL=Z$X.M.from,
                          medU=Z$X.M.to,
                          uppL=Z$X.U.from,
                          uppU=Z$X.U.to)

    ### end new code

    modeli$strata<-dd2$strata.names[strata]
    model0<-rbind(model0,modeli)

  }

  data0$strata<-factor(data0$strata,levels=strata.subset)
  model0$strata<-factor(model0$strata,levels=strata.subset)

  if(type==0){
    pp<-list(model0=model0,data0=data0,dd1=dd1)
  }
  if(type>0){
	##### Rformatting of data0 and model0
	group.labs<-c(paste(percentile,"th percentile",sep=""),"Median",paste(100-percentile,"th percentile",sep=""))


	to_thin<-function(x,labs=c("2.5th percentile??","Median ?!?","97.5th percentile")){
		low<-x[,!(colnames(x) %in% c("med","upp","medU","uppU","medL","uppL"))]
		med<-x[,!(colnames(x) %in% c("low","upp","lowU","uppU","lowL","uppL"))]
		upp<-x[,!(colnames(x) %in% c("low","med","lowU","medU","lowL","medL"))]
		low$group<-labs[1]
		med$group<-labs[2]
		upp$group<-labs[3]
		colnames(low)[colnames(low)=="low"]<-"value"
		colnames(med)[colnames(med)=="med"]<-"value"
		colnames(upp)[colnames(upp)=="upp"]<-"value"

		colnames(low)[colnames(low)=="lowL"]<-"ymin"
		colnames(med)[colnames(med)=="medL"]<-"ymin"
		colnames(upp)[colnames(upp)=="uppL"]<-"ymin"

		colnames(low)[colnames(low)=="lowU"]<-"ymax"
		colnames(med)[colnames(med)=="medU"]<-"ymax"
		colnames(upp)[colnames(upp)=="uppU"]<-"ymax"

		y<-rbind(low,med,upp)
		y$group<-factor(y$group,levels=rev(labs))
		y
	}

	model0<-to_thin(model0,labs=group.labs)
	model0$group2<-model0$group

	data0<-to_thin(data0,labs=group.labs)

	#####
	pp <- ggplot(data = NULL)

	# Add shadded areas from model
	# old code using polygon
	#pp <- pp + geom_polygon(aes_string(x = 'fx(x)', y = 'fy(value)',group='group2',fill='group2'),data=model0,alpha=alpha.segm)+
  #labs(fill="Model")

	# Add shadded areas from model
	# new code using ribbon
	pp <- pp + geom_ribbon(aes_string(x = 'fx(x)', ymin = 'fy(ymin)', ymax = 'fy(ymax)',group='group2',fill='group2'),data=model0,alpha=alpha.segm)+
	labs(fill="Model")

	if(type>1){ # Add lines from data
		pp <- pp + geom_line(aes_string(x='fx(x)',y='fy(value)',group='group',linetype='group'),data=data0,lwd=lwd.line,color=col.line)+
		labs(linetype="Data")
	}
	if(type>2){ # Add points for from data
		pp <- pp + geom_point(aes_string(x='fx(TIME)',y='fy(DV)'),data=dd1,color=col.data,alpha=alpha.data,cex=cex.data)
	}

	## Add formatting
	pp<-pp+
	labs(y=ylab,x=xlab)+
	scale_fill_manual(values=col.segm)+
	scale_linetype_manual(values=lty.line)

	## Add strata if needed
	if(n.strata>1 & length(strata.subset)>1){
		pp<-pp+facet_wrap(~strata)
	}
  }
  pp
}

