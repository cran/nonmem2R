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
                  control=GOF.control())
{

  bin.idv<- match.arg(bin.idv)

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
  group<-group2<-NULL

  if((is.null(vpctab) | is.null(vpcresult)) & is.null(vpcdir)){
    stop("Use either both vpctab and vpcresult or vpcdir")
  }
  if(is.null(vpctab)){
    vpctab<-list.files(path=paste(file.path,vpcdir,sep=""),pattern="vpctab",full.names=TRUE)
    vpctab<-setdiff(vpctab,vpctab[grep("old",substr(vpctab,nchar(vpctab)-4,nchar(vpctab)))])
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
    vpcresult<-setdiff(vpcresult,vpcresult[grep("old",substr(vpcresult,nchar(vpcresult)-4,nchar(vpcresult)))])
  }

  if(!file.exists(vpctab) | !file.exists(vpcresult)){
    stop(paste("files",vpctab,"or",vpcresult,"does not exist"))
  }
  cat("VPC based on files:\n  ",vpctab,"\nand\n  ",vpcresult,"\n")

  dd1<-read.table(file=vpctab,sep=",",header=T)
  dd1$id<-as.numeric(factor(dd1$ID))

  dd2<-readVpc(vpc.results = vpcresult)   # Alt1

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

    ### Get range of actual data in strata
    ### Use this to recuce width of first and last bin
    range.idv<-range(dd1$TIME[dd1$strata_no==strata])
    min.idv<-min(c(range.idv,Z$Xupper))
    max.idv<-max(c(range.idv,Z$Xlower))

    ### Add the last and first row once again with endpoints on x-axis
    Z<-Z[c(1,1:nrow(Z),nrow(Z)),]
    Z$Xupper[1]<-Z$Xlower[1]<-min.idv
    Z$Xlower[nrow(Z)]<-Z$Xupper[nrow(Z)]<-max.idv


    #### Compute x-axis location for bin: bin-midpoint, or median idv of bin
    xx<-(Z$Xupper+Z$Xlower)/2
    if(bin.idv=="median"){
      xx<-Z$Xnobs
      xx[1]<-Z$Xlower[1]
      xx[nrow(Z)]<-Z$Xupper[nrow(Z)]
    }
    datai<-data.frame(  x=xx,
                        low=Z$XL.real,
                        med=Z$XM.real,
                        upp=Z$XU.real)
    datai$strata<-dd2$strata.names[strata]
    data0<-rbind(data0,datai)

    ## here now code for using ribbon

    modeli<-data.frame(x=xx,
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

    if(median.only){
      data0<-subset(data0,group=="Median")
      model0<-subset(model0,group2=="Median")
      lty.line<-lty.line[2]
      col.segm<-col.segm[2]
    }

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

    if(control$add.caption){
      pp<-add.caption(pp,control)
    }

  }

  pp
}

