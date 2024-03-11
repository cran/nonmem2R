#' Visual Predictive Check (VPC) based on Perl-speaks-NONMEM (PsN) generated VPC files (lattice version).
#' @description
#' This function creates VPC using output files from the vpc command in Pearl Speaks NONMEM (PsN).
#' Graphs are generated using the lattice package with many arguments for plot settings are passed
#' directly to the xyplot function and have the same flexibility as when used in xyplot.
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
#' label for x-axis, default is IDV as found in vpcresultfile, passed to xyplot.
#' @param ylab
#' label for y-axis, default is DV as found in vpcresultfile, passed to xyplot.
#' @param fy
#' transformation function for y-axis, default to identity function (f(y)=y).
#' @param fx
#' transformation function for x-axis, default to identity function (f(x)=x)
#' @param col.data
#' color for data points, passed to xyplot, default=8
#' @param cex.data
#' cex for data points, passed to xyplot, default=0.3
#' @param pch.data
#' plot symbol (pch) for data points, passed to xyplot, default=3
#' @param col.line
#' line color for low quartile, median, and high quartile of data, passed to xyplot, default=c("blue","red","blue")
#' @param lwd.line
#' line width (lwd) for low quartile, median, and high quartile of data, passed to xyplot, default=c(1,2,1)
#' @param lty.line
#' line type (lty) for low quartile, median, and high quartile of data, passed to xyplot, default=c(1,1,1)
#' @param col.segm
#' Color of CI regions (low, median and high) for model, passed to xyplot, default=c("lightblue","pink","lightblue")
#' @param alpha.segm
#' alpha of CI regions (low, median and high) for model, passed to xyplot, default=c(0.5,0.5,0.5)
#' @param type
#' type of VPC plot, 1=display model regions only, 2=as 1 + lines (low, median and high) of data,
#' 3= as 2 + points for data.
#' type=0 can also be used and then no graph is produced but instead a list with 2 dataframes is returned.
#' one for the vpcresult and on for the observed data in the vpctab file.
#' @param ...
#' Further arguments, passed to xyplot, e.g xlim and ylim axis limits, main for title of plot, abline for adding reference lines, or
#' scales for formatting axes, see help files for xyplot.
#' Note: col, cex, pch, lty, lwd, and alpha should not be used.
#' Instead use the corresponding arguments defined above.
#' @return
#' lattice object of VPC plot
#' @details
#' The lattice package is used for creating the VPC and vpcfig have functionality for e.g. modify names of strata,
#' change layout of panels, use log-scale.
#'
#' The data used is either specified by the directory of the PsN generated files,
#' or by specifying the file names of both the vpctab-file and the vpcresult file.
#' See examples 1 and 2 below.
#'
#' Names of strata can be changed with the strata.names argument, if strata.names
#' is NULL the names as specified in the vpcresult file are used.
#'
#' strata.subset can be used to select a subset of strata, and or to change the order of stratas. Se example 3.
#' NOTE: strata.subset is matched with strata.names unless strata.names=NULL.
#' If strata.names=NULL then strata.subset is matched to names as specified in vpcresult file.
#' Default is to use all strata's (strata.subset=NULL).
#'
#' The fy and fx arguments can be used to alter the scale of data plotted.
#' For example, using fy=function(y)(log(y+1))
#' is a convenient way to get log-scale for y-axis but with an off-set to show any values==0. Proper y tick marks
#' can then be set by using the xyplot argument scales.
#'
#'
#'
#' @export
#' @importFrom lattice panel.polygon
#' @importFrom latticeExtra as.layer
#' @examples
#'
#' # Get path to the example files included in nonmem2R package
#' file1 <- system.file("extdata", "vpctab004.dat", package = "nonmem2R")
#' file2 <- system.file("extdata", "vpc_results.csv", package = "nonmem2R")
#'
#'
#' # Ex 1, produce VPC with default setting, here specifying both vpctab and vpcresult
#' vpcfig(vpctab=file1,vpcresult=file2)
#'
#' \dontrun{
#' # Ex 2, produce VPC with default setting, here specifying only directory of vpc files
#' path1<-gsub("vpctab004.dat","",file1)
#' vpcfig(vpcdir=path1)
#'}
#'
#' # Ex 3, produce VPC with i) modifies strata names, ii) strata in reverse order, and
#' #        iii) labels
#' strata.names<-c("Group A","Group B")
#' xlab<-"Time after dose (hrs)"
#' ylab<-"Plasma Conc(mmol/L)"
#' vpcfig(vpctab=file1,vpcresult=file2,strata.names=strata.names,strata.subset=2:1,
#'        xlab=xlab,ylab=ylab)

#################

vpcfig<-function(vpcdir=NULL,
                 vpctab=NULL,
                 vpcresult=NULL,
                 use.model.path=TRUE,
                 strata.names=NULL,
                 strata.subset=NULL,
                 percentile=10,
                 fy=function(y){y},fx=function(x){x},
                 xlab=NULL,
                 ylab=NULL,
                 col.data=8,cex.data=0.3,pch.data=3,
                 col.line=c("blue","red","blue"),
                 lwd.line=c(1,2,1),
                 lty.line=c(1,1,1),
                 col.segm=c("lightblue","pink","lightblue"),
                 alpha.segm=c(0.5,0.5,0.5),
                 type=3,...)
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

  #dd2<-read.npc.vpc.results(vpc.results = vpcresult) # Expose func
  dd2<-readVpc(vpc.results = vpcresult)              # Alt1
  #dd2<-loadVPC(vpcresult)                             # Alt2

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


    datai<-data.frame(	x=(Z$Xupper+Z$Xlower)/2,
                       low=Z$XL.real,
                       med=Z$XM.real,
                       upp=Z$XU.real)
    datai$strata<-dd2$strata.names[strata]
    data0<-rbind(data0,datai)

    ### Add the last and first row once again with endpints on x-axis
    #Z<-Z[c(1,1:nrow(Z),nrow(Z)),]
    #Z$Xupper[1]<-Z$Xlower[1]
    #Z$Xlower[nrow(Z)]<-Z$Xupper[nrow(Z)]
    x1<-(Z$Xupper+Z$Xlower)/2
    x1<-c(x1,rev(x1))
    modeli<-data.frame(x=x1,
                       low=c(	Z$X.L.from,
                              rev(Z$X.L.to)),
                       med=c(	Z$X.M.from,
                              rev(Z$X.M.to)),
                       upp=c(	Z$X.U.from,
                              rev(Z$X.U.to))
    )

    modeli$strata<-dd2$strata.names[strata]
    model0<-rbind(model0,modeli)

  }

  data0$strata<-factor(data0$strata,levels=strata.subset)
  model0$strata<-factor(model0$strata,levels=strata.subset)


  ############### Case of stratified VPC (n.strata>1)
  if(n.strata>1 & length(strata.subset)>1){
    ### Plot median,low & upp of data
    p1<-xyplot(fy(low)+fy(med)+fy(upp)~fx(x)|strata,
             #abline=abline,
             ## Here control the collor,lty and lwd for the data
             lty=lty.line,
             lwd=lwd.line,
             col=col.line,
             type='l',
             as.table=T,
             xlab=xlab,
             ylab=ylab,
             data=data0,...
    )


    p2<-xyplot(fy(med)+fy(low)+fy(upp)~fx(x)|strata,
             #abline=abline,
             panel=function(x,y,...){
               n<-length(x)/3
               ## The low region
               i1<-(1:n)+n
               color=col.segm[1]
               alpha=alpha.segm[1]
               panel.polygon(x[i1],y[i1],col=color,border=color,alpha=alpha)
               ## The upper region
               i1<-(1:n)+n*2
               color=col.segm[3]
               alpha=alpha.segm[3]
               panel.polygon(x[i1],y[i1],col=color,border=color,alpha=alpha)
               ## Here control the color of regions
               ## The median region
               i1<-1:n
               color=col.segm[2]
               alpha=alpha.segm[2]
               panel.polygon(x[i1],y[i1],col=color,border=color,alpha=alpha)
             },
             as.table=T,
             xlab=xlab,
             ylab=ylab,
             data=model0,...
    )


    p3<-xyplot(fy(DV)~fx(TIME)|strata,as.table=T,data=dd1,col=col.data,cex=cex.data,pch=pch.data,...)
  }

  ############### Case of not stratified VPC (n.strata==1), or length(strata.subset)==1
  else{
    ### Plot median,low & upp of data
    p1<-xyplot(fy(low)+fy(med)+fy(upp)~fx(x),
               #abline=abline,
               ## Here control the collor,lty and lwd for the data
               lty=lty.line,
               lwd=lwd.line,
               col=col.line,
               type='l',
               as.table=T,
               xlab=xlab,
               ylab=ylab,
               data=data0,...
    )


    p2<-xyplot(fy(med)+fy(low)+fy(upp)~fx(x),
               #abline=abline,
               panel=function(x,y,...){
                 n<-length(x)/3
                 ## The low region
                 i1<-(1:n)+n
                 color=col.segm[1]
                 alpha=alpha.segm[1]
                 panel.polygon(x[i1],y[i1],col=color,border=color,alpha=alpha)
                 ## The upper region
                 i1<-(1:n)+n*2
                 color=col.segm[3]
                 alpha=alpha.segm[3]
                 panel.polygon(x[i1],y[i1],col=color,border=color,alpha=alpha)
                 ## Here control the color of regions
                 ## The median region
                 i1<-1:n
                 color=col.segm[2]
                 alpha=alpha.segm[2]
                 panel.polygon(x[i1],y[i1],col=color,border=color,alpha=alpha)
               },
               as.table=T,
               xlab=xlab,
               ylab=ylab,
               data=model0,...
    )


    p3<-xyplot(fy(DV)~fx(TIME),as.table=T,data=dd1,col=col.data,cex=cex.data,pch=pch.data,...)
  }


  if(type==0){
    pp<-list(model=model0,data=dd1)
  }
  if(type==1){
    pp<-p2
  }
  if(type==2){
    pp<-p2+as.layer(p1)
  }
  if(type==3){
    pp<-p2+ as.layer(p3)+as.layer(p1)
  }

  pp
}

