####################################################
#' Compile parameter table suitable for reports
#'
#' @description
#' Load parameter values from a nonmem .ext output file and compile to table suitable for reports.
#' Format can be "wide" (wide=TRUE, default) with a similar look as from sumoR,
#' or similar a 3 column layout (wide=FALSE).
#'
#' @param model
#' name of the ext file with or without the .ext extension.
#' model may include full or relative path to the ext file. See examples.
#' @param use.model.path
#' Load file from a global defined model library (TRUE=default).
#' If so will look for a global character vector named \code{model.path}
#' @param tableType
#' Table type for THETA's, OMEGA's and SIGMA's
#' tableType=0: Present OMEGA and SIGMA as variance and covariances and display SE for THETA, OMEGA, SIGMA
#' tableType=1: Present OMEGA and SIGMA as variance and covariances and display RSE for THETA, OMEGA, SIGMA
#' tableType=2: Present OMEGA and SIGMA as standard-deviation and correlations and display RSE for THETA, OMEGA, SIGMA
#' tableType=3: Present OMEGA and SIGMA as standard-deviation and correlations and display SE for THETA, OMEGA, SIGMA
#' @param wide
#' produce a wide 9-column table (wide=TRUE, default) or a thin 3-column table(wide=FALSE)
#' @param format.estimate
#' format for estimated value, passed to sprintf
#' @param format.rse
#' format for rse, passed to sprintf
#'
#' @return
#' a character-matrix
#' @export
#' @importFrom stats cov2cor
#' @importFrom stats runif
#'
#' @examples
#' ##### Load the .ext file "run001.ext"
#' # 1) Get path to the example file included in nonmem2R package
#' file1 <- system.file("extdata", "run001.ext", package = "nonmem2R")
#' # 2) Load the file using the extload function
#' extToTable(file1)
#'
extToTable <- function(model,use.model.path=TRUE,tableType=2,wide=TRUE,format.estimate="% -#6.4g",format.rse="%#6.3g") {

  ext<-extload(model,use.model.path=use.model.path)

  y1<-extTransform(ext,type=tableType)
  y<-extFormat(y1,format.estimate=format.estimate,format.rse=format.rse)

  rse.or.se<-"SE"
  if(tableType %in% (1:2)){
    rse.or.se<-"RSE"
  }
  colnames(y)<-c(
    "Theta.ID","Theta.Est",paste("Theta",rse.or.se,sep="."),
    "Omega.ID","Omega.Est",paste("Omega",rse.or.se,sep="."),
    "Sigma.ID","Sigma.Est",paste("Sigma",rse.or.se,sep=".")
  )

  if(!wide){

    y<-rbind(
      c("Thetas","",""),y[,1:3],
      c("Omegas","",""),y[,4:6],
      c("Sigmas","",""),y[,7:9])

    colnames(y)<-c("ID","Est",rse.or.se)
    ii<-y[,1]!=""
    y<-y[ii,]

  }
  y<-gsub(" ","",y)

  ## Set the name for the class
  class(y) <- append(class(y),"extToTable")

  y
}
####################################################################################################
#' Print function for compiled summary information of class extToTable
#'
#' @description
#' Print function for compiled summary information of class extToTable
#' @param x
#' a object af class extToTable.
#' @param ...
#' further arguments to be passed to or from methods.
#' @method print extToTable
#' @export
#' @keywords internal
print.extToTable <- function(x,...){
  class(x)<-"matrix"
  print(x,justify="right",quote=F)
}

####################################################################################################
#' Load final parameter values from a nonmem .ext output file.
#'
#' @description
#' Load parameter values from a nonmem .ext output file.
#' For output files from FO, FOCE and IMP only the final parameter values are loaded.
#' For output tables of MCMC and SAEM all parameter values from ITERATIONS>0 will be loaded
#' unless positive.iterations=FALSE in case all the output from all iterations are loaded.
#'
#' For ext-files with multiple table results either only the last table result is loaded (last.table.only=TRUE, default)
#' or all table result are loaded (last.table.only=FALSE).
#' All but the last table results are then returned as sub-lists to the last table result.
#'
#' @param model
#' name of the ext file with or without the .ext extension.
#' model may include full or relative path to the ext file. See examples.
#' @param use.model.path
#' Load file from a global defined model library (TRUE=default).
#' If so will look for a global character vector named \code{model.path}
#' @param positive.iterations.only
#' Include only rows with ITERATIONS>0 from MCMC and SEAM table results (default=TRUE)
#' @param last.table.only
#' Include only the last table result for ext files with multiple table results
#' @return
#' Named list including theta, theta.sd, omega, omega.sd, sigma, sigma.sd, and ofv.
#' Here .sd is the vector(matrix) with standard errors estimated parameter values in theta(omega & sigma).
#'
#' For MCMC output files each object are matrixes.
#' @export
#' @importFrom utils read.table
#' @examples
#' ##### Load the .ext file "run001.ext"
#' # 1) Get path to the example file included in nonmem2R package
#' file1 <- system.file("extdata", "run001.ext", package = "nonmem2R")
#' # 2) Load the file using the extload function
#' extload(file1)
extload <- function(model,use.model.path=TRUE,positive.iterations.only=TRUE,last.table.only=TRUE) {

  #### Check for global model.path
  file.path<-""
  model.path.ok<-FALSE
  if(use.model.path & exists("model.path")){
    eval(parse(text="model.path.ok<-dir.exists(model.path)"))
    if(model.path.ok){
      eval(parse(text="file.path<-model.path"))
    }
  }

  ### Remove any extension; .ext, .cov, .lst, or .mod
  if (substr(model, nchar(model) - 3, nchar(model)) %in% c(".ext",".cov",".lst",".mod")) {
    model = substr(model, 1, nchar(model) - 4)
  }


	## First check number of TABLE in the ext file
	tmp<-read.table(paste(file.path,model,".ext",sep=""),sep="?",header=F,stringsAsFactors = FALSE)
	skip.rows<-grep("TABLE",tmp[,1])
	n.rows<-c(skip.rows[-1],nrow(tmp))-skip.rows-2

	###First do last table
	i<-	length(skip.rows)
	ret<-extload.sub.table(model,skip.rows[i],n.rows[i],positive.iterations.only,use.model.path=use.model.path)

	### Then add other tables as sublists
	if(!last.table.only & length(skip.rows)>1){
		for(i in 1:(length(skip.rows)-1)){
			reti<-extload.sub.table(model,skip.rows[i],n.rows[i],positive.iterations.only,use.model.path=use.model.path)
			eval(parse(text=paste("ret$table",i,"<-reti",sep="")))
		}
	}
	ret
}


####################################################################################################
#' Internal package function
#'
#' @param model
#' model
#' @param skip
#' number of rows to skip
#' @param nrows
#' number of rows to read
#' @param positive.iterations.only
#' positive iterations only
#' @param use.model.path
#' Load file from a global defined model library (TRUE=default).
#' If so will look for a global character vector named \code{model.path}
#' @return
#' Named list including theta, omega, sigma, and ofv. For MCMC output file each object are matrixes.
#' @export
#' @keywords internal
#' @importFrom utils read.table
extload.sub.table <- function(model,skip,nrows,positive.iterations.only,use.model.path=TRUE) {

  #### Check for global model.path
  file.path<-""
  model.path.ok<-FALSE
  if(use.model.path & exists("model.path")){
    eval(parse(text="model.path.ok<-dir.exists(model.path)"))
    if(model.path.ok){
      eval(parse(text="file.path<-model.path"))
    }
  }

	ext<-read.table(file=paste(file.path,model,".ext",sep=""),skip=skip,nrows=nrows,header=T)
	### Check for type based on name of last column
	last.col<-colnames(ext)[ncol(ext)]
	type<-match(last.col,c("OBJ","SAEMOBJ","MCMCOBJ"))
	if(is.na(type)){
			cat("Unknown nonmem table type:",last.col,"\n")
			cat("Returning empty list\n")
			ret<-list(theta=NULL,omega=NULL,sigma=NULL)
	}
	else{
		if(type==1){  ## OBJ type, return first row with ITERATION<0
			jtheta<-grep("THETA",colnames(ext))
			jomega<-grep("OMEGA",colnames(ext))
			jsigma<-grep("SIGMA",colnames(ext))
			ii<-which(ext$ITERATION==  -1000000000)
			iii<-which(ext$ITERATION== -1000000001)
			theta<-t(ext[ii,jtheta])[,1]
			theta.sd<-rep(NA,length=length(theta))
			if(any(iii)){
			  theta.sd<-t(ext[iii,jtheta])[,1]
			}
      fixed<-colnames(ext)[which(apply(ext[ext$ITERATION>0,],2,sd)==0)]

			omegaV<-t(ext[ii,jomega])[,1]
			omega.SD1<-rep(NA,length=length(omegaV))
			if(any(iii)){
			  omega.SD1<-t(ext[iii,jomega])[,1]
			}
			k<-length(omegaV)
			n<-sqrt(2*k+1/4)-1/2
			omega<-matrix(0,n,n)
			omega[upper.tri(omega,diag=TRUE)]<-omegaV
			omega[lower.tri(omega,diag=TRUE)]<-t(omega)[lower.tri(omega,diag=TRUE)]
			colnames(omega)<-rownames(omega)<-paste("OMEGA",1:n,sep="")
			omega.sd<-omega
			omega.sd[upper.tri(omega.sd,diag=TRUE)]<-omega.SD1
			omega.sd[lower.tri(omega.sd,diag=TRUE)]<-t(omega.sd)[lower.tri(omega.sd,diag=TRUE)]


			sigmaV<-t(ext[ii,jsigma])[,1]
			sigma.SD1<-rep(NA,length=length(sigmaV))
			if(any(iii)){
			  sigma.SD1<-t(ext[iii,jsigma])[,1]
			}
			k<-length(sigmaV)
			n<-sqrt(2*k+1/4)-1/2
			sigma<-matrix(0,n,n)
			sigma[upper.tri(sigma,diag=TRUE)]<-sigmaV
			sigma[lower.tri(sigma,diag=TRUE)]<-t(sigma)[lower.tri(sigma,diag=TRUE)]
			colnames(sigma)<-rownames(sigma)<-paste("sigma",1:n,sep="")
			sigma.sd<-sigma
			sigma.sd[upper.tri(sigma.sd,diag=TRUE)]<-sigma.SD1
			sigma.sd[lower.tri(sigma.sd,diag=TRUE)]<-t(sigma.sd)[lower.tri(sigma.sd,diag=TRUE)]

			ofv=t(ext[ii,ncol(ext)])[,1]

			ret<-list(theta=theta,theta.sd=theta.sd,omega=omega,omega.sd=omega.sd,sigma=sigma,sigma.sd=sigma.sd,ofv=ofv,fix=fixed)

		}
		else{
			jtheta<-c(1,grep("THETA",colnames(ext)))
			jomega<-c(1,grep("OMEGA",colnames(ext)))
			jsigma<-c(1,grep("SIGMA",colnames(ext)))
			ii<-1:nrow(ext)
			if(positive.iterations.only){
				ii<-which(ext$ITERATION>0)
			}
			theta<-ext[ii,jtheta]

			omega<-ext[ii,jomega]

			sigma<-ext[ii,jsigma]

			fixed<-colnames(ext)[which(apply(ext[ii,],2,sd)==0)]

			ret<-list(theta=theta,omega=omega,sigma=sigma,fix=fixed)
		}
	}
	ret
}



####################################################################################################
#' Load the covariance matrix from a nonmem .cov output file.
#'
#' @description
#' Load the covariance matrix from a nonmem .cov output file.
#' Either the covariance matrix of all THETA parameters (default) or the covariance matrix of all parameters, THETA, OMEGA and SIGMA.
#'
#'
#' @param model
#' name of the cov file with or without the .cov extension. model may include full or relative path to the cov file. See examples.
#' @param use.model.path
#' Load file from a global defined model library (TRUE=default).
#' If so will look for a global character vector named \code{model.path}
#' @param theta.only
#' return covariance matrix of theta's only (default)
#' @return
#' the covariance matrix
#' @export
#' @importFrom utils read.table
#'
#' @examples
#' ##### Load the .cov file "run001.cov"
#' # 1) Get path to the example file included in nonmem2R package
#' file1 <- system.file("extdata", "run001.cov", package = "nonmem2R")
#' # 2) Load the file using the covload function
#' covload(file1)
covload <- function(model,use.model.path=TRUE,theta.only=T){

  #### Check for global model.path
  file.path<-""
  model.path.ok<-FALSE
  if(exists("model.path")){
    eval(parse(text="model.path.ok<-dir.exists(model.path)"))
    if(model.path.ok){
      eval(parse(text="file.path<-model.path"))
    }
  }

  ### Remove any extension; .ext, .cov, .lst, or .mod
  if (substr(model, nchar(model) - 3, nchar(model)) %in% c(".ext",".cov",".lst",".mod")) {
    model = substr(model, 1, nchar(model) - 4)
  }

  cov<-read.table(file=paste(file.path,model,".cov",sep=""),skip=1,header=T)
  if(theta.only){
    ii <- grep("THETA", cov$NAME)
    jj <- grep("THETA", colnames(cov))
  }
  else{
    ii <- c(grep("THETA", cov$NAME),grep("OMEGA", cov$NAME),grep("SIGMA", cov$NAME))
    jj <- c(grep("THETA", colnames(cov)),grep("OMEGA", colnames(cov)),grep("SIGMA", colnames(cov)))
  }

	theta.cov<-t(cov[ii,jj])
	theta.cov
}

####################################################################################################
#' Compile summary information of for NONMEM model based on the lst file, ext file, and the cov file.
#'
#' @description
#' Compile summary information similar to that of the sumo PSN function, based on the NONMEM output files
#' lst, ext, and if covariance setp was run, the cov file.
#'
#' @param model
#' name of the lst file with or without the .lst extension. model may include full or relative path to the lst file.
#' @param use.model.path
#' Load file from a global defined model library (TRUE=default).
#' If so will look for a global character vector named \code{model.path}
#' @param tableType
#' Table type for THETA's, OMEGA's and SIGMA's \cr
#' tableType=0: Present OMEGA and SIGMA as variance and covariances and display SE for THETA, OMEGA, SIGMA \cr
#' tableType=1: Present OMEGA and SIGMA as variance and covariances and display RSE for THETA, OMEGA, SIGMA \cr
#' tableType=2: Present OMEGA and SIGMA as standard-deviation and correlations and display RSE for THETA, OMEGA, SIGMA \cr
#' tableType=3: Present OMEGA and SIGMA as standard-deviation and correlations and display SE for THETA, OMEGA, SIGMA \cr
#' @param format.estimate
#' format for estimated value, passed to sprintf
#' @param format.rse
#' format for RSE or SE, passed to sprintf
#' @return
#' named list of class sumoR
#' @export
#' @importFrom utils read.table
#' @importFrom stats cov2cor
#' @importFrom stats runif
#'
#' @examples
#' ##### Compile summary information from the .lst file "run001.lst"
#' # 1) Get path to the example file included in nonmem2R package
#' file1 <- system.file("extdata", "run001.lst", package = "nonmem2R")
#' # 2) Compile summary information from "run001.lst"
#' sumoR(file1)
sumoR<-function(model,use.model.path=TRUE,tableType=2,format.estimate="% -#6.4g",format.rse="%#6.3g"){

  file.path<-""
  model.path.ok<-FALSE
  if(exists("model.path")){
    eval(parse(text="model.path.ok<-dir.exists(model.path)"))
    if(model.path.ok){
      eval(parse(text="file.path<-model.path"))
    }
  }

  ### Remove any extension; .ext, .cov, .lst, or .mod
  if (substr(model, nchar(model) - 3, nchar(model)) %in% c(".ext",".cov",".lst",".mod")) {
    model = substr(model, 1, nchar(model) - 4)
  }

  ### creat full path to model lst file
  lst.file<-paste(file.path,model,".lst",sep="")

  if (grepl("seml.+\\.astrazeneca\\.net", Sys.info()["nodename"])) {
    res<-NULL
    sumoRU(lst.file)
  }
  else{

    ### read the full lst.file as character vector
    d0<-read.table(file=lst.file,sep="?",comment.char="",stringsAsFactors=F)[,1]

    ### Get minimization successful ; Y/N
    minSuccessful<-suppressWarnings(length(grep("0MINIMIZATION SUCCESSFUL",d0))>0)

    ### Check for rounding errors
    roundingErrors<-suppressWarnings(length(grep("DUE TO ROUNDING ERRORS",d0))>0)

    ### Check for any zero final gradient
    k1<-suppressWarnings(max(grep(" PARAMETER:",d0)))
    k2<-suppressWarnings(max(grep(" GRADIENT",d0)))
    tmp<-gsub("GRADIENT:","",d0[k2:(k2+k2-k1-1)])
    tmp0<-paste(tmp,collapse=" ")
    tmp1<-gsub("^\\s+|\\s+$","",tmp0)
    tmp2<-gsub(" +"," ",tmp1)
    final.Grad<-as.numeric(strsplit(tmp2,split=" ")[[1]])

    finalZeroGradient<-sum(final.Grad==0)

    ### Check for any reset of hessian
    resetHessian<-suppressWarnings(length(grep("RESET HESSIAN",d0)))

    ### Check if $COV was run
    k<-suppressWarnings(grep("0COVARIANCE STEP OMITTED:",d0))
    covRun<-gsub(" +"," ",d0[k])=="0COVARIANCE STEP OMITTED: NO"

    ### Check if $COV was successful
    covSuccessful<-	suppressWarnings(length(grep("0COVARIANCE STEP ABORTED",d0))==0) &
      suppressWarnings(length(grep("STANDARD ERROR OF ESTIMATE",d0))>0)

    ### Get number of significant digits in final estimates
    digitsFinalEst<-NA
    k<-suppressWarnings(grep("DIGITS IN FINAL EST",d0))
    if(length(k)>0){	digitsFinalEst<-as.numeric(substr(d0[k],35,46))	}

    ### Get number of observations
    k<-suppressWarnings(grep("TOT. NO. OF OBS RECS:",d0))
    totNoOfObservations<-as.numeric(substr(d0[k],25,46))

    ### Get number of individuals
    k<-suppressWarnings(grep("TOT. NO. OF INDIVIDUALS:",d0))
    totNoOfIndividuals<-as.numeric(substr(d0[k],26,46))

    ### Get Shrinkage
    etaShrink<-NA
    epsShrink<-NA
    k<-suppressWarnings(grep("ETAshrink(",d0,fixed=TRUE))
    if(length(k)>0 & nchar(d0[k])>20){
      tmp0<-substr(d0[k],15,300)
      tmp1<-gsub("^\\s+|\\s+$","",tmp0)
      tmp2<-gsub(" +"," ",tmp1)
      etaShrink<-as.numeric(strsplit(tmp2,split=" ")[[1]])
    }
    k<-suppressWarnings(grep("EPSshrink(",d0,fixed=TRUE))
    if(length(k)>0 & nchar(d0[k])>20){
      tmp0<-substr(d0[k],15,300)
      tmp1<-gsub("^\\s+|\\s+$","",tmp0)
      tmp2<-gsub(" +"," ",tmp1)
      epsShrink<-as.numeric(strsplit(tmp2,split=" ")[[1]])
    }

    ### Load .ext file
    Ext<-extload(model)

    ### Load .cov file
    Cov<-diag(0,length(Ext$theta))
    conditionNumber<-NA
    if(covSuccessful){
      Cov<-covload(model,theta.only=FALSE)
      ii<-diag(Cov)>0
      cor2<-cov2cor(Cov[ii,ii])
      eigenvals<-eigen(cor2,TRUE, only.values = TRUE)$values
      conditionNumber<-(max(eigenvals)/min(eigenvals))
    }

    ### Get OFV
    k<-suppressWarnings(grep("#OBJV:",d0))
    OFV<-as.numeric(gsub("*","",substr(d0[k],8,200),fixed=TRUE))

    y1<-extTransform(Ext,type=tableType)
    coef<-extFormat(y1,format.estimate=format.estimate,format.rse=format.rse)



    res<-list(	model=paste(file.path,model,sep=""),
               minSuccessful=minSuccessful,
               roundingErrors=roundingErrors,
               finalZeroGradient=finalZeroGradient,
               resetHessian=resetHessian,
               covRun=covRun & minSuccessful,
               covSuccessful=covSuccessful,
               digitsFinalEst=digitsFinalEst,
               totNoOfObservations=totNoOfObservations,
               totNoOfIndividuals=totNoOfIndividuals,
               OFV=OFV,
               conditionNumber=conditionNumber,
               etaShrink=etaShrink,
               epsShrink=epsShrink,
               Ext=Ext,
               Cov=Cov,
               coef=coef)

    ## Set the name for the class
    class(res) <- append(class(res),"sumoR")
  }
  res
}

####################################################################################################
#' Print function for compiled summary information of class sumoR
#'
#' @description
#' Print function for compiled summary information of class sumoR
#' @param x
#' a object af class sumoR.
#' @param ...
#' further arguments to be passed to or from methods.
#' @method print sumoR
#' @export
#' @keywords internal
print.sumoR <- function(x,...){
    if(length(x) == 1){
      cat("NA\n")
      return()
    }
    cat("\nNONMEM output summary: ",as.character(x$model),"\n\n")
    #Successful minimization            [    OK   ]
    cases<-c("Successful minimization                   [    OK   ]",
             "Termination problems                      [  ERROR  ]")
    cat(ifelse(x$minSuccessful,cases[1],cases[2]),"\n")

    #roundingErrors
    cases<-c("No rounding errors                        [    OK   ]",
             "Rounding errors                           [  ERROR  ]")
    cat(ifelse(!x$roundingErrors,cases[1],cases[2]),"\n")

    #No final zero gradients            [    OK   ]
    cases<-c("No final zero gradients                   [    OK   ]",
             paste(x$finalZeroGradient,"parameter(s) with final zero gradient   [ WARNING ]"))
    cat(ifelse(x$finalZeroGradient==0,cases[1],cases[2]),"\n")
    #Hessian not reset                  [    OK   ]
    cases<-c("Hessian not reset                         [    OK   ]",
             paste("Hessian reset",x$resetHessian,"times                     [ WARNING ]"))
    cat(ifelse(x$resetHessian==0,cases[1],cases[2]),"\n")

    #Successful covariance step         [    OK   ]
    cases<-c("Covariance step not run                   [    --   ]",
             "Successful covariance step                [    OK   ]",
             "Covariance step problems                  [  ERROR  ]")
    cat(ifelse(!x$covRun,cases[1],ifelse(x$covSuccessful,cases[2],cases[3])),"\n\n")

    #Number of observation records:          3150
    cat("Number of observation records         ",sprintf("%10.0f",x$totNoOfObservations),"\n")

    #Number of individuals:                  567
    cat("Number of individuals                 ",sprintf("%10.0f",x$totNoOfIndividuals),"\n\n")

    #Objective function value:          21644.4840
    cat("Objective function value              ",sprintf("%10.2f",x$OFV),"\n")

    #Number sign. digits in estimates:      4  "
    cat("Number significant digits in estimates ",sprintf("%9.2f",x$digitsFinalEst),"\n")

    #Condition number:                    101.1
    cat("Condition number                    ",sprintf("%12.2f",x$conditionNumber),"\n\n")

    #Shrinkage, ETA and EPS
    cat("ETA shrinkage(%) ",sprintf(" %5.1f",x$etaShrink),"\n")
    cat("EPS shrinkage(%) ",sprintf(" %5.1f",x$epsShrink),"\n\n\n")

    print(x$coef,justify="right",quote=F)

}

####################################################
#' Internal package function
#'
#' @description
#' Formatting of the named list returned by extload
#' @param ext
#' the result ofname of the lst file without the .lst extension. model may include full or relative path to the lst file.
#' @param format.estimate
#' format for estimated value, passed to sprintf
#' @param format.rse
#' format for rse, passed to sprintf
#'
#' @return
#' a character-matrix
#' @export
#' @keywords internal
#' @importFrom stats cov2cor
extFormat <- function(ext,format.estimate="% -#6.4g",format.rse="%#6.3g") {

  ####
  to.vec<-function(x,prefix="rc"){
    kk<-lower.tri(x,diag=T)
    rr<-matrix(rep(1:ncol(x),ncol(x)),ncol=ncol(x))[kk]
    cc<-matrix(rep(1:ncol(x),each=ncol(x)),ncol=ncol(x))[kk]
    xV<-x[kk]
    names(xV)<-paste(prefix,rr,cc,"",sep=".")
    xV
  }

  ##
  ext$omega<-to.vec(ext$omega,prefix="OMEGA")
  ext$omega.sd<-to.vec(ext$omega.sd,prefix="OMEGA")

  ext$sigma<-to.vec(ext$sigma,prefix="SIGMA")
  ext$sigma.sd<-to.vec(ext$sigma.sd,prefix="SIGMA")


  thetaii<-!(names(ext$theta) %in% ext$fix)
  omegaii<-!(names(ext$omega) %in% ext$fix)
  sigmaii<-!(names(ext$sigma) %in% ext$fix)

  ### Max number of parameters ( by theta,omega, sigma)
  np<-max(c(sum(thetaii),sum(omegaii),sum(sigmaii)))

  ### Init res.table tripple (no,est,cv) for theta, omega, sigma
  res.table<-matrix(NA,nrow=np,ncol=9)

  ## Theta's
  if(sum(thetaii)>0){
    res.table[1:sum(thetaii),1]<-which(thetaii)
    res.table[1:sum(thetaii),2]<-ext$theta[thetaii]
    res.table[1:sum(thetaii),3]<-ext$theta.sd[thetaii]
  }

  ## Omegas's
  tmp<-gsub("OMEGA.","",names(ext$omega)[omegaii])
  omega.index<-substr(tmp,1,nchar(tmp)-1)
  if(length(tmp)>0){
    res.table[1:sum(omegaii),4]<-as.numeric(omega.index)
    res.table[1:sum(omegaii),5]<-ext$omega[omegaii]
    res.table[1:sum(omegaii),6]<-ext$omega.sd[omegaii]
  }


  ## Sigma's
  tmp<-gsub("SIGMA.","",names(ext$sigma)[sigmaii])
  sigma.index<-substr(tmp,1,nchar(tmp)-1)
  if(length(tmp)>0){
    res.table[1:sum(sigmaii),7]<-as.numeric(sigma.index)
    res.table[1:sum(sigmaii),8]<-ext$sigma[sigmaii]
    res.table[1:sum(sigmaii),9]<-ext$sigma.sd[sigmaii]
  }

  y<-matrix("",nrow(res.table),ncol(res.table))
  colnames(y)<-c("","  Theta","","","  Omega","","","Sigma","")
  rownames(y)<-rep("",nrow(y))

  for(i in 1:nrow(res.table)){
    for(j in c(2,3,5,6,8,9)){
      if(!is.na(res.table[i,j])){
        if(j %in% c(3,6,9)){
          y[i,j]<-sprintf(format.rse,res.table[i,j])
          y[i,j]<-paste("(",y[i,j],")",sep="")
        }
        else{
          y[i,j]<-sprintf(format.estimate,res.table[i,j])
        }
      }
    }
    y[i,1]<-sprintf("%3d",res.table[i,1])
  }

  ### rse/SD is na, fill with (...) where needed
  ii<-!is.na(res.table[,2]) & is.na(res.table[,3])
  y[ii,3]<-"(...)"

  ii<-!is.na(res.table[,5]) & is.na(res.table[,6])
  y[ii,6]<-"(...)"

  ii<-!is.na(res.table[,8]) & is.na(res.table[,9])
  y[ii,6]<-"(...)"

  if(length(omega.index)>0){
    ii<-1:length(omega.index)
    y[ii,4]<-gsub(".",",",paste("  [",omega.index,"]",sep=""),fixed=TRUE)
  }
  if(length(sigma.index)>0){
    ii<-1:length(sigma.index)
    y[ii,7]<-gsub(".",",",paste("  [",sigma.index,"]",sep=""),fixed=TRUE)
  }

  y
}

####################################################
#' Internal package function
#'
#' @description
#' Transforming of the named list returned by extload
#' @param ext
#' named list according to output from extload
#' @param type
#' Type=0: Present OMEGA and SIGMA as variance and covariances and display SE for THETA, OMEGA, SIGMA
#' Type=1: Present OMEGA and SIGMA as variance and covariances and display RSE for THETA, OMEGA, SIGMA
#' Type=2: Present OMEGA and SIGMA as standard-deviation and correlations and display RSE for THETA, OMEGA, SIGMA
#' Type=3: Present OMEGA and SIGMA as standard-deviation and correlations and display SE for THETA, OMEGA, SIGMA
#' @return
#' named list according to output from extload
#' @export
#' @keywords internal
extTransform<-function(ext,type=2){

  if(!("theta.sd" %in% names(ext))){
    stop("Loaded ext files generated by estimation methods SAEM or MCMC not supported.")
  }

  ###############################
  ### Compute RSE for THETA's for type 1 and 2
  if(type %in% 1:2){
    ext$theta.sd<-abs(ext$theta.sd/ext$theta)
  }

  ###############################
  ### Compute RSE for OMEGA's as variance (and SIGMA's) for type==1
  #(SE-of-variance/variance estimate)
  if(type==1){
    ext$omega.sd<-ext$omega.sd/abs(ext$omega)
    ext$sigma.sd<-ext$sigma.sd/abs(ext$sigma)
  }

  ###############################
  ### Type 2 and 3
  if(type>1){
    ### Compute RSE for OMEGA's as std (and SIGMA's)
    #(SE/variance estimate)/2
    ext$omega.sd<-ext$omega.sd/abs(ext$omega)/2
    ext$sigma.sd<-ext$sigma.sd/abs(ext$sigma)/2


    ###############################
    ### Transform diagonal of OMEGA and Sigma to STD ( variance -> SD)
    omega.diag<-sqrt(diag(ext$omega))
    sigma.diag<-sqrt(diag(ext$sigma))

    ### Transform off-diagonal to correlations, omega and sigma
    tmp<-ext$omega;ii<-diag(tmp)==0;diag(tmp)[ii]<-1;omega<-cov2cor(tmp);
    tmp<-ext$sigma;ii<-diag(tmp)==0;diag(tmp)[ii]<-1;sigma<-cov2cor(tmp);

    ### set diaginal to SD, omega and sigma
    diag(omega)<-omega.diag
    diag(sigma)<-sigma.diag

    ext$omega<-omega
    ext$sigma<-sigma
  }

  ###############################
  ### Type 3
  if(type>2){
    ### Compute SD for OMEGA's as std ( and SIGMA's) from RSE for OMEGA's as std
    ### SD=RSE*mean
    ext$omega.sd<-ext$omega.sd*abs(ext$omega)
  }
  ext
}

####################################################
#' Internal package function
#'
#' @description
#' Internal function for sumoR on unix/linux system
#' @param file.path
#' path including file name to model lst file
#' @return
#' named list
#' @export
#' @keywords internal
sumoRU<-function(file.path){
  system(paste("ssh -q calvin.seml.astrazeneca.net \"cd $(pwd); module unload psn && module load psn && sumo",file.path, "\""))
}


####################################################
#' Run qpsn system call
#'
#' @description
#' Run qpsn system call on system where psn is available.
#' If psn is not available system{base} is used.
#'
#' @param cmd
#' qpsn cmd
#' @param use.model.path
#' Run cmd in a specified model library (TRUE=default).
#' If so will look for a global character vector named \code{model.path} and
#' run the system cmd the folder path as specified in \code{model.path}.
#' @param ...
#' Further arguments
# @return
# named list
#' @export
#' @examples
#' # (if the platform has who)
#' try(systemPSN("who", intern = TRUE))
#'
#' try(systemPSN("ls fizzlipuzzli", intern = TRUE, ignore.stderr = TRUE))
#' # zero-length result since file does not exist, and will give warning.
systemPSN<-function (cmd,use.model.path=TRUE, ...)
{
  cmd1<-"ssh -q calvin.seml.astrazeneca.net \"cd $(pwd); module unload psn r && module load psn r && "

  cmd2<-""
  file.path <- ""
  model.path.ok <- FALSE
  if (use.model.path & exists("model.path")) {
    eval(parse(text = "model.path.ok<-dir.exists(model.path)"))
    if (model.path.ok) {
      eval(parse(text = "file.path<-model.path"))
      cmd2<-paste(" cd ",file.path," &&")
    }
  }


  if (grepl("seml.+\\.astrazeneca\\.net", Sys.info()["nodename"])) {
    print(paste0(cmd1, cmd2, cmd, "\""))
    value <- system(paste0(cmd1, cmd2, cmd, "\""), ...)
    if(!(0%in% value)){
      stop("system_nm2 could not execute your command\n")
      value
    }
    invisible(value)
  }
  else {
    system(cmd, ...)
  }
}

