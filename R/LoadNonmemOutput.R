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
extload <- function(model,positive.iterations.only=T,last.table.only=T) {
  ## Make sure model is without .ext, remove extension if needed
  if(substr(model,nchar(model)-3,nchar(model))==".ext") {
    model=substr(model,1,nchar(model)-4)
  }

	## First check number of TABLE in the ext file
	tmp<-read.table(paste(model,".ext",sep=""),sep="?",header=F,stringsAsFactors = FALSE)
	skip.rows<-grep("TABLE",tmp[,1])
	n.rows<-c(skip.rows[-1],nrow(tmp))-skip.rows-2

	###First do last table
	i<-	length(skip.rows)
	ret<-extload.sub.table(model,skip.rows[i],n.rows[i],positive.iterations.only)

	### Then add other tables as sublists
	if(!last.table.only & length(skip.rows)>1){
		for(i in 1:(length(skip.rows)-1)){
			reti<-extload.sub.table(model,skip.rows[i],n.rows[i],positive.iterations.only)
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
#' @return
#' Named list including theta, omega, sigma, and ofv. For MCMC output file each object are matrixes.
#' @importFrom utils read.table
extload.sub.table <- function(model,skip,nrows,positive.iterations.only) {
	ext<-read.table(file=paste(model,".ext",sep=""),skip=skip,nrows=nrows,header=T)
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
			ii<-which(ext$ITERATION<0)[1]
			theta<-t(ext[ii,jtheta])[,1]
			theta.sd<-t(ext[ii+1,jtheta])[,1]

			omegaV<-t(ext[ii,jomega])[,1]
			omega.SD1<-t(ext[ii+1,jomega])[,1]
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
			sigma.SD1<-t(ext[ii+1,jsigma])[,1]
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

			ret<-list(theta=theta,theta.sd=theta.sd,omega=omega,omega.sd=omega.sd,sigma=sigma,sigma.sd=sigma.sd,ofv=ofv)

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

			ret<-list(theta=theta,omega=omega,sigma=sigma)
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
#' name of the cov file without the .cov extension. model may include full or relative path to the cov file. See examples.
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
covload <- function(model,theta.only=T){
  ## Make sure model is without .cov, remove extension if needed
  if(substr(model,nchar(model)-3,nchar(model))==".cov") {
    model=substr(model,1,nchar(model)-4)
  }
  cov<-read.table(file=paste(model,".cov",sep=""),skip=1,header=T)
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
#' name of the lst file without the .lst extension. model may include full or relative path to the lst file.
#' @return
#' named list of class sumoR
#' @export
#' @importFrom utils read.table
#' @importFrom stats cov2cor
#' @importFrom stats runif
#'
sumoR<-function(model){

  ### Remove any extension; .ext, .cov, .lst, or .mod
  if (substr(model, nchar(model) - 3, nchar(model)) %in% c(".ext",".cov",".lst",".mod")) {
    model = substr(model, 1, nchar(model) - 4)
  }

  ### read the full lst.file as character vector
  lst.file<-paste(model,".lst",sep="")
  d0<-read.table(file=lst.file,sep="?",comment.char="",stringsAsFactors=F)[,1]

  ### Get minimization successful ; Y/N
  minSuccessful<-length(grep("0MINIMIZATION SUCCESSFUL",d0))>0

  ### Check for rounding errors
  roundingErrors<-length(grep("DUE TO ROUNDING ERRORS",d0))>0

  ### Check for any zero final gradient
  k1<-max(grep(" PARAMETER:",d0))
  k2<-max(grep(" GRADIENT",d0))
  tmp<-gsub("GRADIENT:","",d0[k2:(k2+k2-k1-1)])
  tmp0<-paste(tmp,collapse=" ")
  tmp1<-gsub("^\\s+|\\s+$","",tmp0)
  tmp2<-gsub(" +"," ",tmp1)
  final.Grad<-as.numeric(strsplit(tmp2,split=" ")[[1]])

  finalZeroGradient<-sum(final.Grad==0)

  ### Check for any reset of hessian
  resetHessian<-length(grep("RESET HESSIAN",d0))

  ### Check if $COV was run
  k<-grep("0COVARIANCE STEP OMITTED:",d0)
  covRun<-d0[k]=="0COVARIANCE STEP OMITTED:        NO"

  ### Check if $COV was successful
  covSuccessful<-	length(grep("0COVARIANCE STEP ABORTED",d0))==0 &
    length(grep("STANDARD ERROR OF ESTIMATE",d0))>0

  ### Get number of significant digits in final estimates
  digitsFinalEst<-NA
  k<-grep("DIGITS IN FINAL EST",d0)
  if(length(k)>0){	digitsFinalEst<-as.numeric(substr(d0[k],35,46))	}

  ### Get number of observations
  k<-grep("TOT. NO. OF OBS RECS:",d0)
  totNoOfObservations<-as.numeric(substr(d0[k],25,46))

  ### Get number of individuals
  k<-grep("TOT. NO. OF INDIVIDUALS:",d0)
  totNoOfIndividuals<-as.numeric(substr(d0[k],26,46))

  ### Get Shrinkage
  etaShrink<-NA
  epsShrink<-NA
  k<-grep("ETAshrink(",d0,fixed=TRUE)
  if(length(k)>0 & nchar(d0[k])>20){
    tmp0<-substr(d0[k],15,300)
    tmp1<-gsub("^\\s+|\\s+$","",tmp0)
    tmp2<-gsub(" +"," ",tmp1)
    etaShrink<-as.numeric(strsplit(tmp2,split=" ")[[1]])
  }
  k<-grep("EPSshrink(",d0,fixed=TRUE)
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
  k<-grep("#OBJV:",d0)
  OFV<-as.numeric(gsub("*","",substr(d0[k],8,200),fixed=TRUE))


  res<-list(	model=model,
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
             epsShrink=epsShrink)

  ## Set the name for the class
  class(res) <- append(class(res),"sumoR")
  res

}

print.sumoR <- function(x, digits = 2,...){
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
    cat("EPS shrinkage(%) ",sprintf(" %5.1f",x$epsShrink),"\n")
}





