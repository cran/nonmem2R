nonmem2R.globals <- new.env()

nonmem2R.globals$params<-list(col.data = "gray20",
                         cex.data = 1.5,
                         pch.data = 19,
                         alpha.data = 0.5,
                         #
                         col.smooth = "#3366FF",
                         lty.smooth = 1,
                         lwd.smooth = 1,
                         se.smooth=TRUE,
                         span.smooth=2/3,
                         degree.smooth=1,
                         family.smooth="symmetric",
                         #
                         col.refline = "red",
                         lty.refline = 2,
                         lwd.refline = 1,
                         #
                         fill.hist = "gray30",
                         col.hist = "transparent",
                         alpha.hist = 0.5,
                         #
                         fill.box = "steelblue",
                         col.box = "black",
                         alpha.box = 0.6,
                         #
                         axis.labels=TRUE,
                         #
                         add.caption=TRUE,
                         size.caption = 8,
                         col.caption=1,
                         caption.path.depth=99,
                         get.caption=function(){
                           res<-""
                           script.name<-get.GOF.params()$script.name
                           if(script.name!=""){
                             res<-rev(strsplit(getwd(),split="/")[[1]])
                             pre<-"../"
                             n<-get.GOF.params()$caption.path.depth
                             if(n>length(res)){
                               n<-length(res)
                               pre<-""
                             }
                             res<-paste(rev(res[1:n]),collapse="/")
                             res <- paste(Sys.Date(),": ",pre,res,"/",script.name," ",sep="")
                           }
                           res
                         },
                         #
                         corr.fontface=4,
                         #
                         eta.labels=NULL,
                         #
                         script.name="")

nonmem2R.globals$model.path<-""

nonmem2R.globals$params.default<-nonmem2R.globals$params

nonmem2R.globals$dictionary<-list(
  DV    ="Observations",
  PRED  ="Population predictions",
  IPRED ="Individual predictions",
  CWRES ="Cond. weighted res",
  IWRES ="Ind. weighted res",
  TIME  ="Time after first dose(h)",
  TAPD  ="Time since last dose(h)",
  TAD   ="Time since last dose(h)",
  TAFD  ="Time after first dose(h)"
)
nonmem2R.globals$dictionary.default<-nonmem2R.globals$dictionary

####################################################
#' Labels for known NONMEM variables
#' @description
#' get.label match known NONMEM variables to the GOF-dictionary and returns the matched label.
#' Unless trans is NULL, the label is modified to 'f(matched label)'
#' @param x
#' column to get label for
#' @param trans
#' transformation
#' @seealso [get.GOF.dictionary()],  [set.GOF.dictionary()], and [default.GOF.dictionary()].
#' @export
get.label<-function(x,trans=NULL){
  short.long<-nonmem2R.globals$dictionary#get.GOF.dictionary()
  y<-x
  if(nonmem2R.globals$params$axis.labels){
    ii<-match(x,names(short.long))
    y[!is.na(ii)]<-unlist(short.long)[ii[!is.na(ii)]]
    if(!is.null(trans)){
      y<-paste("f(",y,")",sep="")
    }
  }
  y
}


## Below code to generate the text for complete list of GOF params
#t1<-get.GOF.params()
#t2<-names(t1)
#cls<-NULL
#for(i in 1:length(t2)){
#  ni<-t2[i]
#  vi<-t1[[t2[i]]]
#  cli<-class(vi)
#  cls<-c(cls,cli)
#  if(cli=="character"){
#    cat(ni," \\tab \"",vi,"\" \\tab    \\cr \n",sep="")
#  }
#  if(cli=="numeric"){
#    cat(ni," \\tab ",vi," \\tab    \\cr \n",sep="")
#  }
#  if(cli=="logical"){
#    cat(ni," \\tab ",ifelse(vi,"TRUE","FALSE")," \\tab    \\cr \n",sep="")
#  }
#  if(cli=="NULL"){
#    cat(ni," \\tab NULL \\tab    \\cr \n",sep="")
#  }
#}
#table(cls)
#
#
#
#
#
#
#####################################################################################
#' Get, set, and modify the global (or local) GOF parameters and GOF dictionary for labels
#' @description
#' The current global GOF parameters and dictionary is automatically applied to every plot you draw.
#' Use `get.GOF.params` to get the current GOF parameters, and `set.GOF.params` to change
#' one or several parameters.
#' To change settings for one GOF only you can use GOF.control as input to the GOF function.
#' See details below for complete list of GOF parameters.
#'
#' Similarly use `get.GOF.dictionary` and `set.GOF.dictionary` for the dictionary of labels.
#' `set.GOF.dictionary` also allows for adding new items to the dictionary.
#'
#' Use `set.ETA.labels` to set labels for ETA's in all plots
#' @param ...
#' named list of GOF parameter or GOF dictionary for labels
#' @details
#' Table below describe each of the global GOF parameters that can be modified by `set.GOF.params`
#' \tabular{lll}{
#' \strong{Parameter} \tab \strong{Default} \tab \strong{Description} \cr
#' col.data \tab "gray20" \tab  color for points in all GOFs  \cr
#' cex.data \tab 1.5 \tab       cex for points in all GOFs \cr
#' pch.data \tab 19 \tab        pch for points in all GOFs \cr
#' alpha.data \tab 0.5 \tab     alpha for points in all GOFs \cr
#' col.smooth \tab "#3366FF" \tab   color for smoothers  \cr
#' lty.smooth \tab 1 \tab   lty for smoothers \cr
#' lwd.smooth \tab 1 \tab   lwd for smoothers \cr
#' se.smooth \tab TRUE \tab show confidence interval for loess smoother (TRUE) or not (FALSE)  \cr
#' span.smooth \tab 0.6666667 \tab  span parameter for loess smothers  \cr
#' degree.smooth \tab 1 \tab    degree  parameter for loess smothers \cr
#' family.smooth \tab "symmetric" \tab  family parameter for loess smothers  \cr
#' col.refline \tab "red" \tab   color for reference lines in all GOFs \cr
#' lty.refline \tab 2 \tab    lty for reference lines in all GOFs \cr
#' lwd.refline \tab 1 \tab    lwd for reference lines in all GOFs \cr
#' fill.hist \tab "gray30" \tab  color for filling of histograms   \cr
#' col.hist \tab "transparent" \tab color for border of histograms   \cr
#' alpha.hist \tab 0.5 \tab alpha for fill area in histograms   \cr
#' fill.box \tab "steelblue" \tab  color for filling of box-plots   \cr
#' col.box \tab "black" \tab color for border and whisker of box-plots   \cr
#' alpha.box \tab 0.6 \tab alpha for fill area in box-plots   \cr
#' axis.labels \tab TRUE \tab   use dictionary for labels (TRUE) or just use column names for axis labels (FALSE) \cr
#' add.caption \tab TRUE \tab   add caption to GOF plots (true) or not (FALSE) \cr
#' size.caption \tab 8 \tab    size for caption text \cr
#' col.caption \tab 1 \tab    color for caption text \cr
#' caption.path.depth \tab 99 \tab  number of folders levels to include in caption  \cr
#' corr.fontface \tab 4 \tab    font face for correlation in eta.pairs.GOF \cr
#' eta.labels \tab NULL \tab    vector of labels for ETA's\cr
#' script.name \tab "" \tab     script name for caption\cr
#' }
#' @export
get.GOF.dictionary <- function() {
  nonmem2R.globals$dictionary
}

#####################################################################################
#' @rdname get.GOF.dictionary
#' @export
set.GOF.dictionary <- function(...) {
  a<-list(...)
  nm<-setdiff(names(a),names(nonmem2R.globals$dictionary))
  if(length(nm)>0){
    cat(paste("Adding",paste(nm,collapse=","),"\n"))
  }
  tmp<-nonmem2R.globals$dictionary
  tmp[names(a)]<-a
  nonmem2R.globals$dictionary<-tmp
}

#####################################################################################
#' @rdname get.GOF.dictionary
#' @export
default.GOF.dictionary <- function() {
  nonmem2R.globals$dictionary<-nonmem2R.globals$dictionary.default
}

#####################################################################################
#' @rdname get.GOF.dictionary
#' @export
set.GOF.params <- function(...) {
  a<-list(...)
  nm<-setdiff(names(a),names(nonmem2R.globals$params))
  if(length(nm)>0){
    stop(paste(nm," is not a GOF parameter, aborting\n"))
  }
  tmp<-nonmem2R.globals$params
  tmp[names(a)]<-a
  nonmem2R.globals$params<-tmp
}

#####################################################################################
#' @rdname get.GOF.dictionary
#' @export
get.GOF.params <- function() {
  nonmem2R.globals$params
}

#####################################################################################
#' @rdname get.GOF.dictionary
#' @export
default.GOF.params <- function() {
  scriptName<-get.GOF.params()$script.name
  nonmem2R.globals$params<-nonmem2R.globals$params.default
  set.GOF.params(script.name=scriptName)
}

#####################################################################################
#' @rdname get.GOF.dictionary
#' @param labels
#' vector of character strings with the labels
#' @export
#' @examples
#' ### Example setting ETA labels
#' set.ETA.labels(c("Ka","CL","V"))
set.ETA.labels <- function(labels) {
  nonmem2R.globals$params$eta.labels<-labels
}

#####################################################################################
#' @rdname get.GOF.dictionary
#' @param script.name
#' character strings for script name to put at end of caption
#' @export
#' @examples
#' ### Example setting script name
#' set.script.name("MyScript.R")
set.script.name <- function(script.name) {
  nonmem2R.globals$params$script.name<-script.name
}

####################################################
#' @rdname get.GOF.dictionary
#' @export
GOF.control<-function(...)
{
  a<-list(...)
  nm<-setdiff(names(a),names(nonmem2R.globals$params))
  if(length(nm)>0){
    stop(paste(nm," is not a GOF parameter, aborting\n"))
  }
  tmp<-nonmem2R.globals$params
  tmp[names(a)]<-a
  tmp
}


####################################################
#' Get and set model.path
#' @description
#' The model.path in the nonmem2R set of global variables is used by functions
#' loading NONMEM or PSN generated output file. The model.path can ge retrived by get.model.path and
#' can be set using set-model.path. When loading nonmem2R the model-path is set to "",
#' e.g. the current working directory. The model.path can be set abolute or relative, see examples.
#' @export
#' @examples
#' \dontrun{
#' ## Example for setting absolute path
#' set.model.path("c:/NONMEM")
#´
#' ## Example for setting relative path,
#' set.model.path("../NONMEM")
#' }
get.model.path<-function(){
  if (nonmem2R.globals$model.path=="" & exists("model.path")) {
    cat("The user defined variable model.path ignored in nonmem2R 2.01 and later\n")
    cat("Instead use the nonmem2R function set.model.path to define path to model files\n")
  }
  nonmem2R.globals$model.path
}

####################################################
#' @rdname get.model.path
#' @param model.path
#' character string for path to model files
#' @export
set.model.path<-function(model.path){
  if(!dir.exists(model.path) & model.path!=""){
    stop(paste(model.path,"is no a directory folder or does not exists\n"))
  }
  else{
    n<-nchar(model.path)
    if(n>0 & substr(model.path,n,n)!="/"){
      model.path<-paste(model.path,"/",sep="")
    }
    nonmem2R.globals$model.path<-model.path
  }
}
