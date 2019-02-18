## ----echo=FALSE,message=FALSE,warning=FALSE------------------------------
library(nonmem2R)
rm(list=ls())
file1 <- system.file("extdata", "sdtab999", package = "nonmem2R")
sdtab<-read.table(file=file1,skip=1,header=TRUE)
sdtab$gender<-c("Female","Male")[sdtab$SEXM+1]
sdtab$gender<-c("Female","Male")[sdtab$SEXM+1]
sdtab$eGFR<-cut(sdtab$BEGFR,breaks=c(35,80,130),labels=c("eGFR<=80","eGFR>80"))

## ----echo=FALSE,fig.width=7,fig.height=6---------------------------------
set.script.name("MyScript.R")
basic.GOF4(subset(sdtab,DV>0),idv2="TAPD")

## ----echo=TRUE,fig.width=7,fig.height=5----------------------------------
do.individual.GOF(subset(sdtab,DV>0 & ID<7 & TIME<50))

## ----echo=TRUE,fig.width=7,fig.height=6----------------------------------
basic.GOF6(subset(sdtab,DV>0),idv1="TAPD",idv2="PRED")

## ----echo=TRUE,fig.width=7,fig.height=5----------------------------------
basic.eta.GOF(sdtab)

## ----echo=TRUE,fig.height=8,fig.width=7----------------------------------
eta.cov.GOF(subset(sdtab,DV>0),covariates=c("AGE","BWT"))

## ----echo=TRUE,fig.width=7,fig.height=5----------------------------------
eta.cat.GOF(subset(sdtab,DV>0),covariates=c("gender","eGFR"),type="covariate-by-page")

## ----echo=TRUE,fig.width=7,fig.height=7----------------------------------
set.GOF.params(eta.labels=c("Ka","Vc","CL","F"))
eta.pairs.GOF(sdtab,density2D="lower")

## ----echo=TRUE,fig.width=7,fig.height=6----------------------------------
basic.GOF4(subset(sdtab,DV>0),idv2="TAPD",global.ggplot.options=facet_wrap(~gender))

## ----echo=TRUE,fig.width=7,fig.height=6----------------------------------
basic.GOF6(subset(sdtab,DV>0),idv2="TAPD",global.ggplot.options=theme_classic())

## ----echo=TRUE,eval=TRUE-------------------------------------------------
set.GOF.dictionary(TAD="Time since last dose(days)")
set.GOF.dictionary(TIME="Time since first dose(days)")

## ----echo=TRUE,eval=TRUE-------------------------------------------------
get.GOF.dictionary()

## ----echo=TRUE,eval=TRUE,fig.width=5,fig.height=5------------------------
set.GOF.params(pch.data = 1)
do.one.GOF(subset(sdtab,DV>0),"PRED","DV")

## ----echo=TRUE,eval=TRUE,fig.width=7,fig.height=6------------------------
set.GOF.params(caption.path.depth=3,size.caption = 14,pch.data = 19)
basic.GOF4(subset(sdtab,DV>0))
### Change back to default
set.GOF.params(caption.path.depth=99,size.caption =8)

## ----echo=TRUE,eval=TRUE,fig.width=5,fig.height=5------------------------
set.GOF.params(
  get.caption=function(x){
      paste(getwd(),get.GOF.params()$script.name,date(),sep="\n")
      }
  )
do.one.GOF(subset(sdtab,DV>0),"PRED","DV")

## ----echo=TRUE,eval=TRUE-------------------------------------------------
get.GOF.params()

## ----echo=TRUE,fig.width=7,fig.height=4----------------------------------
do.one.GOF(subset(sdtab,DV>0),x="PRED",y="DV")+facet_wrap(~gender)

## ----echo=TRUE,fig.width=7,fig.height=4----------------------------------
do.multi.GOF(subset(sdtab,DV>0),x="TAPD",y=c("DV","PRED","IPRED"))+theme_bw()

## ----echo=TRUE,fig.width=7,fig.height=4----------------------------------
sdtab$TAPDgr<-cut(sdtab$TAPD,breaks=c(0,0.5,2,10,25))
histGOF(subset(sdtab,DV>0),"CWRES")+facet_wrap(~TAPDgr,nrow=1)

## ----echo=TRUE,fig.width=7,fig.height=4----------------------------------
qqnormGOF(subset(sdtab,DV>0),"CWRES")+facet_wrap(~TAPDgr,nrow=1)

## ----echo=TRUE,fig.width=7,fig.height=5----------------------------------
eta.qqnorm.GOF(subset(sdtab,DV>0))+facet_grid(gender~variable)

## ----echo=TRUE,fig.width=7,fig.height=5----------------------------------
p1<-do.one.GOF(subset(sdtab,DV>0),
               "PRED",
               "DV",
               control=GOF.control(add.caption=FALSE)) + labs(title="Re-building basic.GOF4")
p2<-do.one.GOF(subset(sdtab,DV>0),
               "IPRED",
               "DV",
               control=GOF.control(add.caption=FALSE))
sqrt.abs<-function(x){sqrt(abs(x))}
p3<-do.one.GOF(subset(sdtab,DV>0),
               "IPRED",
               "CWRES",
               refline="hrefmedian",
               fy=sqrt.abs,
               control=GOF.control(add.caption=FALSE)) + labs(y=a<-expression(sqrt('|Cond. weighted res.|')))
p4<-do.one.GOF(subset(sdtab,DV>0),
               "TAPD",
               "CWRES",
               refline="href0")
merge4GOF(p1,p2,p3,p4)

