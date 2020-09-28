## ----echo=FALSE,message=FALSE,warning=FALSE------------------------------
library(nonmem2R)
rm(list=ls())

## ----echo=FALSE,fig.width=7,fig.height=5---------------------------------
# Get path to the example files included in nonmem2R package
file1 <- system.file("extdata", "vpctab004.dat", package = "nonmem2R")
path1 <-gsub("vpctab004.dat","",file1)
path2 <- system.file("extdata", "vpc001", package = "nonmem2R")

## ----echo=TRUE,fig.width=7,fig.height=5----------------------------------
# Get path to the example files included in nonmem2R package
file1 <- system.file("extdata", "vpctab004.dat", package = "nonmem2R")
file2 <- system.file("extdata", "vpc_results.csv", package = "nonmem2R")
# produce VPC with default setting, here specifying both vpctab and vpcresult
vpcfig2(vpctab=file1,vpcresult=file2)

## ----echo=TRUE,fig.width=7,fig.height=5----------------------------------
vpcfig2(path1)+
  labs(x="Time after dose (hours)",y="Plasma concentration (nmol/L)")+
  scale_y_log10()+
  theme_bw()

## ----echo=TRUE,fig.width=7,fig.height=5----------------------------------
fy<-function(y){log(y+1)}
y.ticks<-c(0,1,10,100,1000)
vpcfig2(path1,fy=fy)+
  labs(x="Time after dose (hours)",y="Plasma concentration (nmol/L)")+
  scale_y_continuous(breaks=fy(y.ticks),labels=y.ticks,minor_breaks=NULL)+
  theme(legend.position="top")

## ----echo=TRUE,fig.width=7,fig.height=6----------------------------------
vpcfig2(path1,strata.subset=2:1,strata.names=c("Cohort 1","Cohort 2"))

## ----echo=TRUE,fig.width=7,fig.height=6----------------------------------
f1<-vpcfig2(path2,fy=log,strata.names=c("710mg","1000mg"),ylab="Plasma conc (umol/L)",
            censoring.labels=c("a","BLQ(%)"),xlab="Time after dose (hrs)")
##breaks for DV
bry<-c(0.1,0.2,0.4,0.6,1,2,4,6,10,20,40,60)
mbry<-c(1:10,(1:10)/10,(1:10)*10)
##breaks for BLQ, copied from the vpcfif2 console output
brb<-c(5.55058263829153,6.2985767486289,7.04657085896627)
lbb<-c(0,50,100)
## Get minor breaks for BLQ in between the main breaks
mbrb<-(brb[-1]+brb[length(brb)])/2
f1+scale_y_continuous(breaks=c(brb,log(bry)),
                      labels=c(lbb,bry),minor_breaks=c(mbrb,log(mbry)))

