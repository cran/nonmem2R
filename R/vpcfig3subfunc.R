categorical_vpc_sub<-function(d1,d2,ss,pred.corr,...){
	pred.corr.sm<-1
	## Compute pred correction
	## Here fit on log-scale
	if(pred.corr=="pred-corr-prop"){
		tmp<-aggregate(log(PRED)~x_x_x*strata,data=d1,mean)
		pred.sm<-exp(spline(tmp$x,tmp[,3],xout=d1$x_x_x)$y)
		pred.corr.sm<-pred.sm/d1$PRED
		d1$y_y_y<-d1$y_y_y*pred.corr.sm
	}	
	if(pred.corr=="pred-corr-add"){
		tmp<-aggregate(PRED~x_x_x*strata,data=d1,mean)
		pred.sm<-(spline(tmp$x,tmp[,3],xout=d1$x_x_x)$y)
		pred.corr.sm<-pred.sm-d1$PRED
		d1$y_y_y<-d1$y_y_y+pred.corr.sm
	}
	##observed
	d3i<-aggregate(y_y_y~x_x_x*strata,data=d1,mean)
	##simulated
	f<-function(x){aggregate((x*pred.corr.sm)~d1$x_x_x,FUN=mean)[,2]}
	if(pred.corr=="pred-corr-add"){
		f<-function(x){aggregate((x+pred.corr.sm)~d1$x_x_x,FUN=mean)[,2]}
	}	
	tmp<-apply(d2[,-1],2,f)
	## Get mean and quantiles of loess curves simulation
	d5i<-d3i
	if(NCOL(tmp)==1){tmp<-matrix(tmp,nrow=1)}
	d5i$y_y_y<-apply(tmp,1,mean)
	d5i$q5<-apply(tmp,1,quantile,0.05)
	d5i$q95<-apply(tmp,1,quantile,0.95)
	list(d3i,d5i,d1$y_y_y)
}		  

loess_vpc_sub_predcorr<-function(d1,d2,ss,pred.corr,...){
	pred.corr.sm<-1
	## Compute pred correction
	## Here fit on log-scale
	if(pred.corr=="pred-corr-prop"){
		tmp<-loess.smooth(d1$x_x_x,log(d1$PRED),...)
		pred.sm<-exp(spline(tmp$x,tmp$y,xout=d1$x_x_x)$y)
		pred.corr.sm<-pred.sm/d1$PRED
		d1$y_y_y<-d1$y_y_y*pred.corr.sm
	}	
	if(pred.corr=="pred-corr-add"){
		tmp<-loess.smooth(d1$x_x_x,d1$PRED,...)
		pred.sm<-spline(tmp$x,tmp$y,xout=d1$x_x_x)$y
		pred.corr.sm<-pred.sm-d1$PRED
		d1$y_y_y<-d1$y_y_y+pred.corr.sm
	}
	##observed
	tmp<-loess.smooth(d1$x_x_x,d1$y_y_y,...)
	d3i<-data.frame(x_x_x=tmp$x,y_y_y=tmp$y,strata=ss)
	##simulated
	f<-function(x){loess.smooth(d1$x_x_x,x*pred.corr.sm,...)$y}  
	if(pred.corr=="pred-corr-add"){
		f<-function(x){loess.smooth(d1$x_x_x,x+pred.corr.sm,...)$y}
	}	
	tmp<-apply(d2[,-1],2,f)
	## Get mean and quantiles of loess curves simulation
	d5i<-d3i
	d5i$y_y_y<-apply(tmp,1,mean)
	d5i$q5<-apply(tmp,1,quantile,0.05)
	d5i$q95<-apply(tmp,1,quantile,0.95)
	list(d3i,d5i,d1$y_y_y)
}		  
loess_vpc_sub<-function(d1,d2,ss,...){
	##observed
	tmp<-loess.smooth(d1$x_x_x,d1$y_y_y,...)
	d3i<-data.frame(x_x_x=tmp$x,y_y_y=tmp$y,strata=ss)
	##simulated
	f<-function(x){loess.smooth(d1$x_x_x,x,...)$y}
	tmp<-apply(d2[,-1],2,f)
	## Get mean and quantiles of loess curves simulation
	d5i<-d3i
	d5i$y_y_y<-apply(tmp,1,mean)
	d5i$q5<-apply(tmp,1,quantile,0.05)
	d5i$q95<-apply(tmp,1,quantile,0.95)
	list(d3i,d5i,d1$y_y_y)
}		  

loess_spline_sub<-function(d1,d2,ss,minobs,knots,...){		  
	knots2<-min(floor(nrow(d1)/minobs),knots)
	bsi<-bSpline(d1$x_x_x,degree=1,df=knots2+1,intercept=TRUE)
	x.new<-sort(c(attr(bsi,"knots"),attr(bsi,"Boundary.knots")))
	bsp<-bSpline(x.new,degree=1,knots=attr(bsi,"knots"),intercept=TRUE)
	##observed
	mi<-lm(d1$y_y_y~bsi-1)
	y.new<-(bsp%*%mi$coef)[,1]
	d3i<-data.frame(x_x_x=x.new,y_y_y=y.new,strata=ss)
	##simulated
	f<-function(x){	mi<-(bsp%*%lm(x~bsi-1)$coef)[,1]}
	tmp<-apply(d2[,-1],2,f)
	## Get mean and quantiles of loess curves simulation
	d5i<-d3i
	d5i$y_y_y<-apply(tmp,1,mean)
	d5i$q5<-apply(tmp,1,quantile,0.05)
	d5i$q95<-apply(tmp,1,quantile,0.95)
	list(d3i,d5i,d1$y_y_y)
}
