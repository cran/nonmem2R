#' Combine fix grid and simulated grid based on multivariate normal distribution
#' @description
#' grid.sim produce comparable output to mvnorm, however for methods 2 to 4 grid.sim make use of center and scaling,
#' and or use a fix grid of values for one column of the output. The intended use of grid.sim is for computing
#' confidence intervals (CI) for model predictions.
#' Method 1 is the same as rmvnorm, however can handle cases of zero variances in sigma.
#' Method 2 is method 1 followed by centering and scaling of the simulated matrix. Thus output when using method 2
#' always have sample mean and covariance equal to input parameters means and sigma.
#' Method 3 is the same as method 1 however with one column having a fixed range of values rather than a
#' simulated range of values.
#' Method 4 is method 3 followed by centering and scaling of the simulated matrix. Thus output when using method 4
#' always have sample mean and covariance equal to input parameters means and sigma.
#'
#' As the number of simulations (n) goes to infinity all methods in grid.sim are identical to mvnorm in
#' that the sample covariance (mean) of the output will converge to the input covariance matrix sigma (vector means).
#'
#' The advantage with methods 2 to 4 (over method 1 and rmvnorm) is that they provides more stable results,
#' hence number of simulations can be reduced and still have equally stable results when used to represent
#' parameter uncertainty and or population variability in model predictions.
#'
#' @param n
#' Number of simulations
#' @param means
#' vector of mean values
#' @param sigma
#' covariance matrix
#' @param grid.param
#' the index of the parameter for which a fix grid (from qnorm) is used instead of a simulated grid (rnorm).
#' If grid.param=NULL (default) the fix grid will be used for the parameter with largest variance.
#' @param method
#' simulation method, default=4
#' @return
#' row-matrix of parameters
#' @export
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats cov
#' @importFrom stats qnorm
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @examples
#' sigma<-matrix(c(1,0.5,0.5,2),ncol=2)
#' sim1<-grid.sim(1000,sigma=sigma)
#' pairs(sim1)
#' cov(sim1)
grid.sim<-function (n, means = NULL, sigma, grid.param = NULL, method = 4){
  if (!is.matrix(sigma)) {
    sigma <- matrix(sigma, ncol = floor(sqrt(length(sigma))))
  }
  if (length(means) == 0) {
    means = rep(0, ncol(sigma))
  }
  res <- matrix(means, byrow = T, ncol = length(means), nrow = n)
  if (is.null(grid.param)) {
    grid.param <- which.max(diag(sigma))
  }

  ## Get sigma and mean of non-zero variance columns
  zero.var <- (diag(sigma) == 0)
  sigma2 <- sigma[!zero.var, !zero.var]
  means2 <- means[!zero.var]

  ## Get grid.param among non-zero variance columns
  grid.param2<-which(((1:ncol(sigma))==grid.param)[!zero.var])

  if(method==1){
    X<-grid.sim.internal1(n, means = means2, sigma2, grid.param = grid.param2, pure.sim = TRUE)
  }
  if(method==2){
    X<-grid.sim.internal3(n, means=means2, sigma2)
  }
  if(method==3){
    X<-grid.sim.internal1(n, means = means2, sigma2, grid.param = grid.param2, pure.sim = FALSE)
  }
  if(method==4 & length(means2)>1){
    X<-grid.sim.internal2(n, means = means2, sigma2, grid.param = grid.param2)
  }
  if(method==4 & length(means2)==1){
    X<-grid.sim.internal1(n, means = means2, sigma2, grid.param = grid.param2, pure.sim = FALSE)
  }
  res[, !zero.var] <- X
  res
}

#' Function for testing grid.sim and compare with mvnorm
#' @description
#' Test grid.sim
#' @param n
#' number of simulations
#' @param k
#' subset of parameters from a 4X4 sigma to use
#' @return
#' grapics
#' @export
#' @importFrom lattice xyplot
#' @examples
#' \dontrun{
#' require(lattice)
#' test.grid.sim(n=1000)
#' }
test.grid.sim<-function(n=1000,k=1:4){
	sigma<-matrix(ncol=4,byrow=T,c(
		0,0  ,0 ,0,
		0,1  ,0.5,0,
		0,0.5,1.0,0.25,
		0,0  ,0.25,2
	))
	means<-0:3
	sigma<-sigma[k,k]
	means<-means[k]
	a<-grid.sim(n,means,sigma)
	if(length(means)>1){
		b<-rmvnorm(n,mean=means,sigma=sigma)
	}
	else{
		b<-matrix(rep(means,length=n))
		if(sigma>0){
			b<-matrix(rnorm(n,mean=means,sd=sqrt(sigma)))
		}
	}

	#b<-rmvnorm(n,means,sigma)
	r<-NULL
	q<-qnorm(seq(0.5/n,1-0.5/n,length=n))
	for(i in 1:length(means)){
		r<-rbind(r,data.frame(i=k[i],q=q,grid.sim=sort(a[,i]),mvnorm=sort(b[,i])))
	}
	p1<-xyplot(grid.sim+mvnorm~q|factor(i),cex=c(0.8,0.6),data=r,as.table=T,auto.key=list(columns=2))
	cat("Covariance from grid.sim output:\n")
	print(cov(a))
	cat("Covariance from mvnorm output:\n")
	print(cov(b))
	p1
}



#test.grid.sim(n=1000,k=1:4)
#test.grid.sim(n=1000,k=1:3)
#test.grid.sim(n=1000,k=2:4)
#test.grid.sim(n=1000,k=2:3)
#test.grid.sim(n=1000,k=1:2)
#test.grid.sim(n=1000,k=2)
#test.grid.sim(n=1000,k=1)

################################################################################################
################################################################################################
### Define refined version of grid.sim
### Here with bug fix for grid.param then moved to internal function
#' Internal function
#' @description
#' Internal function
#' @param n
#' Number of simulations
#' @param means
#' vector of mean values
#' @param sigma
#' covariance matrix
#' @param grid.param
#' the index of the parameter for which a fix grid (from qnorm) is used instead of a simulated grid (rnorm).
#' If grid.param=NULL ( default) the fix grid will be used for the parameter with largest variance.
#' @param pure.sim
#' Pure sim or use grid simulation
#' @return
#' row-matrix of parameters
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats cov
#' @importFrom stats qnorm
#' @importFrom stats rnorm
#' @importFrom stats sd
grid.sim.internal1<-function (n, means = NULL, sigma, grid.param = NULL, pure.sim = FALSE){
  if (!is.matrix(sigma)) {
    sigma <- matrix(sigma, ncol = floor(sqrt(length(sigma))))
  }
  if (length(means) == 0) {
    means = rep(0, ncol(sigma))
  }
  res <- matrix(means, byrow = T, ncol = length(means), nrow = n)
  if (is.null(grid.param)) {
    grid.param <- which.max(diag(sigma))
  }

  ## Get sigma and mean of non-zero variance columns
  zero.var <- (diag(sigma) == 0)
  sigma2 <- sigma[!zero.var, !zero.var]
  means2 <- means[!zero.var]

  ## Get grid.param among non-zero variance columns
  grid.param2<-which(((1:ncol(sigma))==grid.param)[!zero.var])

  X <- NULL
  if (length(means2) > 1) {
    X <- rmvnorm(n, mean = means2, sigma = sigma2)
  }
  if (length(means2) == 1) {
    X <- matrix(rnorm(n, mean = means2, sd = sqrt(sigma2)))
  }
  if (!pure.sim & length(means2) > 1) {
    X2 <- qnorm(seq(0.5/n, 1 - 0.5/n, length = n))
    X2 <- X2 * sqrt(diag(sigma2)[grid.param2])/sd(X2) + means2[grid.param]
    mu1 <- means2[-grid.param2]
    mu2 <- means2[grid.param2]
    SIGMA12 <- sigma2[-grid.param2, grid.param2]
    SIGMA22 <- sigma2[grid.param2, grid.param2]
    SIGMA11 <- sigma2[-grid.param2, -grid.param2]
    mubar <- NULL
    for (i in 1:length(mu1)) {
      mubar <- cbind(mubar, mu1[i] + SIGMA12[i] * (X2 -
                                                     mu2)/SIGMA22)
    }
    SIGMAbar <- SIGMA11 - SIGMA12 %*% t(SIGMA12)/SIGMA22
    X1 <- rmvnorm(n, sigma = SIGMAbar) + mubar
    X[, grid.param2] <- X2
    X[, -grid.param2] <- X1
  }
  if (!pure.sim & length(means2) == 1) {
    X <- qnorm(seq(0.5/n, 1 - 0.5/n, length = n))
    X <- matrix(X * sqrt(sigma2)/sd(X) + means2)
  }
  res[, !zero.var] <- X
  res<-res[order(runif(nrow(res))),]
  res
}


################################################################################################
### Define version 2 of grid.sim doing centering and scaling
#' Internal function
#' @description
#' Internal function
#' @param n
#' Number of simulations
#' @param means
#' vector of mean values
#' @param sigma
#' covariance matrix
#' @param grid.param
#' the index of the parameter for which a fix grid (from qnorm) is used instead of a simulated grid (rnorm).
#' If grid.param=NULL ( default) the fix grid will be used for the parameter with largest variance.
#' @return
#' row-matrix of parameters
#' @importFrom stats cov
#' @importFrom MASS ginv
grid.sim.internal2<-function (n, means = NULL, sigma, grid.param = NULL)
{
  if (!is.matrix(sigma)) {
    sigma <- matrix(sigma, ncol = floor(sqrt(length(sigma))))
  }
  if (length(means) == 0) {
    means = rep(0, ncol(sigma))
  }
  if (is.null(grid.param)) {
    grid.param <- which.max(diag(sigma))
  }

  ### Re-order columns so that grid.param is in first colmns
  kk<-c(grid.param,setdiff(1:ncol(sigma),grid.param))
  sigma<-sigma[kk,kk]

  X1<-grid.sim.internal1(n,sigma=diag(ncol(sigma)),grid.param=1)
  D1<-ginv(chol(cov(X1)))
  X2<-X1%*%(D1)

  D2<-chol(sigma)
  X3<-X2%*%(D2)
  kk<-order(kk)
  X3<-scale(X3,scale=FALSE)

  X4<-X3[,kk] + matrix(means, byrow = T, ncol = length(means), nrow = n)
  X4
}

################################################################################################
### Define version 3 of rmvnorm
# @importFrom base scale
# @importFrom base chol
#' Internal function
#' @description
#' Internal function
#' @param n
#' Number of simulations
#' @param means
#' vector of mean values
#' @param sigma
#' covariance matrix
#' @return
#' row-matrix of parameters
#' @importFrom stats cov
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats cov
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom MASS ginv
grid.sim.internal3<-function (n, means=NULL, sigma)
{
  if(length(sigma)>1){
    X<-rmvnorm(n,sigma=sigma)
  }
  if(length(sigma)==1){
    X<-rnorm(n,sd=sqrt(sigma))
  }
  ### Center and scale X
  if(length(sigma)>1){
    X<-scale(X,scale=FALSE)
    D1<-chol(ginv(cov(X)))
    D2<-chol(sigma)
    X<-X%*%t(D1)%*%(D2)
    X<-X+matrix(means,byrow=T,ncol=ncol(X),nrow=nrow(X))
  }
  if(length(sigma)==1){
    X<-(X-mean(X))/sd(X)*sqrt(sigma) + means
  }
  X
}


