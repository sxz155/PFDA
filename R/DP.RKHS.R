# unlink("", recursive = TRUE)
#' Releasing Differential Private RKHS smoothing mean of a dataset
#'
#' This function create a DP RKHS smoothing mean from an existing data set
#' with known eigenvalues and eigenvectors
#' @param Data a matrix which the of interest curves are located in columns
#' @param grid grid (x-axis) for each curve, default is equally espaced between 0 and 1.
#' @param alpha,beta Privacy parameters, real numbers
#' @param phi real number, penalty parameter
#' @param ro kernel range parameter in kernel, real number
#' @param drop.col TRUE/FALSE for cleaning the Data, deleting Columns/Rows including missing values
#' @return f.tilda: DP RKHS smoothing mean
#' @return delta: the coefficient of the noise, real number
#' @return f: RKHS smoothing mean
#' @return X: original data (X=Data)
DP.RKHS=function(grid,Data,alpha=1,beta=0.1,kernel="Gau",phi=0.01,ro=0.2,col.drop=TRUE){

### Cleaning Data

  if(col.drop==TRUE){
  drop<-unique(which(is.na(Data),arr.ind = T)[,2])  # columns with missing data
  if(length(drop)!=0){
  Data<-Data[,-drop] # Columns with missing value are deleted
  }
}
  if(col.drop==FALSE){
    drop<-(unique(which(is.na(Data),arr.ind = T)[,1]))   # Rows with missing data
    if(length(drop)!=0){
    Data<-Data[-drop,] # Rows with missing value are deleted
    }
  }

### Grid

  if(missing(grid)){
    grid=seq(0,1,length.out = dim(Data)[1])
    n=length(grid)
  }
  else{
    grid=(as.numeric(grid)-min(as.numeric(grid)))/diff(range(as.numeric(grid)))
    n=length(grid)
  }

### Kernel

  if(is.character(kernel)==TRUE){
    if(kernel=="Exp"){   # kernel of covariance operator of Exponential Process
      C=function(t,s,ro){return(exp(-abs(t-s)/ro))}
    }

    else if(kernel=="M3/2"){ # kernel of covariance operator of Matern Process nu=3/2
      C=function(t,s,ro){ return((1+sqrt(3)*abs(t-s)/ro)*exp(-sqrt(3)*abs(t-s)/ro))}
    }

    else if(kernel=="M5/2"){ # kernel of covariance operator of Matern Process nu=5/2
      C=function(t,s,ro){ return((1+(sqrt(5)*abs(t-s)/ro)+(5*(abs(t-s)^2)/(3*ro^2)))*exp(-sqrt(5)*abs(t-s)/ro))}
    }

    else if(kernel=="Gau"){ # kernel of covariance operator of Gaussian Process
      C=function(t,s,ro){ return(exp(-(abs(t-s)^2)/ro))}
    }

    else if(kernel=="Sob"){ # kernel of covariance operator of Sobolev
      C=function(t,s,ro){return((ro/sinh(ro))*cosh(ro*(1-max(s,t)))*cosh(ro*min(t,s)))}
    }
  }
  else{
    C=kernel
  }

###

  Sig=matrix(nrow=n,ncol=n)   # covariance matrix in the grid [0,1]

  for(i in 1:n){
    for(j in 1:n){  # Sig_{i,j}=C( (i-1)/(n-1) , (j-1)/(n-1) )
      Sig[i,j] = C(grid[i],grid[j],ro=ro)
    }
  }

###

  gamma=eigen(Sig)$values
  e.val.z=gamma[1:n]/n
  e.vec.z=eigen(Sig)$vectors[,1:n]
  m=length(which(e.val.z>0))

### generating Gaussian proccess Z based on KL-expansion

  Z=matrix(0,nrow = n,ncol = 1)
  for(i in 1:m){
    Z=Z+sqrt(e.val.z[i])*rnorm(1)*e.vec.z[,i]
  }

###  generating f_{D} for girls

  xg=Data
  N=dim(xg)[2]

  Ym=matrix(NA,n,N)
  for(s in 1:N){
    Y=matrix(0,n,1)
    for(i in 1:m){
      Y=Y+(e.val.z[i]/(e.val.z[i]+phi))*
        (as.numeric(t(xg[,s]))%*%e.vec.z[,i])*e.vec.z[,i]
    }
    Ym[,s]=Y
  }

  f=rowMeans(x = Ym)

###

  MM=matrix(NA,N,1)
  for(i in 1:N){
    MM[i,1]=sqrt(pracma::trapz(grid,xg[,i]^2))
  }
  MAX=max(MM)

### calculating delta

  Delta2=((4*MAX^2)/(N^2))*(sum(e.val.z/((e.val.z+phi)^2))) # phi should not be zero
  delta=sqrt(((2*log(2/beta))/(alpha^2))*(Delta2))

### generating f.tilda

  f.tilda=f+delta*Z

  out=list("f.tilda"=f.tilda,"delta"=delta,"f"=f,"grid"=grid,"X"=Data)

###

 return(out)

}
