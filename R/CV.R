# unlink("", recursive = TRUE)
#' Cross Validation for finding RKHS smoothing mean parameters
#'
#'This function apply two "Regular" and "Irregular" methods to find the penalty (\eqn{\phi}) and
#'kernel range parameter (\eqn{\rho}) in an RKHS smoothing mean
#' @param Data a matrix which the of interest curves are located in columns
#' @param grid grid (x-axis) for each curve, default is equally espaced between 0 and 1.
#' @param alpha,beta Privacy parameters, real numbers
#' @param kernel kernel function, can be "Exp" (Exponential kernel),
#' "M3/2" (Matern precess with \eqn{\nu}=3/2) "M5/2" (Matern precess with \eqn{\nu}=5/2)
#' "Gau" (Gaussian kernel) and "Sob" (Sobolev kernel)
#' else define it as a bivariate kernel function with parameters "t" and "s" and a range parameter "ro".
#' @param phi a real vector of penalty parameters, It will be done a grid search on them
#' to find the minimum Cross Validation
#' @param ro a real vector of kernel range parameters, It will be done a grid search on them
#' to find the minimum Cross Validation
#' @param fold number of fold using in Cross Validation
#' @param cv.penalty "Regular" or "Irregular"
#' @param Rep number of replications, (just) for "Irregular" method
#' @print.cv TRUE/FALSE, if print or not the matrix of CV for each combination of the penalty and kernel range parameters
#' @print.estimated.time TRUE/FALSE, if print or not the estimated remaining time
#' @return par: the optimum penalty (\eqn{\phi}) and kernel range parameter (\eqn{\rho})
#'in an RKHS smoothing mean which gives the minimum Cross Validation
#' @return time: estimatation of remaining time
#' @export CV

CV=function(grid,Data,alpha=1,beta=0.1,kernel="Exp",phi=c(0.01),ro=c(0.2),fold=10,
   cv.penalty="Regular",Rep=1000,print.cv=TRUE,print.estimated.time=TRUE,col.drop=TRUE){

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


###

  CV=matrix(NA,length(phi),length(ro))
  CV.P=matrix(NA,length(phi),length(ro))
  count=0

### Defining Kernels

### Grid

  if(missing(grid)){
    grid=seq(0,1,length.out = dim(Data)[1])
    n=length(grid)
  }
  else{
    grid=(as.numeric(grid)-min(as.numeric(grid)))/diff(range(as.numeric(grid)))
    n=length(grid)
  }

##

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

  flds <- caret::createFolds(c(1:(dim(Data)[2])), k = fold, list = TRUE, returnTrain = FALSE)
  Time1=Sys.time()

###

  for(I in 1:length(phi)){
   for(J in 1:length(ro)){

      Sig=matrix(nrow=n,ncol=n)   # covariance matrix in the grid [0,1]

       for(i in 1:n){
          for(j in 1:n){  # Sig_{i,j}=C( (i-1)/(n-1) , (j-1)/(n-1) )
                Sig[i,j] = C(grid[i],grid[j],ro=ro[J])
                       }
                    }

###

      gamma=eigen(Sig)$values
      e.val.z=gamma[1:n]/n
      e.vec.z=eigen(Sig)$vectors[,1:n]
      m=length(which(e.val.z>0))

###

      if(cv.penalty=="Regular"){

        MF=matrix(0,nrow = fold,1)

### Finding Cross Validation

        for(i in 1:fold){
          xg=Data[,-flds[[i]]]
          N=dim(xg)[2]

          Ym=matrix(NA,n,N)

          for(s in 1:N){
            Y=matrix(0,n,1)
            for(j in 1:m){
              Y=Y+(e.val.z[j]/(e.val.z[j]+phi[I]))*
                (as.numeric(t(xg[,s]))%*%e.vec.z[,j])*e.vec.z[,j]
            }
            Ym[,s]=Y
          }

          f=rowMeans(x = Ym)

          Q=0
          for(k in flds[[i]]){
            Q=Q+sqrt(pracma::trapz(grid,(f-Data[,k])^2))
          }
          MF[i]=Q/length(flds[[i]])
        }
        CV[I,J]=mean(MF)
      }

###

      if(cv.penalty=="Irregular"){

        MF.P=matrix(0,nrow = fold,1)

        for(i in 1:fold){
          xg=Data[,-flds[[i]]]
          N=dim(xg)[2]

          f.hat=rowMeans(Data[,flds[[i]]])

          f.tilda=matrix(NA,n,Rep)

          Ym=matrix(NA,n,N)
          for(s in 1:N){
            Y=matrix(0,n,1)
            for(j in 1:m){
              Y=Y+(e.val.z[j]/(e.val.z[j]+phi[I]))*
                (as.numeric(t(xg[,s]))%*%e.vec.z[,j])*e.vec.z[,j]
            }
            Ym[,s]=Y
          }

          f=rowMeans(x = Ym)

###

          MM=matrix(NA,N,1)
          for(j in 1:N){
            MM[j,1]=sqrt(pracma::trapz(grid,xg[,j]^2))
          }
          MAX=max(MM)


### calculating delta

          Delta2=((4*MAX^2)/(N^2))*(sum(e.val.z/((e.val.z+phi[I])^2)))
          delta=sqrt(((2*log(2/beta))/(alpha^2))*(Delta2))

### generating f.tilda
          Q=0
          for(l in 1:Rep){
            Z=matrix(0,nrow = n,ncol = 1)
            for(q in 1:m){
              Z=Z+sqrt(e.val.z[q])*rnorm(1)*e.vec.z[,q]
            }
            f.tilda[,l]=f+delta*Z
            Q=Q+sqrt(pracma::trapz(grid,(f.tilda[,l]-f.hat)^2))
          }
          MF.P[i,1]=Q/Rep
        }

        CV.P[I,J]=mean(MF.P)

      }

### Print time

      if(count==0){

        Time2=Sys.time()
        count=1
        estimated.time=round((length(phi)*length(ro)-1)*(Time2-Time1))

        if(print.estimated.time==TRUE){
          cat("Estimation of remaining time is",round((length(phi)*length(ro)-1)*(Time2-Time1)),"seconds \n")

        }
      }

##

    }
  }

###

  if(cv.penalty=="Regular"){
    arg.min.cv= which(CV == min(CV), arr.ind=TRUE)

    if(print.cv==TRUE){
      cat("\n \n CV matrix for each phi and ro is \n \n")
      print(CV)
      cat("CV is gotten from", "row = ",arg.min.cv[1],", col = ",arg.min.cv[2],"\n \n")
      cat("CV is",CV[arg.min.cv[1],arg.min.cv[2]],"\n \n")
      cat("phi.cv =",phi[arg.min.cv[1]],", ro.cv =",ro[arg.min.cv[2]],"\n \n")
    }

    out=matrix(NA,1,3)
    colnames(out)=c("phi","ro","Estimation time")
    out=list("par"=c(phi[arg.min.cv[1]],ro[arg.min.cv[2]]),"time"=estimated.time)
    return(out)

  }

###

  if(cv.penalty=="Irregular"){
    arg.min.cv.p= which(CV.P == min(CV.P), arr.ind=TRUE)

    if(print.cv==TRUE){
      cat("CV matrix for each phi and ro is \n \n")
      print(CV.P)
      cat(" \n \nCV is gotten from", "row = ",arg.min.cv.p[1],", col = ",arg.min.cv.p[2],"\n \n" )
      cat("CV is",CV.P[arg.min.cv.p[1],arg.min.cv.p[2]],"\n \n")
      cat("phi.cv =",phi[arg.min.cv.p[1]],", ro.cv =",ro[arg.min.cv.p[2]],"\n \n")
    }

    out=matrix(NA,1,3)
    colnames(out)=c("phi","ro","Estimation time")
    out=list("par"=c(phi[arg.min.cv.p[1]],ro[arg.min.cv.p[2]]),"time"=estimated.time)
    return(out)
  }


}
# A=CV(Data = Data,phi = c(0.001,0.01),ro=c(0.01,1))
