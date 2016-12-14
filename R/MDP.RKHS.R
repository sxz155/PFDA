# unlink("", recursive = TRUE)
#' Releasing four Differential Private RKHS smoothing means of a dataset
#'
#' This function create four DP RKHS smoothing means from an existing data set
#' based on any arbitrary 4 by 1 vectors for \code{alpha}, \code{beta}, \code{kernel},
#' \code{phi} and \code{ro}.
#' @param Data a matrix which the of interest curves are located in columns
#' @param grid grid (x-axis) for each curve, default is equally espaced between 0 and 1.
#' @param alpha,beta vector of privacy parameters
#' @param phi vector of penalty parameters
#' @param ro vector of kernel range parameters
#' @param drop.col TRUE/FALSE for cleaning the Data, deleting Columns/Rows including missing values
#' @return Multiple \link{DP.RKHS} output each of them includes "f.tilda","delta","f", "grid" and "X"
#' @export
MDP.RKHS=function(grid,Data,alpha=rep(1,1),beta=rep(0.1,1),
                 kernel,phi,ro,col.drop=TRUE){

  M=length(alpha)
  if(missing(kernel)){kernel=rep("Gau",M)}
  if(missing(phi)){phi=rep(0.01,M)}
  if(missing(ro)){ro=rep(0.2,M)}

### Grid

  if(missing(grid)){
    grid=seq(0,1,length.out = dim(Data)[1])
    n=length(grid)
  }
  else{
    grid=(as.numeric(grid)-min(as.numeric(grid)))/diff(range(as.numeric(grid)))
    n=length(grid)
  }

###

 if(length(Reduce(intersect,list(length(alpha),length(beta),
                                 length(kernel),length(phi),length(ro))))==1){

  A=(as.list(rep(NA,M)))
for(i in 1:M){
     A[[i]]=DP.RKHS(grid=grid,Data=Data,alpha =alpha[i],beta = beta[i],kernel=kernel[i],
                    phi=phi[i],ro=ro[i],col.drop=col.drop)
}
  names(A)=c(paste("DP", 1:M, sep = ""))
  return(A)

}
  else{
    stop("length of the vectors are not equal")
  }
  }

# Y=MDP.RKHS(Data = Data,alpha=rep(1,4),beta = rep(0.1,4))
# str(Y)
