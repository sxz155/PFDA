# unlink("", recursive = TRUE)
#' g.X: Generating data for simulation
#'
#' This function simulate a data set by KL expansion
#' with using a known values for eigenvalues and
#' eigenvectors
#' @param N real number, number of observations
#' @param n real number, number of grid points
#' @param e.val.x real vector n*1, eigenvalues
#' @param e.vec.x real valued matrix n*N, eigenvectors
#' @param tau range of the uniform distribution in KL expansion
#' @param mu real vector n*1, initial mean vector
#' @param m positive integer, number of eigenvectors are going to be used
#' @return a real valued matrix n*N
#' @examples g.X(N=100,n=50,e.val.x=seq(1,0.1,length=50))
#' @export g.X
g.X=function(N,n,e.val.x,e.vec.x,tau,mu,m){
  Xm=matrix(NA,n,N)
  for(s in 1:N){
    X=matrix(0,n,1)
    for(i in 1:m){
      X=X+sqrt(e.val.x[i])*runif(1,min = -tau,max = tau)*e.vec.x[,i]
    }
    Xm[,s]=X+mu
  }
  return(Xm)
}

