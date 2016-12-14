# unlink("", recursive = TRUE)
#' Generating RKHS smoothing mean of a dataset
#'
#' This function create a RKHS smoothing mean from an existing data set
#' with known eigenvalues and eigenvectors
#' @param N real number, number of observations
#' @param n real number, number of grid points
#' @param e.val.x real vector n*1, eigenvalues
#' @param e.vec.x real valued matrix n*N, eigenvectors
#' @param tau range of the uniform distribution in KL expansion
#' @param phi real number, penalty parameter
#' @param mu real vector n*1, initial mean vector
#' @param m positive integer, number of eigenvectors are going to be used
#' @param e.val.z real valued matrix n*N, eigenvectors of noise
#' @return a real valued matrix n*N
#' @export g.X.RKHS
g.X.RKHS=function(N,n,e.val.x,e.vec.x,tau,phi,mu,m,e.val.z){

  Ym=matrix(NA,n,N)
  for(s in 1:N){
    Y=matrix(0,n,1)
    for(i in 1:m){
      Y=Y+(e.val.z[i]/(e.val.z[i]+phi))*
        (t(mu)%*%e.vec.x[,i]+sqrt(e.val.x[i])*runif(1,min = -tau,max = tau))*e.vec.x[,i]
    }
    Ym[,s]=Y
  }
  return(Ym)
}
#devtools::document()
