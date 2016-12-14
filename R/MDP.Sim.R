# unlink("/Library/Frameworks/R.framework/Versions/3.2/Resources/library/PFDA", recursive = TRUE)
#' Releasing 4 Differential Private RKHS smoothing means of a simulated dataset
#'
#' This function create 4 DP RKHS smoothing means from an existing data set
#' with known eigenvalues and eigenvectors
#' @param alpha,beta Privacy parameters, real numbers
#' @param N real vector 4*1, number of observations
#' @param n real vector 4*1, number of grid points
#' @param e.val.x real vector n*1, eigenvalues
#' @param e.vec.x real valued matrix n*N, eigenvectors
#' @param tau range of the uniform distribution in KL expansion
#' @param phi real number, penalty parameter
#' @param mu real vector n*1, initial mean vector
#' @param e.val.z real valued matrix n*N, eigenvectors of noise
#' @param pow smoothing parameter, e.val.x_{i}=i^{-pow}
#' @param ro range parameter in kernel, real number
#' @return four \link{DP_Sim} output each of them includin "f.tilda","delta","f" and "X"
#' @export MDP.Sim
MDP.Sim=function(alpha=rep(1,1),beta=rep(0.1,1),
              kernel,phi,ro,
              n,N,tau,case,pow,mu){

  M=length(alpha)
  if(missing(kernel)){kernel=rep("Gau",M)}
  if(missing(phi)){phi=rep(0.01,M)}
  if(missing(ro)){ro=rep(0.2,M)}
  if(missing(n)){n=rep(100,M)}
  if(missing(N)){N=rep(25,M)}
  if(missing(tau)){tau=rep(0.4,M)}
  if(missing(case)){case=rep(2,M)}
  if(missing(pow)){pow=rep(4,M)}
  if(missing(mu)){mu=matrix(rep(0.1*sin(2*pi*seq(0,1,length=n[1])),M),ncol = M)}


  if(length(Reduce(intersect,list(length(alpha),length(beta),length(kernel),
                                length(n),length(N),
                                length(phi),length(ro),length(tau),
                                length(case),length(pow),dim(mu)[2])))==1){

  A=(as.list(rep(NA,M)))
  for(i in 1:M){
    A[[i]]=DP.Sim(alpha =alpha[i],beta = beta[i],kernel=kernel[i],phi=phi[i],ro=ro[i],
              n=n[i],N=N[i],tau=tau[i],case=case[i],pow=pow[i],mu=mu[,i])
  }
  names(A)=c(paste("DP", 1:M, sep = ""))
  return(A)
}
else{
  stop("length of the vectors are not equal")
}
}

# S=MDP.Sim(alpha = c(5,1,0.1),beta=c(1,0.1,0.1))
# str(S)
# plotMDP(MDP = S)
