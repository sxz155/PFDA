#unlink("/Library/Frameworks/R.framework/Versions/3.3/Resources/library/PFDA", recursive = TRUE)
#' Plotting 4 RKHS smoothing mean and its DP version simultaneously
#'
#' This function plots 4 different figures which the original data, the RKHS smoothing mean and its differential private
#' version are simply including in each of them
#' @param DP output of \link{DP.RKHS} or \link{DP.Sim} list
#' @export
plotMDP=function(MDP,text.main,
              legend.cex,legend.loc,seg.lin=0.75,
              text.font=2,text.width=0.15,legend.size=0.57,xlab="", ylab="",extra_range,
              lty.p=3,lwd.p=3,par){

  M=length(MDP)
  if(missing(text.main)){text.main=rep("",M)}
  if(missing(legend.cex)){legend.cex=rep(1.4,M)}
  if(missing(legend.loc)){legend.loc=rep("topleft",M)}
  if(missing(extra_range)){extra_range=matrix(0,M,2)}


  if(missing(par)){
    par.row=floor(sqrt(M))
    par.col=floor((M+par.row-1)/par.row)
    par=c(par.row,par.col)
  }
  else{
    par.row=par[1]
    par.col=par[2]
  }

  if(length(Reduce(intersect,list(length(text.main),length(legend.cex),length(legend.loc))))==1){
  par(mfrow=c(par.row,par.col),font=2,cex=legend.size)
  for(i in 1:M){
    plotDP(DP=MDP[[i]],
       text.main=text.main[i],legend.cex=legend.cex[i],legend.loc=legend.loc[i],seg.lin=seg.lin,
       text.font=text.font,text.width=text.width,xlab=xlab,ylab=ylab,extra_range=extra_range[i,],
       lty.p=lty.p,lwd.p=lwd.p)
  }
  }
    else{
      stop("length of the vectors are not equal")
    }
  }

# library(fda)
# library(refund)
# Data=growth$hgtf
# Data=DTI$cca
# Data=t(Data)
# dim(Data)
# AA=MDP.RKHS(Data = Data,alpha = rep(1,3),beta = rep(0.1,3))
# str(AA)
# plotMDP(MDP = AA)
# str(MDP.Sim())
