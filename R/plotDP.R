# unlink("", recursive = TRUE)
#' Plotting RKHS smoothing mean and its DP version simultaneously
#'
#' This function plots the original data, the RKHS smoothing mean and its differential private
#' version simply in a same plot
#' @param DP output of \link{DP.RKHS} or \link{DP.Sim} list
plotDP=function(DP,text.main="",legend.cex=1.4,legend.loc="topleft",seg.lin=0.75,
                 text.font=2,text.width=0.15,xlab="",ylab="",
                 extra_range=c(0,0),lty.p=3,lwd.p=3,
                 legend.text=c("sample functions","RKHS mean", "DP RKHS mean"),
                 col=c("grey","green","red")){

  plot(y=DP$f,x=DP$grid,type="l",col=col[2],ylim=range(DP$X,DP$f.tilda)+extra_range,lwd=3,
       lty=2,ylab=ylab,xlab=xlab,main=text.main,cex.lab=2,cex.main=2.4,cex.axis=1.5)

  legend(x=legend.loc,legend=legend.text,
         col=col,lty=c(1,1,lty.p),lwd=c(1,3,lwd.p),cex=legend.cex,bty="n",
         seg.len=seg.lin,text.font = text.font,text.width = text.width)

  for(i in 1:dim(DP$X)[2]){
    points(y=DP$X[,i],x=DP$grid,type="l",col=col[1],lwd=1,lty=1)

  }

  points(y=DP$f,x=DP$grid,type="l",col=col[2],ylim=range(DP$f.tilda,DP$f),lwd=4,lty=1)
  points(y=DP$f.tilda,x=DP$grid,type="l",col=col[3],bty="n",lty=lty.p,lwd=lwd.p)

}
