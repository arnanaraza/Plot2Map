## FUNCTION TO GRAPH SEPERATELY EACH CONTINENT

Binned <- function(x, y, caption = "", fname=""){
  intervals <- c(0:12*25, 7:8*50, Inf)
  
  ct <- findInterval(x, intervals, left.open=TRUE)
  r <- c(0,425)
  ux <- aggregate(x, list(ct), FUN=mean, na.rm=T)[,2]
  uy <- aggregate(y, list(ct), FUN=mean, na.rm=T)[,2]
  nu <- aggregate(y, list(ct), FUN=function(x) length(na.omit(x)))[,2]
  q1 <- aggregate(y, list(ct), FUN=quantile, probs=0.25, na.rm=T)[,2]
  q3 <- aggregate(y, list(ct), FUN=quantile, probs=0.75, na.rm=T)[,2]
  cx <- (2:7*0.25)[findInterval(nu, c(0,10,20,50,100,200))]

  # are there extremely high values on the x-axis?
  if(ux[length(ux)] > 400){
    ux[length(ux)] <- 500
    r <- c(0, 525)
  }
  
  setwd(outDir)
  
  png(fname, 1000, 1000, res=150)
  plot(ux,uy, las=1, main=caption, pch=16,cex=cx,
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r,xaxt="n")
  arrows(ux, q1, ux, q3, length=0)
  
  abline(0,1, lty=2, lwd=1)

  par(new=TRUE)

  t <- "#/bin"
  
  legend("topleft",bty="n",pch=16,ncol=2,pt.cex=2:7*0.25, 
         cex=1,c("< 10","10-20","20-50","50-100","100-200","> 200"),
         title=t)
  if(ux[length(ux)] == 500){
    axis(1,at=0:5*100, labels=c(0:4*100, ">400"))
    axis.break(breakpos=450)
  }
  else{axis(1,at=0:4*100, labels=c(0:4*100))
}
  dev.off()
  setwd(mainDir)
  
}




Scatter <- function(x, y, caption = "", fname=""){

  r <- c(0,1050)
  
  # are there extremely high values on the x-axis?
  chk <- sort(x) 
  if (chk[length(chk)] > 1000){
    r <- c(0, max(x, na.rm=T))}

  setwd(outDir)
  
  png(fname, 1000, 1000, res=150)
  plot(x,y, las=1, main=caption, pch=16,cex=0.5,
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r)
  abline(0,1, lty=2, lwd=1)

  dev.off()
  setwd(mainDir)
  
}
