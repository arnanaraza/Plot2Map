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

## FUNCTION TO COMPARE TWO BINNED PLOTS

TwoPlots <- function(x, y, x1, y1, caption = "", fname="", title=""){
  intervals <- c(0:12*25, 7:12*50, Inf)
  x <- ifelse(x > 350, 400, x)
  
  col1 <- rgb(red = 1, green = 0, blue = 0, alpha = 0.5)
  col2 <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
  
  ct <- findInterval(x, intervals, left.open=TRUE)
  r <- c(0, 1.1 * max(x, na.rm=T))
  ux <- aggregate(x, list(ct), FUN=mean, na.rm=T)[,2]
  uy <- aggregate(y, list(ct), FUN=mean, na.rm=T)[,2]
  nu <- aggregate(y, list(ct), FUN=function(x) length(na.omit(x)))[,2]
  q1 <- aggregate(y, list(ct), FUN=quantile, probs=0.25, na.rm=T)[,2]
  q3 <- aggregate(y, list(ct), FUN=quantile, probs=0.75, na.rm=T)[,2]
  cx <- (2:7*0.25)[findInterval(nu, c(0,10,20,50,100,200))]
  
  # are there extremely high values on the x-axis?
  if(ux[length(ux)] == 400){
    r <- c(0,425)
  }else{
    r <- c(0,225)
  }
  
  setwd(outDir)
  
  png(fname, 1000, 1000, res=150)
  plot(ux,uy, las=1, main=caption, pch=16, col= col1,cex=cx,
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r,xaxt="n")
  arrows(ux, q1, ux, q3, length=0, col = col1)
  
  x1 <- ifelse(x1 > 350, 400, x1)
  
  ct1 <- findInterval(x1, intervals, left.open=TRUE)
  ux1 <- aggregate(x1, list(ct1), FUN=mean, na.rm=T)[,2]
  uy1 <- aggregate(y1, list(ct1), FUN=mean, na.rm=T)[,2]
  nu1 <- aggregate(y1, list(ct1), FUN=function(x1) length(na.omit(x1)))[,2]
  q11 <- aggregate(y1, list(ct1), FUN=quantile, probs=0.25, na.rm=T)[,2]
  q33 <- aggregate(y1, list(ct1), FUN=quantile, probs=0.75, na.rm=T)[,2]
  cx1 <- (2:7*0.25)[findInterval(nu1, c(0,10,20,50,100,200))]
  
  par(new=TRUE)
  plot(ux1,uy1, las=1, main=caption, pch=16, col= col2, cex=cx1,
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r,xaxt="n")
  arrows(ux1, q11, ux1, q33, length=0,col= col2)
  
  abline(0,1, lty=2, lwd=1)
  
  if (title == 'harmo'){
    t <- "#/bin, original"}
  else{t <- "#/bin, unweighted"}
  
  legend("topleft",bty="n",pch=16,ncol=2,pt.cex=2:7*0.25,col=col1,
         cex=1,c("< 10","10-20","20-50","50-100","100-200","> 200"),
         title=t)
  par(new=TRUE)
  
  if (title == 'harmo'){
    t <- "#/bin, harmonized"
  }else{t <- "#/bin, weighted"}
  
  legend("top",bty="n",pch=16,ncol=2,pt.cex=2:7*0.25, col= col2,
         cex=1,c("< 10","10-20","20-50","50-100","100-200","> 200"),
         title=t)
  
  if(ux[length(ux)] == 400){
    axis(1,at=0:4*100, labels=c(0:3*100, ">300"))
    axis.break(breakpos=350)
  }
  else{axis(1,at=0:3*100, labels=c(0:3*100))
  }
  
  dev.off()
  setwd(mainDir)
  
}

