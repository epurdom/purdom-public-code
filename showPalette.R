#' @title showPalette
#' @details \code{showPalette}
#'   creates plot that prints the index of each color next to a display of the color. 
#' @param cex numeric value giving the cex for the text of the plot.
#' @param colPalette a vector of character colors. By default, the palette 
#'   \code{bigPalette} is used
#' @param which numeric. Which colors to plot. Must be a numeric vector with
#'   values between 1 and length of \code{colPalette}. If missing, all colors
#'   plotted.
#' @details \code{showPalette} will plot the \code{colPalette} colors with their
#'   labels and index.
#'
#' @examples
#' showPalette(bigPalette)
#' showPalette(massivePalette,cex=0.6)


showPalette<-function(colPalette,which=NULL,cex=1){
  oldPar<-par(no.readonly = TRUE)
  wh<-which
  if(is.null(wh)){
    wh<-seq_along(colPalette)
  }
  else{ colPalette<-colPalette[wh]}
  n<-ceiling(sqrt(length(colPalette)))
  nblank<-n^2-length(colPalette)
  xwide<-n
  yup<-n
  x1<-rep(c(seq_len(xwide))-.5,yup)
  x2<-rep(c(seq_len(xwide))+.5,yup)
  xtext<-rep(c(seq_len(xwide)),yup)
  ycolor1<-rep(seq(1,yup*2,by=2)-.5,each=xwide)
  ycolor2<-rep(seq(1,yup*2,by=2)+.5,each=xwide)
  ytext<-rep(seq(2,yup*2,by=2)+.5,each=xwide)

  par(mar=c(0,0,0,0),omi=c(0,0,0,0))
  plot.new()
  plot.window(xlim=c(.5,xwide+.5),ylim=c(.5,(yup*2)+.5))
  rect(x1,ycolor1,x2,ycolor2,col=c(colPalette,rep("white",nblank)),border=FALSE)
  if(length(colPalette)>100){
	  half<-ceiling(length(colPalette)/2)
	 adj.text<-cbind(rep(.5,half*2),rep(c(0,1),times=half))
	 adj.text<-adj.text[seq_along(colPalette),]
  }
  else adj.text<-matrix(c(0.5,0),nrow=length(colPalette),ncol=2,byrow=TRUE)
  for(i in seq_along(colPalette)){
      text(xtext[i],ytext[i]-1,colPalette[i],cex=cex,adj=adj.text[i,])
      if(length(colPalette)<=100) text(xtext[i],ytext[i]-2,wh[i],cex=cex,adj=c(0.5,1))
  }
	par(oldPar)
}

