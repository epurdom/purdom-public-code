#' @title showPalette
#' @details \code{bigPalette} is a long palette of colors (length 58) used by 
#'   \code{\link{plotClusters}} and accompanying functions. \code{showPalette}
#'   creates plot that gives index of each color in a vector of colors.
#'   \code{massivePalette} is a combination of \code{bigPalette} and the
#'   non-grey colors of \code{\link{colors}()} (length 487). 
#'   \code{massivePalette} is mainly useful for when doing
#'   \code{\link{plotClusters}} of a very large number of clusterings, each with
#'   many clusters, so that the code doesn't run out of colors. However, many of
#'   the colors will be very similar to each other.
#' @param which numeric. Which colors to plot. Must be a numeric vector with
#'   values between 1 and length of \code{colPalette}. If missing, all colors
#'   plotted.
#' @param cex numeric value giving the cex for the text of the plot.
#' @param colPalette a vector of character colors. By default, the palette 
#'   \code{bigPalette} is used
#' @details \code{showPalette} will plot the \code{colPalette} colors with their
#'   labels and index.
#'
#' @export
#'
#' @examples
#' showPalette()
#' showPalette(massivePalette,cex=0.6)
showPalette<-function(colPalette=bigPalette,which=NULL,cex=1){
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

#' @rdname plottingFunctions
#' @export
bigPalette<-c(
	'#E31A1C',
	'#1F78B4',
	'#33A02C',
	'#FF7F00',
	'#6A3D9A',
	'#B15928',
	'#A6CEE3',
	'#bd18ea',
	'cyan',
	'#B2DF8A',
	'#FB9A99',
	"deeppink4",
	'#00B3FFFF',
	'#CAB2D6',
	'#FFFF99',
	'#05188a',
	'#CCFF00FF',
	'cornflowerblue',
	'#f4cc03',
	'black',
	'blueviolet',
	'#4d0776',
	'maroon3',
	'blue',
#	'grey',
	'#E5D8BD',
	'cadetblue4',
	'#e5a25a',
	"lightblue1",
	'#F781BF',
	'#FC8D62',
	'#8DA0CB',
	'#E78AC3',
	'green3',
	'#E7298A',
	'burlywood3',
	'#A6D854',
	"firebrick",
	'#FFFFCC',
	"mediumpurple",
	'#1B9E77',
	'#FFD92F',
	'deepskyblue4',
	"yellow3",
	'#00FFB2FF',
	'#FDBF6F',
	'#FDCDAC',
	"gold3",
	'#F4CAE4',
	'#E6F5C9',
	'#FF00E6FF',
	'#7570B3',
	"goldenrod",
	'#85848f',
	"lightpink3",
	"olivedrab",
#	"plum",
#	"lightskyblue3",
#	"mediumturquoise",
	'cadetblue3'
)

#' @importFrom grDevices colors
.rcolors<-function(){
	#so sure that setting seed doesn't mess up when installing package
	set.seed(23589)
	return(sample(colors()[-c(152:361)]))
}

#' @rdname plottingFunctions
#' @export
massivePalette<-unique(c(bigPalette,.rcolors()))




#' @param breaks either vector of breaks, or number of breaks (integer) or a
#'   number between 0 and 1 indicating a quantile, between which evenly spaced
#'   breaks should be calculated. If missing or NA, will determine evenly spaced
#'   breaks in the range of the data.
#' @param makeSymmetric whether to make the range of the breaks symmetric around zero (only used if not all of the data is non-positive and not all of the data is non-negative)
#' @param returnBreaks logical as to whether to return the vector of breaks. See details.
#' @details if returnBreaks if FALSE, instead of returning the vector of breaks, the function will just return the second smallest and second largest value of the breaks. This is useful for alternatively just setting values of the data matrix larger than these values to this value if breaks was a percentile. This argument is only used if \code{breaks<1}, indicating truncating the breaks for large values of data.
#' @rdname plottingFunctions
#' @details \code{setBreaks} gives a set of breaks (of length 52) equally spaced
#'   between the boundaries of the data. If breaks is between 0 and 1, then the
#'   evenly spaced breaks are between these quantiles of the data.
#' @export
#' @examples
#' setBreaks(data=simData,breaks=.9)
setBreaks<-function(data,breaks=NA,makeSymmetric=FALSE,returnBreaks=TRUE){
  if(all(is.na(data))) stop("data consists only of NA values")
  if(length(unique(na.omit(as.vector(data))))==1){
    warning("data consists of only a single non NA value")
    val<-unique(na.omit(as.vector(data)))
    return(seq(val-1,val+1,length=52))
  }
  isPositive<-all(na.omit(as.vector(data))>=0)
  isNegative<-all(na.omit(as.vector(data))<=0)
  #
  #get arround bug in aheatmap
  #if colors are given, then get back 51 colors, unless give RColorBrewer, in which case get 101! Assume user has to give palette. So breaks has to be +1 of that length
  #TO DO: might not need any more with updated aheatmap.
  ncols<-51
  minData<-min(data,na.rm=TRUE)
  maxData<-max(data,na.rm=TRUE)
  maxAbsData<-max(abs(data),na.rm=TRUE)
  if(!is.vector(breaks)) stop("breaks argument must be a vector")
  if(missing(breaks) || is.na(breaks)){
    #go from minimum to max
    if(makeSymmetric & !isPositive & !isNegative){
      breaks<-seq(-maxAbsData,maxAbsData,length=ncols+1)
      seconds<-c(-maxAbsData,maxAbsData)
    }
    else{
      breaks<-seq(minData,maxData,length=ncols+1)
      seconds<-c(minData,maxData)
    }
    
  }
  else if(length(breaks)>0 && !is.na(breaks)){
    if(length(breaks)==1){
      if(breaks<1){
        if(breaks<0.5) breaks<-1-breaks
        #
        uppQ<-if(isPositive) quantile(data[which(data>0)],breaks,na.rm=TRUE) else quantile(data,breaks,na.rm=TRUE)
        lowQ<-if(isPositive) min(data,na.rm=TRUE) else quantile(data,1-breaks,na.rm=TRUE)
        
        if(makeSymmetric & !isPositive & !isNegative){
          #make breaks symmetric around 0
          absq<-max(abs(c(lowQ,uppQ)),na.rm=TRUE)
          absm<-max(abs(c(min(data,na.rm=TRUE),max(data,na.rm=TRUE))))
          #is largest quantile also max of abs(data)?
          quantAllMax <- if( isTRUE( all.equal(round(absq,5), round(absm,5)))) TRUE else FALSE
          if(!quantAllMax){
            breaks <- c(-absm, seq(-absq,absq,length=ncols-1), absm)
            seconds<-c(-absq,absq)
          }
          else{
            #equally spaced
            breaks <- seq(-absm,absm,length=ncols+1)
            seconds<-c(-absm,absm)
          }
        }
        else{
          #determine if those quantiles are min/max of data
          quantMin <- if( isTRUE( all.equal(round(lowQ,5), round(minData,5)))) TRUE else FALSE
          quantMax<-if( isTRUE( all.equal(round(uppQ,5),round(maxData,5)))) TRUE else FALSE
          
          if(!quantMin & !quantMax){
            breaks <- c(minData, seq(lowQ,uppQ,length=ncols-1), maxData)
            seconds<-c(lowQ,uppQ)
          }
          if(!quantMin & quantMax){
            breaks <- c(minData, seq(lowQ,maxData,length=ncols))
            seconds<-c(lowQ,maxData)
          }
          if(quantMin & !quantMax){
            breaks <- c(seq(minData,uppQ,length=ncols), maxData)
            seconds<-c(minData,uppQ)
          }
          if(quantMin & quantMax){
            breaks<-seq(minData,maxData,length=ncols+1)
            seconds<-c(minData,maxData)
          }
        }
      }
      else{ #breaks is number of breaks
        if(length(breaks)!=52) warning("Because of bug in aheatmap, breaks should be of length 52 -- otherwise the entire spectrum of colors will not be used")
        if(makeSymmetric& !isPositive & !isNegative){
          breaks<-seq(-maxAbsData,maxAbsData,length=breaks)
          seconds<-c(-maxAbsData,maxAbsData)
        }
        else{
          breaks<-seq(minData,maxData,length=breaks)
          seconds<-c(minData,maxData)
        }
      }
    }
  }
  if(!length(unique(breaks))==length(breaks)){
    breaks<-sort(unique(breaks))
    warning("setBreaks did not create unique breaks, and therefore the resulting breaks will not be of exactly the length requested. If you have replicable code that can recreate this warning, we would appreciate submitting it to the issues tracker on github (existing issue: #186)")
    seconds<-c(min(breaks),max(breaks))
  }
  if(returnBreaks) return(breaks)
  else return(seconds)
  
}



#' @rdname plottingFunctions
#'
#' @details \code{seqPal1}-\code{seqPal4} are palettes for the heatmap.
#'   \code{showHeatmapPalettes} will show you these palettes.
#'
#' @export
#'
#' @examples
#'
#' #show the palette colors
#' showHeatmapPalettes()
#'
#' #compare the palettes on heatmap
#' cl <- clusterSingle(simData, subsample=FALSE,
#' sequential=FALSE, 
#' mainClusterArgs=list(clusterFunction="pam", clusterArgs=list(k=8)))
#'
#' \dontrun{
#' par(mfrow=c(2,3))
#' plotHeatmap(cl, colorScale=seqPal1, main="seqPal1")
#' plotHeatmap(cl, colorScale=seqPal2, main="seqPal2")
#' plotHeatmap(cl, colorScale=seqPal3, main="seqPal3")
#' plotHeatmap(cl, colorScale=seqPal4, main="seqPal4")
#' plotHeatmap(cl, colorScale=seqPal5, main="seqPal5")
#' par(mfrow=c(1,1))
#' }
#'
showHeatmapPalettes<-function(){
	palettesAll<-list(seqPal1=seqPal1,seqPal2=seqPal2,seqPal3=seqPal3,seqPal4=seqPal4,seqPal5=seqPal5)
	maxLength<-max(sapply(palettesAll,length))
	palettesAllAdj<-lapply(palettesAll,function(x){
		if(length(x)<maxLength) x<-c(x,rep("white",maxLength-length(x)))
			return(x)})
	ll<-list()
	sapply(seq_along(palettesAllAdj),function(ii){ll[[2*ii-1]]<<-palettesAllAdj[[ii]]})
	sapply(seq_len(length(palettesAllAdj)-1),function(ii){ll[[2*ii]]<<-rep("white",length=maxLength)})
	names(ll)[seq(1,length(ll),by=2)]<-names(palettesAll)
	names(ll)[seq(2,length(ll),by=2)]<-rep("",length(palettesAll)-1)
	mat<-do.call("cbind",ll)
	plotClusters(mat,input="colors")
}
