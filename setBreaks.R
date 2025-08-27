#' @param breaks either vector of breaks, or number of breaks (integer) or a
#'   number between 0 and 1 indicating a quantile, between which evenly spaced
#'   breaks should be calculated. If missing or NA, will determine evenly spaced
#'   breaks in the range of the data.
#' @param makeSymmetric whether to make the range of the breaks symmetric around zero (only used if not all of the data is non-positive and not all of the data is non-negative)
#' @param returnBreaks logical as to whether to return the vector of breaks. See details.
#' @details if returnBreaks if FALSE, instead of returning the vector of breaks, the function will just return the second smallest and second largest value of the breaks. This is useful for alternatively just setting values of the data matrix larger than these values to this value if breaks was a percentile. This argument is only used if \code{breaks<1}, indicating truncating the breaks for large values of data.
#' @details \code{setBreaks} gives a set of breaks (of length 52) equally spaced
#'   between the boundaries of the data. If breaks is between 0 and 1, then the
#'   evenly spaced breaks are between these quantiles of the data.
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
