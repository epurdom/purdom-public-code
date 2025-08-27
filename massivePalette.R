#' @details \code{massivePalette} is a combination of \code{bigPalette} and the
#'   non-grey colors of \code{colors}, resulting in a list of colors of 
#'   length 487. 
#'   \code{massivePalette} is mainly useful for when working with a very large 
#'   number of clusters to guarantee that the code doesn't run out of colors. 
#'   However, many of the colors beyond those defined in \code{bigPalette} 
#'   will be very similar to each other (though randomly ordered).

.rcolors<-function(){
  #so sure that setting seed doesn't mess up when installing package
  set.seed(23589)
  return(sample(colors()[-c(152:361)]))
}

massivePalette<-unique(c(bigPalette,.rcolors()))