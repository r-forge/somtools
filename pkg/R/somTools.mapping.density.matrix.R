#from somTools
#Copyright 2012 David H. Brown
#GPL license available
##' somTools.mapping.density.matrix
##'
##' returns a matrix to be passed to the cartogram() function
##' if newdata is provided, its nearest nodes will be calculated
##' otherwise, the datacode or data values stored in the somTools
##' will be used.
##'
##' @param somTools somTools
##' @param newdata numeric
##' @param scalefun function
##'
##' Possibly deprecated in favor of using the sp method?

somTools.mapping.density.matrix <- function(somTools, newdata, scalefun = function(vec) {vec}) {
# tab is a vector that will hold the count/density for each nodeIndex
tab <- NULL
if(!is(somTools, "somTools")) {
    stop("somTools.density.matrix must have a somTools as first argument.")
}
#if newdata is missing, we can try to use somTools@datacode
if(missing(newdata)) {
    if(is.null(somTools@datacode)) {
      if(is.null(somTools@data)) { #neither data nor datacode
        stop("This somTools does not have either nor datacode; cannot infer density; you must specify newdata")
        } else { #we do have data but no datacode
          newdata <- somTools@data
          nearest <- somTools.nearest.node(somTools, newdata)
          tab <- somTools.nodeIndexVector.to.countVector(somTools, nearest)
          } #else we do have data
      } else { #we do have datacode
        #just to help avoid confusion later...
        tab <- somTools.nodeIndexVector.to.countVector(somTools, somTools@datacode)
        } #else we do have datacode
  } else { #we do have newdata

#check parameters:
  newdata <- somTools.newdata.massage(somTools, newdata)
  if(identical(FALSE,newdata)) {
    stop("somTools@codes and newdata must match; see warnings.")
  }
  nearest <- somTools.nearest.node(somTools, newdata)
  tab <- somTools.nodeIndexVector.to.countVector(somTools, nearest)
} #else we do have newdata

#at this point, all we want is the vector tab, with one density value per nodeIndex
matrix(as.numeric(scalefun(tab)),nrow=somTools@ydim,byrow=TRUE)
}