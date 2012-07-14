#from somTools
#Copyright 2012 David H. Brown
#GPL license available
##' somTools.map.density
##'
##' returns a vector of data count for each grid cell
##' if newdata is provided, its nearest nodes will be calculated
##' otherwise, the datacode or data values stored in the somTools
##' will be used.
##'
##' @param somTools somTools
##' @param newdata numeric
##' @param scalefun function

somTools.map.density <- function(somTools, newdata, scalefun = function(vec) {vec}) {
    if(!is(somTools, "somTools")) {
    stop("somTools.map.density must have a somTools as first argument.")
    }
#if newdata is missing, we can try to use training data from somTools@datacode or @data
if(missing(newdata)) {
    if(is.null(somTools@datacode)) {
      if(is.null(somTools@data)) { #neither data nor datacode
        stop("This somTools does not have either data nor datacode; cannot infer density; you must specify newdata")
        } else { #we do have data but no datacode
          nearest <- somTools.nearest.node(somTools, somTools@data)
          } #else we do have data
      } else { #we do have datacode
        nearest <- somTools@datacode
        } #else we do have datacode
  } else { #we do have newdata
  newdata <- somTools.newdata.massage(somTools, newdata)
  if(identical(FALSE,newdata)) {
    stop("somTools@codes and newdata must match; see warnings.")
  }
  nearest <- somTools.nearest.node(somTools, newdata)
} #else we do have newdata

#at this point, all we want is the vector tab, with one density value per nodeIndex
somTools.nodeIndexVector.to.countVector(somTools, nearest)
}