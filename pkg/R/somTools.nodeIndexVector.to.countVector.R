#from somTools
#Copyright 2012 David H. Brown
#GPL license available
##' somTools.nodeIndexVector.to.countVector
##'
##' Given a vector containing any number of nodeIndex values in any order,
##' returns a vector with somTools@xdim*somTools@ydim values, containing
##' the frequency with which the nodeIndex at that position appeared in vec
##'
##' @param somTools somTools
##' @param vec numeric
##'
##' Yes, this is a really simply function, but inlined, its purpose is far 
##' from clear.
somTools.nodeIndexVector.to.countVector <- function(somTools, vec) {
  tabulate(vec,nbins=somTools@xdim*somTools@ydim)
}