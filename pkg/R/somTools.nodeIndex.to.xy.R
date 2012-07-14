#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#' Returns a matrix where rownames are the node.index and
#' column names are 'x' and 'y'
somTools.nodeIndex.to.xy <- function (somTools, node.index) {
if(!is(somTools, "somTools")) {
    stop("Must have a somTools as first argument.")
  }
  
  if(missing(node.index)) node.index <- (1:(somTools@xdim*somTools@ydim))

  #ensure that we're looking for whole numbers and don't bother doing any twice
  #no; this is a problem because we don't get back the same number of entries
  #node.index <- unique(floor(node.index))
  node.index <- floor(node.index)
  #if doing this more than once per node proves an optimization issue,
  #let it be handled by the caller

  #check bounds
    if(any(node.index < 1)) {
      warning("Node indexes begin at 1; one or more is less than 1")
      }
    if (any(node.index > somTools@xdim*somTools@ydim)) {
      warning("At least one node.index is too large.")
      }
  
    x <- (node.index-1) %% somTools@xdim
    y <- (node.index-1) %/% somTools@xdim

  matrix(c(x,y), ncol=2, byrow=FALSE, dimnames = list(node.index, c("x","y")))
}