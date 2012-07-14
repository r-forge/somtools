#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#' returns a single-column matrix
#' row name is from the input newdata;
#' column name is "nodeIndex"
#'
somTools.nearest.node <- function(somTools, newdata, distfun = somTools.nearest.node.distfun.Euclidian, scale.data) {
#check parameters:
  newdata <- somTools.newdata.massage(somTools, newdata)
  if(identical(FALSE,newdata)) {
    stop("somTools@codes and newdata must match; see warnings.")
  }

#@todo: would be nice to be able to check with the original SOM whether to scale
#and use that as the default. In case you're wondering why I'm not putting the
#default value in the formals.
  if(missing(scale.data)) { scale.data <- FALSE }

#first, find distances to all nodes
if(scale.data) {
  dst <- t(apply(somTools.scale.new.data(somTools,newdata),1,function(nd) {
   apply(somTools@scalecodes,1,function(y) {sqrt(sum((y-nd)^2))})
  }))
  } else { #not scale.data
  dst <- t(apply(newdata,1,function(a) {
    apply(somTools@codes,1,function(b) {distfun(a, b)})
  }))
  } #elseif
  
  w <- apply(dst,1,function(x) {which(x==min(x))})
  matrix(w, dimnames=list(attr(w,"names"),'nodeIndex'), byrow=TRUE, ncol=1)
} #f somTools.nearest.node

somTools.nearest.node.distfun.Euclidian <- function(a,b) { sqrt(sum((b-a)^2))}