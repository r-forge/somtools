#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#' Generics for somTools class
#'

#' initialize a somTools; 
#' if passed a \link{som} or \link{kohonen} object,
#' will assign values to appropriate slots
setMethod("initialize","somTools",
  function(.Object,anysom,...) {
    if(! missing(anysom)) { 
      if (class(anysom) == "kohonen") {
        .Object@original.class = "kohonen"
        .Object@data <- anysom$data#might be empty
        .Object@topol <- substr(anysom$grid$topo,1,4)
        .Object@toroidal <- anysom$toroidal
        .Object@xdim <- anysom$grid$xdim
        .Object@ydim <- anysom$grid$ydim
        .Object@datacode <- anysom$unit.classif
        .Object@codes <- anysom$code
        .Object@scalecodes <- scale(.Object@codes)
      } #if kohonen

      if (class(anysom) == "som") {
        .Object@original.class = "som"
        .Object@topol <- anysom$topol
        .Object@toroidal <- FALSE
        .Object@data <- anysom$data #som::som always keeps data
        .Object@xdim <- anysom$xdim
        .Object@ydim <- anysom$ydim
        #som uses x+y*xdim; which starts at 0; our functions assume nodeIndex starts at 1:
        .Object@datacode <- (anysom$visual[,1]+(anysom$xdim * anysom$visual[,2]) + 1) 
        .Object@codes <- anysom$code
        .Object@scalecodes <- scale(.Object@codes)
  } #if som
## Add future classes of self-organizing map here

## If we did not find a class, anysom probably wasn't meant for us and should
## be passed to the next method?
if (is.null(.Object@original.class)) {
  classNextMethod(.Object,anysom,...)
  } else {
       callNextMethod(.Object,...)
    }
  .Object #return value if we have an anysom
  } #if anysom not missing
  else {callNextMethod(.Object,...)}
  } #function
) #setMethod initialize


#' \link{print}ing a somTools is handled by \link{print.somTools}
setMethod("print","somTools",
  function(x,...) {
    print.somTools(x, ...)
    } #function
  ) #setMethod print

#' \link{show}ing a somTools is handled by \link{print.somTools}
setMethod("show","somTools",
  function(object) {
    print.somTools(object)
    } #function
  ) #setMethod show
