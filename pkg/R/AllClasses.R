#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#' Class definition for somTools
#' 
#'
setClass("somTools",
  representation(
    topol = "character",
    toroidal = "logical",
    xdim = "numeric",
    ydim = "numeric",
    #whatever the interfacing function says
    data = "ANY",
    #value in n-space at each node
    codes = "matrix",
    scalecodes = "matrix", #a saved copy of scale(codes)
    #list of nodeIdexes for each observation
    #ANY becuase this is numeric or NULL
    datacode = "ANY", #like kohonen$unit.classif
    
    #' original.class records the type of SOM that initialized this somTools
    original.class = "character"
    ) #representation
    , 
    #validity parameter creates the validObject() method
    validity=function(object) {
  messages <- NULL #by default

  #simple known problems:
  if (is.null(object@codes)) {
    messages<-c(messages, "Codes not known (@codes) -- has the map been trained?")
    }
  if (is.null(object@topol)) {
    messages<-c(messages, "Topology not known (@topol)")
    }
  if (is.null(object@original.class)) {
    messages<-c(messages, "Original class not known (@original.class)")
    }
  if (object@xdim < 1) {
    messages<-c(messages, "X-dimension must be at least 1 (@xdim)")
    }
  if (object@ydim < 1) {
    messages<-c(messages, "Y-dimension must be at least 1 (@ydim)")
      }
  #did we pass all the tests?
  if (is.null(messages)) { TRUE }
  #or do we have problems?
  else { messages }
} #validity function
  ) #setClass
