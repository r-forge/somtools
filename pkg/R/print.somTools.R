#from somTools
#Copyright 2012 David H. Brown
#GPL license available
print.somTools <- function(x,...) {
if (!class(x) == "somTools") {
    stop("argument 'x' must be an object of class 'somTools'")
      } 
    cat(sprintf("\nA Generic Self-Organizing Map interface to a %s object.", x@original.class))
    cat(sprintf("\nThe map consists of %d %dD nodes arranged in a %dx%d %s%s grid."
      , attr(x@codes, "dim")[1] #how many nodes
      , attr(x@codes, "dim")[2] #how many features (dimensions)
      , x@xdim, x@ydim #grid dimensions
      , ifelse(x@toroidal,"toroidal ","") #toroidal?
      , ifelse(x@topol=="rect","rectangular", ifelse(x@topol=="hexa","hexagonal","unknown"))
      )) #sprintf cat map description.
    cat(sprintf("\nThe map was trained on %sdata which %s been retained."
      , ifelse(length(x@datacode),sprintf("%d ",length(x@datacode)),"") #number of training data
      , ifelse(0==length(x@data),"have not","have") #whether we have trainigndata
      )) #sprintf cat: training description
    cat("\n") #put the prompt back on its own line
} #function print.somTools