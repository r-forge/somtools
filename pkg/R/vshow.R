#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#' verbose show for debugging
vshow <- function(something,description="{something}") {
  cat(sprintf(">>> %s is a %s:\n",description,class(something)))
  show(something)
}
