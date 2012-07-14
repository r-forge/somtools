#from somTools
#Copyright 2012 David H. Brown
#GPL license available
##' enlarge a bounding box as calculated by bounds()
##' by the proportion indicated by amount
##'
##' amount may be a single value or a vector

expand.bounds <- function(bbox, amount=0.02) {
  expansion <- (bbox[2,]-bbox[1,])*amount/2
  bbox[1,] <- bbox[1,] - expansion
  bbox[2,] <- bbox[2,] + expansion
  bbox
}