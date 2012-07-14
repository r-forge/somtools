#from somTools
#Copyright 2012 David H. Brown
#GPL license available
##' calculate a bounding box of the given matrix
##' or list of matrices with equal ncol (in which case, na.rm=TRUE)

bounds <- function(coord.matrix) {
  if(is.list(coord.matrix)) {
  bounds.matrix <- matrix(NA,
   nrow=2, ncol=ncol(coord.matrix[[1]]),
   dimnames=list(c('min','max'),colnames(coord.matrix[[1]]))
   )
    for(l in 1:length(coord.matrix)) {
      tmp <- bounds(coord.matrix[[l]])
      bounds.matrix['min',]
      for (i in 1:min(ncol(bounds.matrix),ncol(coord.matrix[[l]]))) {
      bounds.matrix['min',i]=min(bounds.matrix['min',i],tmp[,i], na.rm=TRUE)
      bounds.matrix['max',i]=max(bounds.matrix['max',i],tmp[,i], na.rm=TRUE)
      } #for i columns
    } #for l items in list
    return(bounds.matrix)
  }#if handling a list. Otherwise...

  if(!is.matrix(coord.matrix)) {
    stop('bounds(coord.matrix) works with matrices (or a list of matrices with equal ncol)')
    }
  bounds.matrix <- matrix(0,
   nrow=2, ncol=ncol(coord.matrix),
   dimnames=list(c('min','max'),colnames(coord.matrix))
   )
for (i in 1:ncol(coord.matrix)) {
  bounds.matrix['min',i]=min(coord.matrix[,i])
  bounds.matrix['max',i]=max(coord.matrix[,i])
  }
  bounds.matrix
}