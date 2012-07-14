#from somTools
#Copyright 2012 David H. Brown
#GPL license available
## calcaulate the U-matrix of distances between nodes
## returns values according to the nodeIndex
##REIMPLEMENT TO USE stats::dist as in kohonen's
somTools.umat <- function(somTools, scale.data=FALSE) {
  #check parameters:
  if(!is(somTools, "somTools")) {
    warning("Must have a somTools as first argument.")
    return(FALSE)
  }

  if(scale.data) {
    sc <- somTools@scalecodes
  } else {
    sc <- somTools@codes
  }
  #we'll need all of them eventually, so calling adjacent.nodes with no 
  #indexes gives us the complete list.
  #can retrieve the adjacent nodeIndexes as as vector with
  #adjacentIndexes[[thisIndex]][,1]
  adjacentIndexes <- somTools.adjacent.nodes(somTools)
  
  #is there a way to vectorize this? I'm not sure there is when you have
  #a list of matrices
  apply(array(1:(somTools@xdim*somTools@ydim)),1, function(i) {
    #the current code is sc[i,]
    #a matrix of the adjacent codes is obtained via
    #sc[adjacentIndexes[[i]][,1],]
      #adjacentCodes <- sc[adjacentIndexes[[i]][,1],]
      mean(rowSums((sc[i,]-sc[adjacentIndexes[[i]][,1],])^2)) #not correct
      #mean(rowSums(sqrt((sc[i,]-sc[adjacentIndexes[[i]][,1],])^2)))
    } #function
    ) #apply
}