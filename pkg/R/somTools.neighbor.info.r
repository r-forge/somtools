#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#let's return a matrix with columns:
#data.seq, nearest.nodeIndex, nearest.distance, {xx neighbor.nodeIndex xx}, neighbor.distance
#, orthogonal.projection.alpha, distance.ratio, cosine.separation
#with a row for each neighbor of each datum

somTools.neighbor.info <- function (somTools, newdata
  , scale.data=FALSE, scale.distances=FALSE) {
    
  #list to hold values; assemble to matrix later
  l <- NULL;
  
#check parameters:
  newdata <- somTools.newdata.massage(somTools, newdata)
  if(identical(FALSE,newdata)) {
    stop("somTools@codes and newdata must match; see warnings.")
  }

  #nearest.nodeIndex is a single-column (nodeIndex) matrix with rownames from newdata (if any)
  nearest.nodeIndex <- somTools.nearest.node(somTools, newdata, scale.data=scale.data);
  
  #note that neighbor.nodeIndex will be a list of matrices such that
  #neighbor.nodeIndexes[[nodeIndex]][,1] is a list of the neighbor indices
  #Columns are: "nodeIndex","x","y",'x.plot','y.plot'
  neighbor.nodeIndexes <- somTools.adjacent.nodes(somTools, nearest.nodeIndex)

    if(scale.distances) {
      sc <- somTools@scalecodes
      sd <- somTools.scale.new.data(somTools,newdata)
      } else {
      sc <- somTools@codes
      sd <- newdata
      }

###
###
### OUTER LOOP on each (possibly scaled) newdata
###
###

#  apply(cbind(nearest.nodeIndex, sd),1,function(x) {
  
  for(outer.loop in 1:nrow(sd)) {
    x<-matrix(c(nearest.nodeIndex[outer.loop], sd[outer.loop,]),nrow=1);
    #x is a row of a matrix where the first column/item is a nodeIndex
    #and the remainder [,-1] are the n-dimensional data compnents we're projecting

    origin.nodeIndex <- x[1] #x is just one row (numeric vector) for now; get "incorrect number of dimensions" with [,1]
#    origin.xy=somTools.nodeIndex.to.xy(somTools,origin.nodeIndex)
    origin.code <- sc[origin.nodeIndex,]
    neighbor.code <- sc[neighbor.nodeIndexes[[origin.nodeIndex]][,1],]
    neighbor.vec <- t(t(neighbor.code) - origin.code)
#    neighbor.grid.xy <- neighbor.nodeIndexes[[origin.nodeIndex]][,c('x.plot','y.plot')]
#    neighbor.relative.xy <- t(t(neighbor.grid.xy) - origin.xy[,c('x','y')])
    data.code <- x[-1]
    data.vec <- data.code - origin.code
    data.vec.length <- sqrt(data.vec %*% data.vec) #using this?

###
###
### INNER LOOP on each neighbor
###
###

    #apply(neighbor.vec,1,function(nv) {
    for(inner.loop in 1:nrow(neighbor.vec)) {
    
      nv<-as.numeric(t(neighbor.vec[inner.loop,]))
    #calculate orthogonal projection of data along the vector toward each neighbor
     #per page 387 David C. Lay, _Linear Algebra and its applications_ 3rd ed
     # y = y^ + z  : actual vector = orthogonal projection of y onto u + component of y orthogonal to u
     # y^ = (alpha) u for some scalar (alpha) -- *** WE WANT THIS ALPHA! ***
     # (alpha) = (y dot u) / (u dot u)
     # y^ = ((y dot u) / (u dot u)) * u
     # For us, u is the neighborhood vector named nv, so we find alpha as:
     # ( (data.vec %*% nv) / (nv %*% nv) )
     
     data.neighbor.distance = sqrt(sum( (neighbor.vec-data.vec) ^2 ))

     orthogonal.projection.alpha <- as.numeric( (data.vec %*% nv) / (nv %*% nv) )
     distance.ratio <- data.vec.length / data.neighbor.distance
     cosine.separation <- as.numeric( (data.vec %*% nv) / (data.vec.length * sqrt(nv %*% nv) ) )
     
     #return value:
#let's return a matrix with columns:
#data.seq, nearest.nodeIndex, nearest.distance, {xx neighbor.nodeIndex xx}, neighbor.distance
#, orthogonal.projection.alpha, distance.ratio, cosine.separation
     l<-c(l, outer.loop, x[1], data.vec.length, data.neighbor.distance, orthogonal.projection.alpha, distance.ratio, cosine.separation)
  } #INNER LOOP
      

  } #OUTER LOOP
  r <- matrix(l, ncol=7, byrow=TRUE)
  colnames(r) <- c('data.seq','nearest.nodeIndex', 'nearest.distance', 'neighbor.distance', 
    'orthogonal.projection.alpha', 'distance.ratio', 'cosine.separation')
  
  r
}
