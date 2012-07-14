#from somTools
#Copyright 2012 David H. Brown
#GPL license available
# returns a list[[node.index]] where each entry is a 4-column matrix
# listing x1,y1,x2,y2 points that identify the edges surrounding the 
# set of nodes nodes dentified by node.index (all nodes if missing).
# The x/y points are NOT suitable for plotting and have NOT been transformed
# from the integer node.index xy equivalents e.g., for hex topologies or
# cartogram interpolations
# No attempt has been made to order the segments to be contiguous; they
# should be drawn individually. In fact, some segments will be reversed
# with respect to the order in which points are returned by the
# somTools.node.polygon() function
somTools.node.edges <- function(somTools, node.index, include.interior=FALSE) {
  if(!is(somTools, "somTools")) {
    stop("Must have a somTools as first argument.")
  }
  
  if(missing(node.index)) node.index <- (1:(somTools@xdim*somTools@ydim))
  
  polys <- somTools.node.polygon(somTools, node.index, closed=TRUE)
  #if we get spurious edges, it might be on account of floating-point imprecision
  #we could find some sort of round-down function like
  #rounddown <- function(x,digits) { floor(x*10^digits)/10^digits }

########################################
## Vectorization optimization results
##
## As you can see below, I wrote two versions of this code;
## the first had an inner loop; in the second, I was able to
## dispose of that inner loop by using some temporary structures
## (p.first and p.next are matrices; p.test is a vector)
##
## I ran each version 3 times on a 10x10 hexagonal grid (all nodeIndexes)
##
## Inner loop:
##    user  system elapsed
##    0.10    0.00    0.12
##    0.13    0.00    0.12
##    0.12    0.00    0.13
##
## Vectorized / matrix manipulation
##    user  system elapsed
##    0.05    0.00    0.04
##    0.03    0.00    0.05
##    0.05    0.00    0.05
##
## It appears that the rewritten version requires
## about half the time of the original.
##
#########################################
  
#  l <- NULL #for inner j loop version
  tmp <- NULL
  for (i in node.index) {
    #might be able to vectorize this inner loop if I really need to;
    #would probably need to construct an auxiliary vector containing the results
    #of the sorting test:
    p.first <- polys[[i]][-nrow(polys[[i]]),] #all but the last row
    p.next <- polys[[i]][-1,] #all but the first row
    # p.first[i,] and p.next[i,] are now the endpoints of segment i
    p.test <- (p.first[,1]>p.next[,1]) | (p.first[,1]==p.next[,1] & p.first[,2]>p.next[,2])
    # p.test is a logical vector indicating whether a given row needs to have its points flipped
    # multiplying a matrix by the logical vector results in a matrix where all FALSE rows are zero
    # and all TRUE rows are unchanged.
    tmp <- rbind(tmp, cbind((p.test * p.first) + ((!p.test) * p.next), ((!p.test) * p.first) + (p.test * p.next)) )
    
#    for(j in 1:(nrow(polys[[i]])-1)) {
#      #we want to ensure the points are listed in the same sequence
#      #no matter how the polygon was constructed
#      #otherwise duplicate detection won't work
#      if( #mini-sort
#        (polys[[i]][j,1] > polys[[i]][j+1,1]) #x1 is larger than x2
#        | #or x1==x2 and y1 is larger than y2
#        ((polys[[i]][j,1] == polys[[i]][j+1,1]) & (polys[[i]][j,2] > polys[[i]][j+1,2]))
#       ) {
#        a <- j+1
#        b <- j
#      } else {
#        a <- j
#        b <- j+1
#      } #else
#      l <- c(l, polys[[i]][a,], polys[[i]][b,])
#      } #for j = each starting point in the closed list of polygon points
    } #for each node in the list
#  tmp <- matrix(l, byrow=TRUE, ncol=4) ##for inner loop version
  dimnames(tmp) <- list(NULL,c('x1','y1','x2','y2'))
  if(include.interior) {
    return (unique(tmp)) #avoid duplicate edges, at least
  }
  #interior edges border multiple polygons, so we can identify all occurrences of that edge by
  #running the duplicated() function by rows (MARGIN=1) in both directions (fromLast=FALSE:TRUE):
  dups <- which(duplicated(tmp, MARGIN=1) | duplicated(tmp, MARGIN=1, fromLast=TRUE))
  if(length(dups)==0) {
    return(tmp) #otherwise we would return tmp[-0,] which is empty
    }
  #then we simply return the matrix without any of the duplicated rows:
  tmp[-dups,]
} #somTools.node.polygon