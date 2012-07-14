#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#compute the centroid node for each node on the map
#after method proposed and implemented by Prof. Lutz Hamel, URI
somTools.centroids <- function (somTools, umat, explicit=FALSE) {
    #check parameters:
  if(!is(somTools, "somTools")) {
    warning("Must have a somTools as first argument.")
    return(FALSE);
  }
  if(missing(umat)) {
    umat <- somTools.umat(somTools)
    }
    
  adjacent <- somTools.adjacent.nodes(somTools)
  #adjacent is a list of matrices
  #each matrix has a 'nodeIndex' column
  #length(adjacent) is the number of nodes in the map
  
  #centroids is a vector that matches the nodeIndex sequence of nodes in the map
  #note that zero is an invalid nodeIndex, but it will be replaced momentarily
  centroids <- rep(NA, length(adjacent))
  
  #can't do this without a loop of some sort because "recursive indexing failed
  # at level 2" in adjacent (a list); adjacent has to be a list because it contains
  #matrices of varying numbers of rows. 
  for(i in 1:length(adjacent)) {
    #first, we'll set it to the least-umat neighbor
    centroids[i]<-adjacent[[i]][which.min(umat[adjacent[[i]][,'nodeIndex']]), 'nodeIndex']
    #but check if this node is itself cooler:
    if(umat[i] < umat[centroids[i]]) { centroids[i] <- i }
    }
  
  #at this point, centroids is a vector of nodeIndexes such that
  #centroids[x] is the nodeIndex of a neighbor of x or is x itself,
  #whichever has the least "umat" value.
  if(!explicit) {
  #find the self-centered nodes that are their own local minima
    minima <- which(centroids==1:length(adjacent))
    #so long as we have some nodes that don't point to a local minima:
    while(! all(centroids %in% minima)) {
      #list of the nodes that are directly connected to local minima
      #(but not the minima themselves; not sure that's a useful optimization)
      connected <- setdiff(which(centroids %in% minima), minima)
      disconnected <- which(!(centroids %in% minima))
      #to get a vector of disconnected node indexes whose current centroid
      #is a connected node index:
      # disconnected[which(centroids[disconnected] %in% connected)]
      #We need to follow these two steps back to get to a minima.
      #The minima node index to which the connected node connects is found by,
      # centroids[centroids[disconnected[which(centroids[disconnected] %in% connected)]]]
      #but that's getting hard to understand, so let's set a temporary variable:
      almost.connected <- disconnected[which(centroids[disconnected] %in% connected)]
      if(length(almost.connected)==0) {break} #should never occur, but just in case
      centroids[almost.connected] <- centroids[centroids[almost.connected]]
    } #while some centroids do not point to the local minimima  
  } # if not explicit
  
  
  centroids
}