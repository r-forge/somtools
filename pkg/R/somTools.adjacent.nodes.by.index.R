#' Calculate adjacent nodes in a map given a somTools object and a node index

#' returns a matrix with three columns, the offset 
somTools.adjacent.nodes.by.index <- function(somTools, nodeIndex) {
  #@todo: check that map is a GenericSom
  #@todo: handle toroidal maps
  #@todo: check bounds on nodeIndex
  r <- NULL #accumulate data in list to form matrix later
  rnodes <- somTools@xdim
  nodeIndexByZero = nodeIndex-1
  hasRowPrevious = nodeIndexByZero >= rnodes
  hasRowNext = nodeIndexByZero <= rnodes*(rnodes-1)
  hasColumnPrevious = ((nodeIndexByZero %% rnodes) > 0)
  hasColumnNext = ((nodeIndexByZero %% rnodes) < (rnodes-1))
  if(identical(somTools@topol,"rect")) {
    if (hasRowPrevious) r <- c(r,nodeIndex-rnodes,0,-1)
    if (hasRowPrevious && hasColumnPrevious) r <- c(r,nodeIndex-rnodes-1,-1,-1)
    if (hasRowPrevious && hasColumnNext) r <- c(r,nodeIndex-rnodes+1,1,-1)
    if (hasColumnPrevious) r <- c(r,nodeIndex-1,-1,0)
    if (hasColumnNext) r <- c(r,nodeIndex+1,1,0)
    if (hasRowNext) r <- c(r,nodeIndex+rnodes,0,1)
    if (hasRowNext && hasColumnPrevious) r <- c(r,nodeIndex+rnodes-1,-1,1)
    if (hasRowNext && hasColumnNext) r <- c(r,nodeIndex+rnodes+1,1,1)
  } #rectangular topology
  
  #@todo: hexagonal
  #nodes are mapped with x=0 at left, y=0 at bottom.
  #index = x+ncol*y
  
  return( matrix(data=r,ncol=3, byrow=TRUE,
    dimnames=list(NULL,c("list.offset","x.offset","y.offset"))
    ) )
}