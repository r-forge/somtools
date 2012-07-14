#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#' Calculate adjacent nodes in a map given a somTools object and a node index
#' Here we're considering only the list of nodes,
#' not their actual visualization in 2D space.
#'
#' For each index in node.index, the neighbors are calculated and stored
#' in a list by the node index. E.g., if node.index=9:10, a list of length
#' 10 will be returned, but list items 1:8 are NULL.
#' If node.index is missing, neighbors are calculated for all nodes in the map.
#' 
#' Each non-NULL item in the returned list is a matrix listing the neighbors
#' Columns are: "nodeIndex","x","y",'x.plot','y.plot'
#'
#' Note that in som's hexa maps, nodes always have integer x/y locations even
#' though they are drawn in 2D with alternate rows offset by a half position.
somTools.adjacent.nodes <- function(somTools, node.index) {
  if(!is(somTools, "somTools")) {
    stop("Must have a somTools as first argument.")
  }
  
  if(missing(node.index)) node.index <- (1:(somTools@xdim*somTools@ydim))

  #ensure that we're looking for whole numbers and don't bother doing any twice
  #no; we want to return rows in a 1:1 correspondence to node.index; let's do some twice if we need to
  node.index <- floor(node.index) #had also included unique()
  #if doing this more than once per node proves an optimization issue,
  #let it be handled by the caller

  #check bounds
    if(any(node.index < 1)) {
      warning("Node indexes begin at 1; one or more is less than 1")
      }
    if (any(node.index > somTools@xdim*somTools@ydim)) {
      warning("At least one node.index is too large.")
      }

  l <- vector("list")
  for(i in node.index) {
    x <- (i-1) %% somTools@xdim
    y <- (i-1) %/% somTools@xdim
  
  
  xy <- NULL #accumulate neighbors as a vector: x1, y1, x2, y2...
  #...a more useful matrix will be formed when we're done.

  #We know the current node's x,y; let's use that info
  #we'll assume cells identified are valid and deal with
  #invalid ones later
  
  #both rect and hexa topologies include "  orthogonal" neighbors:
  xy <- c(xy
    ,x, y-1 # down
    ,x, y+1 # up
    ,x+1, y # right
    ,x-1, y # left
    )

  #the rectangular topology has four diagonal neighbors:
  if(isTRUE(all.equal(somTools@topol,"rect"))) {
  xy <- c(xy
    ,x-1, y-1 # downleft
    ,x+1, y+1 # upright
    ,x+1, y-1 # downright
    ,x-1, y+1 # upleft
    )
  } #if rect
  
  #hexagonal topologies have just two diagonal neighbors:
  if(isTRUE(all.equal(somTools@topol,"hexa"))) {
    #however, som::som shifts even rows to the right while
    #kohonen::som shifts odd rows to the right
    #our y value is 0-indexed, so our row is actually y+1
    shift.right <- ((y+1) %% 2) == 0
    if(somTools@original.class %in% c("kohonen")) {
      shift.right <- ! shift.right
      }
    if( shift.right ) { # if this row is shifted right, then it "touches" the X+1 cells
      xy <- c(xy, x+1, y-1, x+1, y+1)
     } else { #if the row is not shifted right, then it "touches" the x-1 cells
      xy <- c(xy, x-1, y-1, x-1, y+1)
     }
  } #hexagonal topology
  
  # get the separate lists of x and y node coordinates
  xymat <- matrix(xy, ncol=2, byrow=TRUE)
  xlist <- xymat[,1]
  ylist <- xymat[,2]
  
  #wrap toroidal maps
  if (isTRUE(somTools@toroidal)) {
    xlist.plotting <- xlist #including out-of-bounds values
    ylist.plotting <- ylist #including out-of-bounds values
    xlist <- replace(xlist,xlist==-1,somTools@xdim-1)
    xlist <- replace(xlist,xlist==somTools@xdim,0)
    ylist <- replace(ylist,ylist==-1,somTools@ydim-1)
    ylist <- replace(ylist,ylist==somTools@ydim,0)
  } else { #or remove out-of-bounds nodes from lists
  #can we combine the two bounds tests?
    #first check the x value; remove from ylist first:
    ylist <- subset(ylist,xlist!=-1)
    xlist <- subset(xlist,xlist!=-1)
    ylist <- subset(ylist,xlist!=somTools@xdim)
    xlist <- subset(xlist,xlist!=somTools@xdim)
    #now check remaining y values, removing from xlist first:
    xlist <- subset(xlist,ylist!=-1)
    ylist <- subset(ylist,ylist!=-1)
    xlist <- subset(xlist,ylist!=somTools@ydim)
    ylist <- subset(ylist,ylist!=somTools@ydim)
    xlist.plotting <- xlist #no out-of-bounds values
    ylist.plotting <- ylist #no out-of-bounds values
  } #else
  
  #add a new matrix to the list"
  l[[i]] <- matrix(c(
    xlist+(ylist*somTools@xdim)+1 #calculate nodeIndex from xylists
    , xlist #datum xlist
    , ylist #datum ylist
    , xlist.plotting #plotting xlist
    , ylist.plotting #plotting ylist
    ) #end of data
    ,ncol=5 #
   , dimnames=list(NULL,c("nodeIndex","x","y",'x.plot','y.plot'))
   )
} #for i in x
return (l) #the list
}