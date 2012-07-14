#from somTools
#Copyright 2012 David H. Brown
#GPL license available
# returns a list[[node.index]] where each entry is a n*2 matrix
# listing x/y points that surround the node
# the x/y points HAVE been hexified
# sp:Polygon requires the polygon to be closed=TRUE
# @TODO: move the test for the original class and which hex row shifts
#        to be a class property and set during initialization
somTools.node.polygon <- function(somTools, node.index, closed=FALSE) {
  if(!is(somTools, "somTools")) {
    stop("Must have a somTools as first argument.")
  }
  
  if(missing(node.index)) node.index <- (1:(somTools@xdim*somTools@ydim))
  
  
  xy <- somTools.nodeIndex.to.xy(somTools,node.index)
  x <- xy[, 'x']
  y <- xy[, 'y']
  #we can form a monster list l of x for all node.index, y for all node.index
  #and then reassemble it as needed in a matrix

  l <- c( node.index ) #keep track to identify entries in list

  if(identical(somTools@topol,'rect')) {
  #let's handle just rectangular grids first...
  l <- c(l,
    x-0.5, y-0.5, #ll
    x-0.5, y+0.5, #ul
    x+0.5, y+0.5, #ur
    x+0.5, y-0.5) #lr
  if(closed) { l <- c(l,x-0.5, y-0.5) }
  }

  if(identical(somTools@topol,'hexa')) {
  #hexagon grids are arranged such that rows are on a line and columns stagger
  #For the distance between these parallel sides to be equal to 1.0, the
  #length of the side must be sqrt(3)/3.
  #For kohonen maps, the 1st (bottom) and other "odd" rows are offset to the right
  #For som hex maps, the 2nd-from-bottom and "even" rows are offset to the right
  sq3 <- sqrt(3) #not really saving time by precomputing these, but it's a
  sq33 <- sq3/3  #maybe a little clearer in the code below?
  sq36 <- sq3/6
  
  if(identical(somTools@original.class,'kohonen')) {
    x <- x+((1-(y %%2)) * .5)
    } else {
    x <- x+((y %% 2) * .5)
  }

  y <- y*(sq3/2) #scaly y height to nest
  
  l <- c(l,
    x, y-sq33, #bottom
    x-0.5, y-sq36, #lower-left
    x-0.5, y+sq36, #upper-left
    x, y+sq33, #top
    x+0.5, y+sq36, #upper-right
    x+0.5, y-sq36) #lower-right
  if(closed) { l <- c(l,x, y-sq33) }
  }
  m <- matrix(l, nrow=length(node.index)  ) #matrix
  l <- list()
  for (i in 1:length(node.index)) {
    l[[m[i,1]]] <- matrix(m[i,-1],ncol=2,byrow=TRUE,dimname=list(NULL,c('x','y')))
    }
  l
} #somTools.node.polygon