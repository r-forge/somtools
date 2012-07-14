#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#scale data coordinates from square xy to align with hexes (if appropriate to map)
#currently takes lists of x, y or a matrix for x ; this may change?
#returns a 2-column matrix
somTools.hexify.xy <- function(somTools, x, y) {
#check parameters:
  if(!is(somTools, "somTools")) {
    stop("Must have a somTools as first argument.")
  }
if(missing(y)) {#x better be a matrix, then
  y <- x[,2]
  x <- x[,1]
} #missing y
if(length(x) != length(y)) {
  stop('Length of x must match length of y')
  }
  
  if (identical(somTools@topol,'rect')) {
    return(matrix(c(x,y), ncol=2, dimnames=list(NULL,c('x','y')))) #unchanged as matrix
    }
  
  
  #we'll add 1 to y to account for maps which shift odd rows 
  if(somTools@original.class %in% c('kohonen')) {
    ys <- 1+y
    } else {
    ys <- y
    }
  
  #here we calculate a zig-zag offset based on the y value
  xadj <- abs(ys/2-round(ys/2))
  
  #return value:
  matrix(c(x+xadj, y * sqrt(3)/2), ncol=2, dimnames=list(NULL,c('x','y')) )
}
