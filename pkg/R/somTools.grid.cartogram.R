#from somTools
#Copyright 2012 David H. Brown
#GPL license available
##' obtain a cartogram on somTools according to areas given as vector, one-per-cell.

##' some code relating to working with the sp package is thanks to
##' (or at least inspired by) Thomas Zumbrumm; see a currently unfinished 
##' project at: https://r-forge.r-project.org/projects/cart/
somTools.grid.cartogram <- function(somTools, areas, sea.expansion=0.2, grid.dimensions=c(128,128), blur=0) {
  if(!is(somTools, "somTools")) {
    stop("Must have a somTools as first argument.")
  }
  if(missing(areas)) {
    #include some minimum area to avoid squishing a cell to nothing
    areas <- (somTools.map.density(somTools) + 0.75) ^ 1.5 #squaring it seemed too extreme
    }
  if(length(areas) != somTools@xdim*somTools@ydim) {
    warning("Areas should be equal to the number of cells in the SOM")
    }
  if (length(grid.dimensions) == 1) grid.dimensions <- rep(grid.dimensions,2)
  if (length(grid.dimensions) != 2) {
    warning("Should specify grid.dimensions as a vector of 2 numerics (x,y)")
    }
  if (length(grid.dimensions) < 2) stop("Unusable grid.dimensions.")
  
  if (!isTRUE(all.equal(log(grid.dimensions, 2), floor(log(grid.dimensions, 2))))) {
    warning("Set grid.dimensions to powers of 2 for faster calculation")
    }

  spolys <- somTools.grid.SpatialPolygons(somTools)
  
  #the cartogram process wants a fairly large "sea" of space around the
  #actual map area so as to avoid edge effects
  #note: the cartogram function can do this itself...
  range <- diff(t(spolys@bbox))
  shift <- range * sea.expansion
  bbox.expanded <- spolys@bbox + c(-shift, shift)
  range.expanded <- diff(t(bbox.expanded))
  spgrid <- SpatialGrid(
            GridTopology(
              cellcentre.offset = as.numeric(bbox.expanded[, "min"]),
              cellsize = as.numeric(range.expanded / (grid.dimensions - 1)),
              cells.dim = grid.dimensions #spgrid@grid@cells.dim
            )#GridTopology #spgrid@grid
          )#SpatialGrid
          
  ## overlay grid and polygons

  ## This is an extension of the point-in-polygon problem. We obtain a vector of
  ## indices of the polygons in spdf.
  ind <- overlay(spgrid, spolys)

  ## calculate "density"

  ## For each grid cell, we need to determine the fraction of the units of
  ## "variable" as the number of units per cell. For NAs, i.e. for the "sea",
  ## insert the mean value for the whole "land" mass. For the tabulation, the
  ## levels need to be enforced because there might be polygons with count zero.
  ## For these cells, division by zero is corrected by replacing the resulting
  ## infinite result by zero.
  tab <- xtabs(~ factor(ind, levels = seq(along = spolys@polygons)))
  indVar <- areas[as.numeric(names(tab))] / tab
  indVar[is.infinite(indVar)] <- 0
  tmean <- sum(tab * indVar[as.numeric(names(tab))]) / sum(tab)
  ind[is.na(ind)] <- length(areas) + 1
  indVar[length(areas) + 1] <- tmean
  
  dens <- matrix(indVar[ind], byrow = TRUE, ncol = grid.dimensions[2])

  cart <- cartogram(dens, blur=blur)
  
  #now we need to add some extra information to the cartogram
  #first, the bounding box of the expanded grid this cartogram represents
  cart$bbox <- bbox.expanded
  cart$cellcentre.offset <- spgrid@grid@cellcentre.offset
  cart$cellsize <- spgrid@grid@cellsize
  cart$cells.dim <- spgrid@grid@cells.dim
  
  #we're going to include a function; functions bring along their environment,
  #so we'll remove largish objects we no longer need:
  rm('spgrid','spolys','areas','tab','indVar','ind','dens')
  #the transform function scales data from the original space to the
  #cartogram grid.dimensions space
  cart$transform <- function(x, y = NULL) {
    if(missing(y) || is.null(y)) {
     y = x[,2]
     x = x[,1]
    }
    x <- (x-cart$cellcentre.offset[1]) / cart$cellsize[1]
    #we need to flip the y axis to match the cartogram structure with [1,1] at UL:
    y <- cart$cells.dim[2]- ( (y-cart$cellcentre.offset[2]) / cart$cellsize[2] )

    tmp <- predict(cart, x, y)
    #and we need to flip the y axis back to match how things plot with (0,0) at LL:
    tmp$y <- cart$cells.dim[2]-tmp$y
    
    matrix(c(tmp$x * cart$cellsize[1] + cart$cellcentre.offset[1],
             tmp$y * cart$cellsize[2] + cart$cellcentre.offset[2])
           , ncol=2, dimnames=list(NULL,c('x','y')))
  }# cart$transform

  return(cart)
}