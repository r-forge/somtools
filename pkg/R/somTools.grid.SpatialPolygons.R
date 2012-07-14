#from somTools
#Copyright 2012 David H. Brown
#GPL license available
##' create a sp:Polygons object containing all the polygons in the map.
somTools.grid.SpatialPolygons <- function(somTools) {
  if(!is(somTools, "somTools")) {
    stop("Must have a somTools as first argument.")
  }
  xys <- somTools.node.polygon(somTools,closed=TRUE)
  polys <- list()
  for (i in 1:length(xys)) {
    polys[[i]] <- Polygons(list(Polygon(xys[[i]])), ID=i)
    }
  SpatialPolygons(polys, pO=1:length(xys))
}