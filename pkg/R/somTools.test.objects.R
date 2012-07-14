#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#file to build test objects for somTools

set.seed(4)

if ("package:kohonen" %in% search()) detach(package:kohonen)
library('som') #masks kohonen::som
ssom <- som(as.matrix(iris[1:4]),xdim=12,ydim=8, topol="hexa")
sgen <- new("somTools",ssom)
detach (package:som)
#sgen #summarize

if ("package:som" %in% search()) detach (package:som)
library('kohonen') #masks som::som
#partial training data:
#ksom <- som(as.matrix(iris[1:50*3,1:4]), init=iris[sample(1:150, 10*10, replace=FALSE),1:4,drop=FALSE], grid = somgrid(10, 10, "hexagonal"), rlen=1000, toroidal=FALSE, keep.data=TRUE)
#comparison plot:
# plot(ksom,classif=map(ksom, as.matrix(iris[,1:4]))$unit.classif,type="mapping",pch=pointcodes)


#full map:
#ksom <- som(as.matrix(iris[1:4]),grid = somgrid(10, 10, "hexagonal"), rlen=1000,toroidal=FALSE, keep.data=TRUE)
#ksom <- som(as.matrix(iris[1:4]),grid = somgrid(10, 10, "rectangular"), rlen=1000,toroidal=FALSE, keep.data=TRUE)

#wide map:
#ksom <- som(as.matrix(iris[1:4]), grid=somgrid(14,7,"hexagonal"),rlen=1000,keep.data=TRUE)
#ksom <- som(as.matrix(iris[1:4]), grid=somgrid(14,7,"rectangular"),rlen=1000,keep.data=TRUE)
#wide map, not yet converged:
#ksom <- som(as.matrix(iris[1:4]), grid=somgrid(14,7,"hexagonal"),rlen=10,keep.data=TRUE)
#ksom <- som(as.matrix(iris[1:4]), grid=somgrid(14,7,"rectangular"),rlen=10,keep.data=TRUE)
#partial training data:
#ksom <- som(as.matrix(iris[1:50*3,1:4]), init=iris[sample(1:150, 14*7, replace=FALSE),1:4,drop=FALSE], grid = somgrid(14, 7, "rectangular"), rlen=1000, toroidal=FALSE, keep.data=TRUE)
#comparison plot:
# plot(ksom,classif=map(ksom, as.matrix(iris[,1:4]))$unit.classif,type="mapping",pch=pointcodes)

#wideish map:
ksom <- som(as.matrix(iris[1:4]), grid=somgrid(10,6,"rectangular"),rlen=1000,keep.data=TRUE)
#partial training data:
#ksom <- som(as.matrix(iris[1:50*3,1:4]), init=iris[sample(1:150, 10*6, replace=FALSE),1:4,drop=FALSE], grid = somgrid(10, 6, "rectangular"), rlen=1000, toroidal=FALSE, keep.data=TRUE)

#max map
#ksom <- som(as.matrix(iris[1:4]), grid=somgrid(15,10,"hexagonal"),rlen=1000,keep.data=TRUE)
#ksom <- som(as.matrix(iris[1:4]), grid=somgrid(15,10,"rectangular"),rlen=1000,keep.data=TRUE)
#ksom <- som(as.matrix(iris[1:50*3,1:4]), init=iris[sample(1:150, 15*10, replace=FALSE),1:4,drop=FALSE], grid = somgrid(15, 10, "rectangular"), rlen=1000, toroidal=FALSE, keep.data=TRUE)


#small map, about a quarter of max map
#ksom <- som(as.matrix(iris[1:4]), grid=somgrid(6,4,"hexagonal"),rlen=1000,keep.data=TRUE)
#ksom <- som(as.matrix(iris[1:4]), grid=somgrid(6,4,"rectangular"),rlen=1000,keep.data=TRUE)

#small map, not yet converged:
#ksom <- som(as.matrix(iris[1:4]), grid=somgrid(6,4,"hexagonal"),rlen=10,keep.data=TRUE)

#data(wines)
#ksom <- som(wines, rlen=1000, grid=somgrid(12,14, "hexagonal"), toroidal=FALSE, keep.data=TRUE)
kgen <- new("somTools", ksom)
#detach(package:kohonen)
#kgen #summarize

#atom <- read.csv("D:/URI/SOM/fcps-atom.csv")
#ksom <- som(as.matrix(atom[1:3]), grid=somgrid(30,20,"rectangular"),rlen=1000,keep.data=TRUE)
#kgen <- new("somTools", ksom)