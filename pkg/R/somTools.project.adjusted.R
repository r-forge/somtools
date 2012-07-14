#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#let's return a 4-column matrix
#with a row for each input newdata
#and columns(x,y,xadj,yadj)

#note that this function does not account for hexagonal mappings
#that is done by somTools.hexify.xy

somTools.project.adjusted <- function (somTools, newdata
  , bestmatch=FALSE, scale.data=FALSE, scale.distances=FALSE, alphafun=somTools.project.adjusted.alphafun.linear
  , scalefun=somTools.project.adjusted.scalefun.linear) {
  verbose <- FALSE
  warn.over.one <- TRUE
#check parameters:
  if(missing(newdata)) {
    newdata <- somTools@data
    bestmatch <- as.matrix(somTools@datacode,ncol=1)
    }
  newdata <- somTools.newdata.massage(somTools, newdata)
  if(identical(FALSE,newdata)) {
    stop("somTools@codes and newdata must match; see warnings.")
  }

  if(!identical(FALSE,bestmatch)) {
    nearest.nodeIndex <- as.matrix(bestmatch,ncol=1)
    colnames(nearest.nodeIndex) <- 'nodeIndex'
    } else {
  #nearest.nodeIndex is a single-column (nodeIndex) matrix with rownames from newdata (if any)
  nearest.nodeIndex <- somTools.nearest.node(somTools, newdata, scale.data=scale.data);
  }
  if (verbose) {
    cat(sprintf("nearest.nodeIndex:\n"))
    show(nearest.nodeIndex)
    }
  #note that neighbor.nodeIndex will be a list of matrices such that
  #neighbor.nodeIndexes[[nodeIndex]][,1] is a list of the neighbor indices
  #Columns are: "nodeIndex","x","y",'x.plot','y.plot'
  neighbor.nodeIndexes <- somTools.adjacent.nodes(somTools, nearest.nodeIndex)
  if (verbose>1) {
    cat(sprintf("neighbor.nodeIndexes:\n"))
    show(neighbor.nodeIndexes)
    }
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

  adjusted <- t(apply(cbind(nearest.nodeIndex, sd),1,function(x) {
    #x is a row of a matrix where the first column/item is a nodeIndex
    #and the remainder [,-1] are the n-dimensional data compnents we're projecting

############# NEED TO STEP THROUGH THIS TO MAKE SURE DATA FORMATS ARE AS EXPECTED!
    
    #@todo: respect scale.distances
    #@todo: look at this to see whether we can do it without apply()... i.e.,
    #if there is some way to avoid assuming we have a single row
    if(verbose) vshow(x,"in t(apply(cbind(nearest.nodeIndex, sd)... x")
    
    origin.nodeIndex <- x['nodeIndex'] #x is just one row (numeric vector) for now; get "incorrect number of dimensions" with [,1]
    origin.xy=somTools.nodeIndex.to.xy(somTools,origin.nodeIndex)
    if(verbose) vshow(origin.nodeIndex,'origin.nodeIndex')
    
    if(verbose) vshow(origin.xy,'origin.xy')

      
    if(verbose>4) vshow(sc, sprintf('sc (%s)', ifelse(scale.data,'scaled','not scaled')))
    if(verbose>4) vshow(sd, sprintf('sd (%s)', ifelse(scale.data,'scaled','not scaled')))

    origin.code <- sc[origin.nodeIndex,]
    if(verbose) vshow(origin.code, 'origin.code')

    neighbor.code <- sc[neighbor.nodeIndexes[[origin.nodeIndex]][,1],]
    if(verbose) vshow(neighbor.code,'neighbor.code')

    #calculate vector from origin=(0_1,...,0_n)
    #the default vectorization is by column, so we need to t(neighbor.code)
    #to get x,y pairs so that origin.xy lines up properly... then reform a matrix
    neighbor.vec <- t(t(neighbor.code) - origin.code)
    if(verbose) vshow(neighbor.vec,'neighbor.vec')
    
    neighbor.grid.xy <- neighbor.nodeIndexes[[origin.nodeIndex]][,c('x.plot','y.plot')]
    if(verbose) vshow(neighbor.grid.xy,'neighbor.grid.xy')

    #the default vectorization is by column, so we need to t(neighbor.grid.xy)
    #to get x,y pairs so that origin.xy lines up properly... then reform a matrix
    neighbor.relative.xy <- t(t(neighbor.grid.xy) - origin.xy[,c('x','y')])
    
    if(verbose) vshow(neighbor.relative.xy,'neighbor.relative.xy')
    
    #did scaling outside the loop
#    if(scale.data) {
#      data.code <- somTools.scale.new.data(somTools,x[-1])
#      } else {
      data.code <- x[-1]
#      }
#    if(verbose) vshow(data.code, 'data.code')

    #calculate vector from origin=(0_1,...,0_n)
    data.vec <- data.code - origin.code
    if(verbose) vshow(data.vec,'data.vec')
    
    data.vec.length <- sqrt(data.vec %*% data.vec) #using this?

###
###
### INNER LOOP on each neighbor
###
###

    #calculate orthogonal projection of data along the vector toward each neighbor
    data.neighbor.proj <- apply(neighbor.vec,1,function(nv) {
      if(verbose) vshow(nv,"in t(apply(neighbor.vec... nv")
      
     #per page 387 David C. Lay, _Linear Algebra and its applications_ 3rd ed
     # y = y^ + z  : actual vector = orthogonal projection of y onto u + component of y orthogonal to u
     # y^ = (alpha) u for some scalar (alpha) -- *** WE WANT THIS ALPHA! ***
     # (alpha) = (y dot u) / (u dot u)
     # y^ = ((y dot u) / (u dot u)) * u
     # For us, u is the neighborhood vector named nv, so we find alpha as:
     # ( (data.vec %*% nv) / (nv %*% nv) )

     alpha <- as.numeric( (data.vec %*% nv) / (nv %*% nv) )

#what about just comparing the distances? No; doesn't seem to work
#    alpha <- as.numeric (data.vec.length / ( sqrt(sum( (nv-data.vec) ^2 )) ) )
     
     #now, this alpha value is sometimes greater than 1 which overshoots the neighbor
     #need to find a good way to scale it.
     
     alpha <- alphafun(alpha, nv=nv, data.vec=data.vec, data.vec.length=data.vec.length)
     
     #alpha <- alpha * sqrt(sum( (nv*alpha)^2 )) / sqrt(sum(data.vec^2)) #length of projection / length of data vector didn't do much

#inline functions moved outside to be passed in as alphafun     
#how about multiplying by: (distance to nearest) / (distance to neighbor) 
#     alpha <- alpha * ( (data.vec.length) / ( sqrt(sum( (neighbor.vec-data.vec) ^2 ))) )
#this is looking good... very much like dividing by the number of neighbors, but perhaps more meaningful
#Kohonen (p18 of the book) mentions direction cosines...
#      alpha <- abs(alpha) * ( (data.vec %*% nv) / (data.vec.length * sqrt(nv %*% nv) ) )
     
     if( warn.over.one & abs(alpha)>1) {
      print("") #newline?
      print(sprintf("warn.over.one: Projection to neighbor is %f (before scaling)",alpha))
      vshow(x, "x of outer loop: nodeIndex, datum to project")
      vshow(data.vec, "data.vec (translated)")
      vshow(nv, "nv (neighbor vector, translated)")
      vshow(origin.code, "origin.code")
     # vshow(sd, "sd (datum, possibly scaled)") #let's not... sd is all data!
      
#      vshow(data.code, "data.code, not translated")
      } #warn.over.one
     
     alpha #return value
  } #function (nv)
  ) #apply(neighbor.vec,1...
      
    if(verbose) vshow(data.neighbor.proj,'data.neighbor.proj before scaling')
    data.neighbor.proj <- scalefun(data.neighbor.proj)
    if(verbose) vshow(data.neighbor.proj,'data.neighbor.proj after scaling')
    
    #we seem to be moving too far... quite probably because one neighbor is pulling
    #while the opposite neighbor is pushing. A quick fix could be to divide by 2?
    #maybe it would be better to allow only pulling, not pushing... i.e., set any
    #negative projections to zero.
    #okay... here's what we do... if push=TRUE, allowing pushing, we divide all offsets by 2
    #...otherwise, we allow only "pulling," by replacing negative values with zero.
#    if(push) {
#      data.neighbor.proj <- data.neighbor.proj
#    } else {
#      data.neighbor.proj <- replace(data.neighbor.proj, data.neighbor.proj < 0, 0)
#    }
      
    #now, multiply each offset in the neighbor.relative.grid by the corresponding projection length
    #vectorization takes entries in a matrix by column, so the values in data.neighbor.proj.unit
    #will be used twice: first for x column then for y column from neighbor.relative.grid
    neighbor.adjustments <- neighbor.relative.xy * data.neighbor.proj #was data.neighbor.proj.unit
    
    if(verbose) vshow(neighbor.adjustments,'neighbor.adjustments')
    
    xy <- colSums(neighbor.adjustments)# if we make it a matrix,nrow=1,dimname=list(,c('x','y')))
    if(verbose) vshow(xy,'final xy adjustment')
    
    if(verbose) vshow(xy+origin.xy,"return value of t(apply(sd...")
    xy + origin.xy

  })) #t(apply(newdata....
  colnames(adjusted) <- c('x','y')
  
  adjusted
}

#functons to scale individual projection components. 
#arguments include alpha (the orthogonal projection distance)
#data.vec.length, nv (translated neighbor vector), data.vec (translated)
somTools.project.adjusted.alphafun.linear <- function (alpha, nv, data.vec, data.vec.length) { alpha }
#Kohonen (p18 of the book) mentions direction cosines...
somTools.project.adjusted.alphafun.cosine <- function (alpha, nv, data.vec, data.vec.length) { 
  abs(alpha) * ( (data.vec %*% nv) / (data.vec.length * sqrt(nv %*% nv) ) )
  }
#how about multiplying by: (distance to nearest) / (distance to neighbor) 
somTools.project.adjusted.alphafun.distratio <- function (alpha, nv, data.vec, data.vec.length) { 
  alpha * ( (data.vec.length) / ( sqrt(sum( (nv-data.vec) ^2 ))) )
  }

### some possibly useful scalefunctions:
somTools.project.adjusted.scalefun.linear <- function (d) { d }
somTools.project.adjusted.scalefun.logsquish <- function (d) { log(1+abs(d))*sign(d) }
somTools.project.adjusted.scalefun.exlin <- function(ex=1, lin=1, pre=1) { function (d) { (abs(d/pre)^ex) * sign(d) * lin * pre } }
somTools.project.adjusted.scalefun.neighbors <- function(d) { d / length(d) }
somTools.project.adjusted.scalefun.halfneighbors <- function(d) { d / length(d) * 2 }
#this doesn't work as written (object 'somTools' not found), but you get the idea:
#somTools.project.adjusted.scalefun.halfmaxneighbors <- function(d) { d / ifelse(identical(somTools@topol,"hexa"),6,8) }
