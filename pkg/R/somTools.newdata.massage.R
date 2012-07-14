#from somTools
#Copyright 2012 David H. Brown
#GPL license available
# attempts to format newdata as a matrix containing as many
# columns as somTools@codes. Returns that matrix on success
# or false on failure (warnings issued)
somTools.newdata.massage <- function(somTools,newdata) {
  if(!is(somTools, "somTools")) {
    warning("Must have a somTools as first argument.")
    return(FALSE);
  }
  #handle data.frame
  if(is.data.frame(newdata)) {
    newdata <- as.matrix(newdata)
  }
  
  #handle something else
  if (! is.matrix(newdata)) {
    if(! 0 == (length(newdata) %% attr(somTools@codes,"dim")[2])) {
      warning("Length of newdata (2nd argument) must have a length that is a multiple of the number of columns in somTools@codes.")
      return(FALSE)
      } 
    newdata <- matrix(newdata,byrow=TRUE,ncol=attr(somTools@codes,"dim")[2])
  }

  #now have a matrix; check columns
  if(attr(newdata,"dim")[2] != attr(somTools@codes,"dim")[2]) {
    warning("Columns of newdata (2nd argument) must equal the number of columns in somTools@codes.")
  } 
  
  #return possibly modified newdata if no problems:
  newdata
}