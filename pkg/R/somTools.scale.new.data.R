#from somTools
#Copyright 2012 David H. Brown
#GPL license available
somTools.scale.new.data <- function(somTools, newdata) {
#check parameters:
  newdata <- somTools.newdata.massage(somTools, newdata)
  if(identical(FALSE,newdata)) {
    stop("somTools@codes and newdata must match; see warnings.")
  }
  
  t(apply(newdata,1,function(x) {
  #?base::scale  "...centering is done by subtracting the column means..."
  (
    x - attr(somTools@scalecodes,"scaled:center")
  #?base::scale  "...each column of x is divided by the corresponding value from scale..."
  ) / attr(somTools@scalecodes,"scaled:scale")
  }# anon function
  )) #apply/t
}