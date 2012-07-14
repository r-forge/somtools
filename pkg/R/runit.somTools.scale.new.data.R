#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#requires that somTools.scale.new.data applied to the node codes
#must produce the same result as the scale() function's output
#stored in scalecodes
test.someGeneric.scale.new.data <- function() {
  all(somTools.scale.new.data(sgen,sgen@codes)==sgen@scalecodes)
}