#from somTools
#Copyright 2012 David H. Brown
#GPL license available
#requires that sgen be constructed as per somTools.test.objects.R
test.someGeneric.nearest.node <- function() {
  checkEqualsNumeric(somTools.nearest.node(sgen,iris[,1:4]), sgen@datacode)
}