#from somTools
#Copyright 2012 David H. Brown
#GPL license available
require('RUnit')
somTools.testSuite <- defineTestSuite(
  'somTools Tests'
  ,c( #a list of directories
    'D:/URI/Thesis/somTools/somTools/R'
    )
  )
  
runTestSuite(somTools.testSuite)
