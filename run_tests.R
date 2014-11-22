# install.packages('RUnit')
library('RUnit')
source('cachematrix.R')

suite <- defineTestSuite("example",
                          dirs = file.path("tests"),
                          testFileRegexp = "^test_\\w+\\.R$")

results <- runTestSuite(suite)
printTextProtocol(results)
