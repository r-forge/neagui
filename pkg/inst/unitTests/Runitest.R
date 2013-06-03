require("RUnit", quietly=TRUE) || stop("RUnit package not found")
require("neaGUI")



runitPat <- ".*_test\\.[rR]$"
runitDirs <- c(".")
suite.neaGUI  <- defineTestSuite(name="neaGUI",
                         dirs=runitDirs,
                         testFileRegexp=runitPat,
                         rngKind="default",
                         rngNormalKind="default")

testResult <- runTestSuite(testsuite.neaGUI )
printTextProtocol(testResult)







