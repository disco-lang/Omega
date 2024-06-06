
import Test.HUnit
import System.Exit

import TestExpr

allTests = TestList [exprTests]

main = do
  counts <- runTestTT allTests
  if errors counts > 0 || failures counts > 0
    then exitFailure
    else exitSuccess