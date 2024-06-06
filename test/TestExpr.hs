
module TestExpr(exprTests) where

import Test.HUnit

import Data.Presburger.Omega.Expr
import Debug.Trace

-- | Test whether two expressions are equal by subtracting them.
--   Equal expressions simplify to 0.
assertEqualExps :: IntExp -> IntExp -> Assertion
assertEqualExps e1 e2 =
  if show (e1 |-| e2) == "intE 0"
  then return ()
  else assertFailure message
  where
    message =
      "Expressions not equal:\n\t" ++ show e1 ++ "\n\t" ++ show e2

sumOfProducts = assertEqualExps sop pos
  where
    [x, y] = takeFreeVariables' 2
    sop = x |*| x |-| x |*| y |-| 2 *| y |*| y
    pos = (x |+| y) |*| (x |-| 2 *| y)

exprTests = TestLabel "Expression manipulation" $ TestList
            [test sumOfProducts]