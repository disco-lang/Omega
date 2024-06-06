
-- | Internal routines used by both "Data.Presburger.Omega.Set" and
-- "Data.Presburger.Omega.Rel"

module Data.Presburger.Omega.SetRel where

import Data.Presburger.Omega.LowLevel
import Data.Presburger.Omega.Expr

-- Make a lookup function for translating 'VarHandle's to 'Var's.
-- The position of a handle determines what 'Var' it translates to. 
makeLookupFunction :: [VarHandle] -> (VarHandle -> Var)
makeLookupFunction lowLevelVars =
    let expVars = takeFreeVariables (length lowLevelVars)
        varLookupTable = zip lowLevelVars expVars

        findVar v = case lookup v varLookupTable
                    of Just v' -> v'
                       Nothing -> error "Cannot find Omega variable"
    in findVar

-- Create an expression fom some low-level data.
--
-- The boolean parameter is true for equality constraints,
-- false for inequality constraints.
constraintToExpr :: Bool          -- ^ Is equality
                 -> [VarHandle]   -- ^ Bound variables
                 -> [Coefficient] -- ^ Terms
                 -> Int           -- ^ Constant part
                 -> BoolExpr      -- ^ Expression
constraintToExpr isEquality boundVars terms constant =
    let -- The existential variables are innermost
        findVar = makeLookupFunction boundVars

        -- Sum of all products and the constant term
        sumTerm = sumOfProductsExpr constant $ map productTerm terms
            where
              productTerm (Coefficient v n) = (n, [findVar v])

        -- Test whether is equal to zero/nonnegative
        boolTerm = if isEquality
                   then testExpr IsZero sumTerm
                   else testExpr IsGEZ sumTerm
    in boolTerm

-- Get the set as a function.
-- We pass list-building routines to the low-level queryDNFSet function.
setToExpression :: OmegaSet -> IO (Int, BoolExp)
setToExpression s = do
  (setVars, conjuncts) <- queryDNFSet addEq [] addGeq [] addConjunct [] s
  return (length setVars, wrapSimplifiedExpr $ disjExpr conjuncts)
    where
      -- Call constraintToExpr with the existential variables bound first,
      -- then the set variables.

      addEq setVars exVars terms constant =
          (constraintToExpr True (exVars ++ setVars) terms constant :)

      addGeq setVars exVars terms constant =
          (constraintToExpr False (exVars ++ setVars) terms constant :)

      addConjunct _ exVars eqs geqs =
          wrapExistentialVars exVars eqs geqs

-- Get a relation as a boolean expression.
-- In the formula, we expect to see only the output variable whose index
-- is given by 'index'.
--
-- In the result expression, the chosen output variable is bound innermost,
--  A mysterious error will occur otherwise.

-- We pass list-building routines to the low-level queryDNFRelation function.
relToExpression :: OmegaRel -> IO (Int, Int, BoolExp)
relToExpression s = do
  (inVars, outVars, cs) <- queryDNFRelation addEq [] addGeq [] addConjunct [] s
  return (length inVars, length outVars, wrapSimplifiedExpr $ disjExpr cs)
    where
      addEq inVars outVars exVars terms constant =
          let vars = exVars ++ inVars ++ outVars
          in (constraintToExpr True vars terms constant :)

      addGeq inVars outVars exVars terms constant =
          let vars = exVars ++ inVars ++ outVars
          in (constraintToExpr False vars terms constant :)

      addConjunct _ _ exVars eqs geqs =
          wrapExistentialVars exVars eqs geqs

      hasExistentialVars = error "relToExpression: cannot create expression"

wrapExistentialVars exVars eqs geqs = (conjunct :)
    where
      conjunct =
          -- Create a conjunction of constraints, with one quantifier for each
          -- existential variable
          iterateN existsExpr (length exVars) $ conjExpr (geqs ++ eqs)
    

-- Apply a function n times
iterateN f n x = go n x
    where go 0 x = x
          go n x = go (n-1) (f x)


