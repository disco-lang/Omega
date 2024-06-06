
-- | Sets whose members are represented compactly using a
-- Presburger arithmetic formula.  This is a high-level interface to
-- 'OmegaSet'.
--
-- This module is intended to be imported qualified, e.g.
--
-- > import qualified Data.Presburger.Omega.Set as WSet

module Data.Presburger.Omega.Set
    (Set,

     -- * Building sets
     set, fromOmegaSet,

     -- * Operations on sets
     toOmegaSet,

     -- ** Inspecting
     dimension, predicate,
     lowerBoundSatisfiable,
     upperBoundSatisfiable,
     obviousTautology,
     definiteTautology,
     exact,
     inexact,
     unknown,
     equal,

     -- ** Bounds
     upperBound, lowerBound,

     -- ** Binary operations
     union, intersection, difference,
     Effort(..),
     gist,

     -- ** Unary operations
     complement,
     approximate
    )
where

import System.IO.Unsafe

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L
import Data.Presburger.Omega.LowLevel(OmegaSet, Effort(..))
import Data.Presburger.Omega.SetRel

-- | Sets of points in Z^n defined by a formula.
data Set = Set
    { setDim      :: !Int      -- ^ the number of variables
    , setExp      :: BoolExp   -- ^ a predicate defining the set
    , setOmegaSet :: OmegaSet  -- ^ low-level representation of this set
    }

instance Show Set where
    -- Generate a call to 'set'
    showsPrec n s = showParen (n >= 10) $
                    showString "set " .
                    shows (setDim s) .
                    showChar ' ' .
                    showsPrec 10 (setExp s)

-- | Create a set whose members are defined by a predicate.
--
-- The expression should have one free variable for each dimension.
--
-- For example, the set of all points on the plane is
-- 
-- >  set 2 (\[_, _] -> trueE)
-- 
-- The set of all points (x, y, z) where x > y + z is
-- 
-- >  set 3 (\[x,y,z] -> x |>| y |+| z)
--
set :: Int                      -- ^ Number of dimensions
    -> ([Var] -> BoolExp)       -- ^ Predicate defining the set
    -> Set
set dim mk_expr
    | variablesWithinRange dim expr =
        Set
        { setDim      = dim
        , setExp      = expr
        , setOmegaSet = unsafePerformIO $ mkOmegaSet dim expr
        }
    | otherwise = error "set: Variables out of range"
  where
    expr = mk_expr (takeFreeVariables dim)

mkOmegaSet :: Int -> BoolExp -> IO OmegaSet
mkOmegaSet dim expr = L.newOmegaSet dim (\vars -> expToFormula vars expr)

-------------------------------------------------------------------------------
-- Creating sets from Omega sets

-- | Convert an 'OmegaSet' to a 'Set'.
fromOmegaSet :: OmegaSet -> IO Set
fromOmegaSet oset = do
  (dim, expr) <- setToExpression oset
  return $ Set
             { setDim      = dim
             , setExp      = expr
             , setOmegaSet = oset
             }

-- | Internal function to convert an 'OmegaSet' to a 'Set', when we know
-- the set's dimension.  This can avoid actually building the expression
-- when all we want is the dimension.
omegaSetToSet :: Int -> OmegaSet -> IO Set
omegaSetToSet dim oset = return $
    Set
    { setDim      = dim
    , setExp      = unsafePerformIO $ do (_, expr) <- setToExpression oset
                                         return expr
    , setOmegaSet = oset
    }

-------------------------------------------------------------------------------
-- Using sets

-- First, some helper functions for applying OmegaSet functions to Sets

useSet :: (OmegaSet -> IO a) -> Set -> a
useSet f s = unsafePerformIO $ f (setOmegaSet s)

useSetSet :: (OmegaSet -> IO OmegaSet) -> Int -> Set -> Set
useSetSet f dim s = unsafePerformIO $ do
  omegaSetToSet dim =<< f (setOmegaSet s)

useSet2 :: (OmegaSet -> OmegaSet -> IO a) -> Set -> Set -> a
useSet2 f s1 s2 = unsafePerformIO $ f (setOmegaSet s1) (setOmegaSet s2)

useSet2Set :: (OmegaSet -> OmegaSet -> IO OmegaSet)
           -> Int
           -> Set
           -> Set
           -> Set
useSet2Set f dim s1 s2 = unsafePerformIO $ do
  omegaSetToSet dim =<< f (setOmegaSet s1) (setOmegaSet s2)

-- | Get the dimensionality of the space a set inhabits
dimension :: Set -> Int
dimension = setDim

-- | Get the predicate defining a set's members
predicate :: Set -> BoolExp
predicate = setExp

-- | Convert a 'Set' to an 'OmegaSet'.
toOmegaSet :: Set -> OmegaSet
toOmegaSet = setOmegaSet

-- | Compute the upper bound of a set by setting all UNKNOWN
--   constraints to true.
upperBound :: Set -> Set
upperBound s = useSetSet L.upperBound (setDim s) s

-- | Compute the lower bound of a set by setting all UNKNOWN
--   constraints to false.
lowerBound :: Set -> Set
lowerBound s = useSetSet L.lowerBound (setDim s) s

lowerBoundSatisfiable :: Set -> Bool
lowerBoundSatisfiable = useSet L.lowerBoundSatisfiable

upperBoundSatisfiable :: Set -> Bool
upperBoundSatisfiable = useSet L.upperBoundSatisfiable

obviousTautology :: Set -> Bool
obviousTautology = useSet L.obviousTautology

definiteTautology :: Set -> Bool
definiteTautology = useSet L.definiteTautology

-- | True if the set has no UNKNOWN constraints.
exact :: Set -> Bool
exact = useSet L.exact

-- | True if the set has UNKNOWN constraints.
inexact :: Set -> Bool
inexact = useSet L.inexact

-- | True if the set is completely UNKNOWN.
unknown :: Set -> Bool
unknown = useSet L.unknown

-- | Test whether two sets are equal.
-- The sets must have the same dimension
-- (@dimension s1 == dimension s2@), or an error will be raised.
--
-- The answer is precise if both relations are 'exact'.
-- If either relation is inexact, this function returns @False@.
equal :: Set -> Set -> Bool
equal = useSet2 L.equal

-- | Union of two sets.
-- The sets must have the same dimension
-- (@dimension s1 == dimension s2@), or an error will be raised.
union :: Set -> Set -> Set
union s1 s2 = useSet2Set L.union (setDim s1) s1 s2

-- | Intersection of two sets.
-- The sets must have the same dimension
-- (@dimension s1 == dimension s2@), or an error will be raised.
intersection :: Set -> Set -> Set
intersection s1 s2 = useSet2Set L.intersection (setDim s1) s1 s2

-- | Difference of two sets.
-- The sets must have the same dimension
-- (@dimension s1 == dimension s2@), or an error will be raised.
difference :: Set -> Set -> Set
difference s1 s2 = useSet2Set L.difference (setDim s1) s1 s2

-- | Get the gist of a set, given some background truth.  The
-- gist operator uses heuristics to simplify the set while
-- retaining sufficient information to regenerate the original by
-- re-introducing the background truth.  The sets must have the
-- same dimension.
--
-- The gist satisfies the property
--
-- > x === gist effort x given `intersection` given
gist :: Effort -> Set -> Set -> Set
gist effort s1 s2 = useSet2Set (L.gist effort) (setDim s1) s1 s2

complement :: Set -> Set
complement s = useSetSet L.complement (setDim s) s

approximate :: Set -> Set
approximate s = useSetSet L.approximate (setDim s) s
