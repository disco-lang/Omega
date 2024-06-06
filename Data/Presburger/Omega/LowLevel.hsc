
-- | This module provides a low-level interface for creating,
-- manipulating, and querying Presburger arithmetic formulae.
-- The real work is done by the C++ Omega library
-- (<http://github.com/davewathaverford/the-omega-project>).
--
-- The main data types are 'OmegaSet' and 'OmegaRel', which use a formula
-- to define a set or relation, respectively, on integer-valued points in
-- Cartesian space.
-- A typical use involves creating a Presburger arithmetic 'Formula', using
-- it to create a set or relation, and then querying the set or relation.
 
{-# OPTIONS_GHC -XForeignFunctionInterface -fwarn-incomplete-patterns
                -XEmptyDataDecls -XRankNTypes -XMultiParamTypeClasses
                -XFunctionalDependencies -XTypeSynonymInstances
                -XFlexibleInstances -XFlexibleContexts #-}

module Data.Presburger.Omega.LowLevel
    (-- * Sets and relations
     Presburger,
     OmegaSet, newOmegaSet,
     OmegaRel, newOmegaRel,

     -- * Inspecting sets and relations directly
     queryDNFSet, queryDNFRelation,

     -- * Queries on sets and relations
     lowerBoundSatisfiable, upperBoundSatisfiable,
     obviousTautology, definiteTautology,
     exact, inexact, unknown,

     -- * Creating new sets and relations from old ones

     -- ** Bounds
     upperBound, lowerBound,

     -- ** Binary operations
     equal, union, intersection, composition,
     restrictDomain, restrictRange,
     difference, crossProduct, 
     Effort(..),
     gist,

     -- ** Unary operations
     transitiveClosure,
     domain, range, inverse, complement,
     deltas, approximate,

     -- * Constructing formulas
     Formula,
     true, false,
     conjunction, disjunction, negation,
     VarHandle,
     qForall, qExists,
     Coefficient(..),
     inequality, equality
     )
where

#include "C_omega.h"

#let alignof x = "%d", __alignof__(x)

import Control.Monad
import Data.Int
import Data.List(findIndex)
import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import qualified Foreign.Marshal.Alloc as ForeignAlloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)

-------------------------------------------------------------------------------
-- Data types, classes, and functions imported from C++

-- External data types, these have the same name as in C.
data Relation                   -- A set or relation
data Form                       -- A logic formula (Formula)
data F_And                      -- A conjunction
data F_Declaration              -- A forall or exists formula
data Var_Decl                   -- A handle to a variable
data DNF_Iterator               -- Iterator over a DNF clause
data Conjunct                   -- One conjunct within a DNF clause
data Tuple_Iterator a           -- Iterator over a Tuple (in Omega library)
data EQ_Iterator                -- Iterator over a set of EQ constraints
data EQ_Handle                  -- Handle to an EQ constraint
data GEQ_Iterator               -- Iterator over a set of GEQ constraints
data GEQ_Handle                 -- Handle to a GEQ constraint
data Constr_Vars_Iter           -- Iterate over coefficients in a constraint

class Constraint a              -- The 'Constraint' base class

instance Constraint EQ_Handle
instance Constraint GEQ_Handle

-- Pointers to external data types
type C_Relation         = Ptr Relation
type C_Form             = Ptr Form
type C_And              = Ptr F_And
type C_Quantifier       = Ptr F_Declaration
type C_Var              = Ptr Var_Decl
type C_DNF_Iterator     = Ptr DNF_Iterator
type C_Conjunct         = Ptr Conjunct
type C_Tuple_Iterator a = Ptr (Tuple_Iterator a)
type C_EQ_Iterator      = Ptr EQ_Iterator
type C_EQ_Handle        = Ptr EQ_Handle
type C_GEQ_Iterator     = Ptr GEQ_Iterator
type C_GEQ_Handle       = Ptr GEQ_Handle
type C_Constr_Vars_Iter = Ptr Constr_Vars_Iter

-- | The 'gist' routine takes a parameter specifying how much effort to
-- put into generating a good result.  Higher effort takes more time.
-- It's unspecified what a given effort level does.
data Effort = Light | Moderate | Strenuous
              deriving (Eq, Show, Enum)

-- Everything containing a formula is an instance of class Logical
class Logical f where
    add_and    :: f -> IO C_Form
    add_or     :: f -> IO C_Form
    add_not    :: f -> IO C_Form
    add_forall :: f -> IO C_Quantifier
    add_exists :: f -> IO C_Quantifier
    convert_to_and :: f -> IO C_And
    finalize   :: f -> IO ()

instance Logical C_Relation where
    add_and    = hsw_relation_add_and
    add_or     = hsw_relation_add_or
    add_not    = hsw_relation_add_not
    add_forall = hsw_relation_add_forall
    add_exists = hsw_relation_add_exists
    -- We take advantage of the fact that C_And is a subclass of C_Form
    -- here, and simply cast the pointer.
    convert_to_and r = liftM castPtr $ hsw_relation_add_and r
    finalize   = hsw_relation_finalize

instance Logical C_Form where
    add_and    = hsw_formula_add_and
    add_or     = hsw_formula_add_or
    add_not    = hsw_formula_add_not
    add_forall = hsw_formula_add_forall
    add_exists = hsw_formula_add_exists
    convert_to_and = hsw_formula_to_and
    finalize   = hsw_formula_finalize

-- C_And is a subclass of C_Form and implements all its methods.
-- Consequently, we simply cast to C_Form
instance Logical C_And where
    add_and    = hsw_formula_add_and . castPtr
    add_or     = hsw_formula_add_or . castPtr
    add_not    = hsw_formula_add_not . castPtr
    add_forall = hsw_formula_add_forall . castPtr
    add_exists = hsw_formula_add_exists . castPtr
    convert_to_and = return
    finalize   = hsw_formula_finalize . castPtr

-- C_Quantifier is a subclass of C_Form and implements all its methods.
-- Consequently, we simply cast to C_Form
instance Logical C_Quantifier where
    add_and    = hsw_formula_add_and . castPtr
    add_or     = hsw_formula_add_or . castPtr
    add_not    = hsw_formula_add_not . castPtr
    add_forall = hsw_formula_add_forall . castPtr
    add_exists = hsw_formula_add_exists . castPtr
    convert_to_and = hsw_formula_to_and . castPtr
    finalize   = hsw_formula_finalize . castPtr

-- Used for freeing data that was allocated in C
foreign import ccall safe free :: Ptr a -> IO ()

foreign import ccall safe hsw_new_relation
    :: CInt -> CInt -> IO C_Relation
foreign import ccall safe hsw_new_set
    :: CInt -> IO C_Relation
foreign import ccall safe hsw_free_relation
    :: C_Relation -> IO ()
foreign import ccall "&hsw_free_relation" ptr_to_free_relation
    :: FunPtr (C_Relation -> IO ())
foreign import ccall safe hsw_relation_show
    :: C_Relation -> IO CString
foreign import ccall safe hsw_num_input_vars
    :: C_Relation -> IO CInt
foreign import ccall safe hsw_num_output_vars
    :: C_Relation -> IO CInt
foreign import ccall safe hsw_num_set_vars
    :: C_Relation -> IO CInt
foreign import ccall safe hsw_input_var
    :: C_Relation -> CInt -> IO C_Var
foreign import ccall safe hsw_output_var
    :: C_Relation -> CInt -> IO C_Var
foreign import ccall safe hsw_set_var
    :: C_Relation -> CInt -> IO C_Var
foreign import ccall safe hsw_is_lower_bound_satisfiable
    :: C_Relation -> IO Bool
foreign import ccall safe hsw_is_upper_bound_satisfiable
    :: C_Relation -> IO Bool
foreign import ccall safe hsw_is_obvious_tautology
    :: C_Relation -> IO Bool
foreign import ccall safe hsw_is_definite_tautology
    :: C_Relation -> IO Bool
foreign import ccall safe hsw_is_exact
    :: C_Relation -> IO Bool
foreign import ccall safe hsw_is_inexact
    :: C_Relation -> IO Bool
foreign import ccall safe hsw_is_unknown
    :: C_Relation -> IO Bool
foreign import ccall safe hsw_upper_bound
    :: C_Relation -> IO C_Relation
foreign import ccall safe hsw_lower_bound
    :: C_Relation -> IO C_Relation
foreign import ccall safe hsw_equal
    :: C_Relation -> C_Relation -> IO CInt
foreign import ccall safe hsw_union
    :: C_Relation -> C_Relation -> IO C_Relation
foreign import ccall safe hsw_intersection
    :: C_Relation -> C_Relation -> IO C_Relation
foreign import ccall safe hsw_composition
    :: C_Relation -> C_Relation -> IO C_Relation
foreign import ccall safe hsw_restrict_domain
    :: C_Relation -> C_Relation -> IO C_Relation
foreign import ccall safe hsw_restrict_range
    :: C_Relation -> C_Relation -> IO C_Relation
foreign import ccall safe hsw_difference
    :: C_Relation -> C_Relation -> IO C_Relation
foreign import ccall safe hsw_cross_product
    :: C_Relation -> C_Relation -> IO C_Relation
foreign import ccall safe hsw_gist
    :: C_Relation -> C_Relation -> CInt -> IO C_Relation
foreign import ccall safe hsw_transitive_closure
    :: C_Relation -> IO C_Relation
foreign import ccall safe hsw_domain
    :: C_Relation -> IO C_Relation
foreign import ccall safe hsw_range
    :: C_Relation -> IO C_Relation
foreign import ccall safe hsw_inverse
    :: C_Relation -> IO C_Relation
foreign import ccall safe hsw_complement
    :: C_Relation -> IO C_Relation
foreign import ccall safe hsw_deltas
    :: C_Relation -> IO C_Relation
foreign import ccall safe hsw_approximate
    :: C_Relation -> IO C_Relation

foreign import ccall safe hsw_relation_add_and
    :: C_Relation -> IO C_Form
foreign import ccall safe hsw_relation_add_or
    :: C_Relation -> IO C_Form
foreign import ccall safe hsw_relation_add_not
    :: C_Relation -> IO C_Form
foreign import ccall safe hsw_relation_add_forall
    :: C_Relation -> IO C_Quantifier
foreign import ccall safe hsw_relation_add_exists
    :: C_Relation -> IO C_Quantifier
foreign import ccall safe hsw_relation_finalize
    :: C_Relation -> IO ()

-- These functions take formula pointer arguments
foreign import ccall safe hsw_formula_add_and
    :: C_Form -> IO C_Form
foreign import ccall safe hsw_formula_add_or
    :: C_Form -> IO C_Form
foreign import ccall safe hsw_formula_add_not
    :: C_Form -> IO C_Form
foreign import ccall safe hsw_formula_add_forall
    :: C_Form -> IO C_Quantifier
foreign import ccall safe hsw_formula_add_exists
    :: C_Form -> IO C_Quantifier
foreign import ccall safe hsw_formula_finalize
    :: C_Form -> IO ()

foreign import ccall safe hsw_declaration_declare
    :: C_Quantifier -> IO C_Var

-- If the argument is a C_And, the argument is returned;
-- otherwise, add_and is called
foreign import ccall safe hsw_formula_to_and
    :: C_Form -> IO C_And

foreign import ccall safe hsw_add_constraint
    :: C_And -> Bool -> CInt -> Ptr CInt -> Ptr C_Var -> CInt -> IO ()

foreign import ccall safe separate_relation_dimensions
    :: Ptr C_Relation -> C_Relation -> IO ()

-- Look at the internal representation of a set
foreign import ccall safe hsw_query_dnf
    :: C_Relation -> IO C_DNF_Iterator
foreign import ccall safe hsw_dnf_iterator_next
    :: C_DNF_Iterator -> IO C_Conjunct
foreign import ccall safe hsw_dnf_iterator_free
    :: C_DNF_Iterator -> IO ()

-- For inspecting Omega data structures
foreign import ccall safe hsw_get_conjunct_variables
    :: C_Conjunct -> IO (C_Tuple_Iterator C_Var)
foreign import ccall safe hsw_tuple_iterator_next
    :: (C_Tuple_Iterator (Ptr a)) -> IO (Ptr a)
foreign import ccall safe hsw_tuple_iterator_free
    :: (C_Tuple_Iterator a) -> IO ()

foreign import ccall safe hsw_get_eqs
    :: C_Conjunct -> IO C_EQ_Iterator
foreign import ccall safe hsw_eqs_next
    :: C_EQ_Iterator -> IO C_EQ_Handle
foreign import ccall safe hsw_eqs_free
    :: C_EQ_Iterator -> IO ()
foreign import ccall safe hsw_eq_handle_free
    :: C_EQ_Handle -> IO ()

foreign import ccall safe hsw_get_geqs
    :: C_Conjunct -> IO C_GEQ_Iterator
foreign import ccall safe hsw_geqs_next
    :: C_GEQ_Iterator -> IO C_GEQ_Handle
foreign import ccall safe hsw_geqs_free
    :: C_GEQ_Iterator -> IO ()
foreign import ccall safe hsw_geq_handle_free
    :: C_GEQ_Handle -> IO ()

foreign import ccall safe hsw_constraint_get_const
    :: Ptr a -> IO #{type coefficient_t}
foreign import ccall safe hsw_constraint_get_coefficients
    :: Ptr a -> IO C_Constr_Vars_Iter
foreign import ccall safe hsw_constr_vars_next
    :: Ptr Coefficient -> C_Constr_Vars_Iter -> IO Bool 
foreign import ccall safe hsw_constr_vars_free
    :: C_Constr_Vars_Iter -> IO ()


-- For debugging
foreign import ccall safe hsw_debug_print_eq
    :: C_EQ_Handle -> IO ()
foreign import ccall safe hsw_debug_print_geq
    :: C_GEQ_Handle -> IO ()

-------------------------------------------------------------------------------
-- Marshalling from Omega Library to Haskell

-- A C++ iterator
class Iterator i a | i -> a where next :: i -> IO a

instance Iterator C_DNF_Iterator C_Conjunct where
    next = hsw_dnf_iterator_next

instance Iterator (C_Tuple_Iterator (Ptr a)) (Ptr a) where
    next = hsw_tuple_iterator_next

instance Iterator C_EQ_Iterator C_EQ_Handle where
    next = hsw_eqs_next

instance Iterator C_GEQ_Iterator C_GEQ_Handle where
    next = hsw_geqs_next

-- Imperatively accumulate over the contents of the iterator
foreach :: Iterator i (Ptr b) => (a -> Ptr b -> IO a) -> a -> i -> IO a
foreach f x iter = visit x
    where
      visit x = do y <- next iter
                   if y == nullPtr
                     then return x
                     else visit =<< f x y

-- Iterate through each conjunct in a DNF clause
iterateDNF :: (a -> C_Conjunct -> IO a) -> a -> C_Relation -> IO a
iterateDNF f x rel = do
  iter <- hsw_query_dnf rel
  x' <- foreach f x iter
  hsw_dnf_iterator_free iter
  return x'

-- Iterate through the variables in a conjunct
iterateConjVars :: (a -> C_Var -> IO a) -> a -> C_Conjunct -> IO a
iterateConjVars f x conj = do
  iter <- hsw_get_conjunct_variables conj
  x' <- foreach f x iter
  hsw_tuple_iterator_free iter
  return x'

-- Iterate through the equality constraints in a conjunct
iterateEqs :: (a -> C_EQ_Handle -> IO a) -> a -> C_Conjunct -> IO a
iterateEqs f x conj = do
  iter <- hsw_get_eqs conj
  x' <- foreach wrapped_f x iter
  hsw_eqs_free iter
  return x'
    where
      -- This wrapper just makes sure the handle is freed after use
      wrapped_f x eqHdl = do 
        x' <- f x eqHdl
        hsw_eq_handle_free eqHdl
        return x'

-- Iterate through the inequality constraints in a conjunct
iterateGeqs :: (a -> C_GEQ_Handle -> IO a) -> a -> C_Conjunct -> IO a
iterateGeqs f x conj = do
  iter <- hsw_get_geqs conj
  x' <- foreach wrapped_f x iter
  hsw_geqs_free iter
  return x'
    where
      -- This wrapper just makes sure the handle is freed after use
      wrapped_f x geqHdl = do 
        x' <- f x geqHdl
        hsw_geq_handle_free geqHdl
        return x'

-- Read the coefficients from a Constraint
peekConstraintVars :: Constraint a => Ptr a -> IO [Coefficient]
peekConstraintVars cst = do
  iter <- hsw_constraint_get_coefficients cst

  -- Allocate temporary storage on the C side for some data
  c_var_info <- ForeignAlloc.malloc

  -- Traverse and pull out values
  coeffs <- getCoefficients iter c_var_info []

  -- Free the temporary storage and the iterator
  ForeignAlloc.free c_var_info
  hsw_constr_vars_free iter

  return coeffs
    where
      getCoefficients iter c_var_info coeffs = do

        -- Read one coefficient
        ok <- hsw_constr_vars_next c_var_info iter

        -- If it returned false, we're done
        if not ok then return coeffs else do
        coeff <- peek c_var_info
        getCoefficients iter c_var_info (coeff:coeffs)

-- Read the list of input variables [N, N-1 ... 1].
-- This will probably crash if the number of variables is not specified
-- correctly.
peekInputVars, peekOutputVars, peekSetVars
    :: CInt -> C_Relation -> IO [VarHandle]
peekInputVars n rel =
    mapM (liftM VarHandle . hsw_input_var rel) [n, n - 1 .. 1]

peekOutputVars n rel =
    mapM (liftM VarHandle . hsw_output_var rel) [n, n - 1 .. 1]

peekSetVars n rel =
    mapM (liftM VarHandle . hsw_set_var rel) [n, n - 1 .. 1]

-- Helper function to read a constraint.
queryConstraint :: Constraint c =>
                   ([Coefficient] -> Int -> a -> a) -- Accumulating function
                -> a            -- Initial value
                -> Ptr c        -- Constraint to query
                -> IO a
queryConstraint f acc eq = do
  coefficients <- peekConstraintVars eq
  constant <- hsw_constraint_get_const eq
  return $ f coefficients (fromIntegral constant) acc

-------------------------------------------------------------------------------
-- Exported interface

-- | Data types containing Presburger formulae.
class Presburger a where
    -- | Extract the pointer from a formula
    pPtr :: a -> ForeignPtr Relation

    -- | Test whether two sets or relations have the same arity
    sameArity :: a -> a -> Bool

    -- | Convert a raw pointer to an OmegaSet or OmegaRel
    fromPtr :: Ptr Relation -> IO a

-- Use a wrapped relation or set
withPresburger :: Presburger a => a -> (C_Relation -> IO b) -> IO b
withPresburger p = withForeignPtr (pPtr p)

-- Use two wrapped relations or sets
withPresburger2 :: (Presburger a, Presburger b) =>
                   a -> b -> (C_Relation -> C_Relation -> IO c) -> IO c
withPresburger2 p q f = withForeignPtr (pPtr p) $ \ptr ->
                        withForeignPtr (pPtr q) $ \ptr2 ->
                        f ptr ptr2

-- | A set of points in Z^n.
-- This is a wrapper around the Omega library's Relation type.  
data OmegaSet = OmegaSet { sPtr :: {-# UNPACK #-} !(ForeignPtr Relation)
                         , sDom :: [VarHandle]
                         }

instance Show OmegaSet where
    show rel = unsafePerformIO $ withPresburger rel $ \ptr -> do
        -- Call hsw_relation_show to get a C string, then convert to String
        cStr <- hsw_relation_show ptr
        str  <- peekCString cStr
        free cStr
        return str

instance Presburger OmegaSet where
    pPtr = sPtr

    sameArity s1 s2 =
      length (sDom s1) == length (sDom s2)

    fromPtr ptr = do
      numVars <- hsw_num_set_vars ptr
      varIDs <- peekSetVars numVars ptr
      wrapOmegaSet ptr varIDs

-- Convert a raw set pointer to an OmegaSet
wrapOmegaSet :: C_Relation -> [VarHandle] -> IO OmegaSet
wrapOmegaSet ptr dom = do
  foreignptr <- newForeignPtr ptr_to_free_relation ptr
  return $! OmegaSet { sPtr = foreignptr
                     , sDom = dom
                     }

-- | Create an Omega set.  The first parameter is the number of dimensions
-- the set inhabits.  The second parameter builds a formula
-- defining the set's members. The set's members are those points
-- for which the formula evaluates to True.
newOmegaSet :: Int              -- ^ Dimensionality of the space that the set
                                -- inhabits
            -> ([VarHandle] -> Formula) -- ^ Set members
            -> IO OmegaSet
newOmegaSet numVars init = do
  rel <- hsw_new_set (fromIntegral numVars)

  -- Look up the ID for each variable in the tuple.  Variables are ordered
  -- from last to first because the last variable is "innermost," has
  -- de Bruijn index 1, and belongs at position 1 in the list.
  freeVarIDs <- peekSetVars (fromIntegral numVars) rel

  runFD (init freeVarIDs) rel
  wrapOmegaSet rel freeVarIDs

-- | Inspect a set's low-level representation directly.  This function
-- takes care of data structure traversal and relies on other routines to
-- interpret the data.
--
-- All three accumulating functions take the set variables as their
-- first parameter, and any existentially quantified variables as
-- their second parameter.  The set variables are returned along with
-- a result value.
queryDNFSet :: ([VarHandle] -> [VarHandle] -> [Coefficient] -> Int -> a -> a)
               -- ^ Accumulating function for equality constraints
            -> a                -- ^ Initial value for equality constraints
            -> ([VarHandle] -> [VarHandle] -> [Coefficient] -> Int -> b -> b)
               -- ^ Accumulating function for inequality constraints
            -> b                -- ^ Initial value for inequality constraints
            -> ([VarHandle] -> [VarHandle] -> a -> b -> c -> c)
               -- ^ Accumulating function for conjuncts
            -> c                -- ^ Initial value for conjuncts
            -> OmegaSet         -- ^ Set to query
            -> IO ([VarHandle], c)
queryDNFSet readEq unitEq readGeq unitGeq readConj unitConj s = do
    conjuncts <- withPresburger s $ iterateDNF doConjunct unitConj
    return (sDom s, conjuncts)
    where
      doConjunct acc conjunct = do
        -- Find existentially bound variables in this conjunct, which
        -- Omega calls "wildcard variables"
        wildcardVars <- iterateConjVars findWildcards [] conjunct
        let wc = map VarHandle wildcardVars

        -- For each EQ relation, get the relation
        eqs <- iterateEqs (queryConstraint $ readEq (sDom s) wc)
               unitEq conjunct

        -- For each GE relation, get the relation
        geqs <- iterateGeqs (queryConstraint $ readGeq (sDom s) wc)
                unitGeq conjunct

        return $ readConj (sDom s) wc eqs geqs acc

      findWildcards acc var =
          -- Is this an input variable?
          case findIndex (var ==) (map unVarHandle $ sDom s)
          of Just n  -> return acc
             Nothing -> -- Otherwise, assume it's a wildcard
                        -- FIXME: call into C to check the variable's kind
                        return $ var : acc

-- | A relation from points in a /domain/ Z^m
-- to points in a /range/ Z^n.
-- This is a wrapper around the Omega library's Relation type.
--
-- A relation can be considered as just a set of points in Z^(m+n).
-- However, many routines treat the domain and range differently.

data OmegaRel = OmegaRel { rPtr :: {-# UNPACK #-} !(ForeignPtr Relation)
                         , rDom :: [VarHandle]
                         , rRng :: [VarHandle]
                         }

instance Show OmegaRel where
    show rel = unsafePerformIO $ withPresburger rel $ \ptr -> do
        -- Call hsw_relation_show to get a C string, then convert to String
        cStr <- hsw_relation_show ptr
        str  <- peekCString cStr
        free cStr
        return str

instance Presburger OmegaRel where
    pPtr = rPtr

    sameArity r1 r2 =
      length (rDom r1) == length (rDom r2) &&
      length (rRng r1) == length (rRng r2)

    fromPtr ptr = do
      numOutputs <- hsw_num_output_vars ptr
      outputVarIDs <- peekOutputVars numOutputs ptr

      numInputs <- hsw_num_input_vars ptr
      inputVarIDs <- peekInputVars numInputs ptr

      wrapOmegaRel ptr inputVarIDs outputVarIDs

-- Convert a raw relation pointer to an OmegaSet
wrapOmegaRel :: C_Relation -> [VarHandle] -> [VarHandle] -> IO OmegaRel
wrapOmegaRel ptr dom rng = do
  foreignptr <- newForeignPtr ptr_to_free_relation ptr
  return $! OmegaRel { rPtr = foreignptr
                     , rDom = dom
                     , rRng = rng }

-- | Create an Omega relation.  The first two parameters are the number
-- of dimensions of the domain and range, respectively.  The third parameter
-- builds a formula defining the relation.  Two points are related iff the
-- formula evaluates to True on those points.
newOmegaRel :: Int              -- ^ Dimensionality of the domain
            -> Int              -- ^ Dimensionality of the range
            -> ([VarHandle] -> [VarHandle] -> Formula)
                                -- ^ The relation
            -> IO OmegaRel
newOmegaRel numInputs numOutputs init = do
  rel <- hsw_new_relation (fromIntegral numInputs) (fromIntegral numOutputs)

  -- Look up the IDs for the input and output variables.
  outputVarIds <- peekOutputVars (fromIntegral numOutputs) rel
  inputVarIds <- peekInputVars (fromIntegral numInputs) rel

  runFD (init inputVarIds outputVarIds) rel
  wrapOmegaRel rel inputVarIds outputVarIds

-- | Inspect a relation's low-level representation directly.  This function
-- takes care of data structure traversal and relies on other routines to
-- interpret the data.
--
-- All three accumulating functions take the relation's input and
-- output variables as their first and second parameters, respectively,
-- and any existentially quantified variables as
-- their second parameter.  The relation's input and output variables are
-- returned along with a result value.
queryDNFRelation :: ([VarHandle] -> [VarHandle] -> [VarHandle] -> [Coefficient] -> Int -> a -> a)
                    -- ^ Accumulating function for equality constraints
                 -> a           -- ^ Initial value for equality constraints
                 -> ([VarHandle] -> [VarHandle] -> [VarHandle] -> [Coefficient] -> Int -> b -> b)
                    -- ^ Accumulating function for inequality constraints
                 -> b           -- ^ Initial value for inequality constraints
                 -> ([VarHandle] -> [VarHandle] -> [VarHandle] -> a -> b -> c -> c)
                    -- ^ Accumulating function for conjuncts
                 -> c           -- ^ Initial value for conjuncts
                 -> OmegaRel    -- ^ Relation to query
                 -> IO ([VarHandle], [VarHandle], c) -- ^ Input variables, output variables, and result
queryDNFRelation readEq unitEq readGeq unitGeq readConj unitConj r = do
    conjuncts <- withPresburger r $ iterateDNF doConjunct unitConj
    return (rDom r, rRng r, conjuncts)
    where
      doConjunct acc conjunct = do
        -- Find existentially bound variables in this conjunct, which
        -- Omega calls "wildcard variables"
        wildcardVars <- iterateConjVars findWildcards [] conjunct
        let wc = map VarHandle wildcardVars

        -- For each EQ relation, get the relation
        eqs <- iterateEqs (queryConstraint $ readEq (rDom r) (rRng r) wc)
               unitEq conjunct

        -- For each GE relation, get the relation
        geqs <- iterateGeqs (queryConstraint $ readGeq (rDom r) (rRng r) wc)
                unitGeq conjunct

        return $ readConj (rDom r) (rRng r) wc eqs geqs acc

      findWildcards acc var =
          -- Is this an input variable?
          case findIndex (var ==) (map unVarHandle $ rDom r ++ rRng r)
          of Just n  -> return acc
             Nothing -> -- Otherwise, assume it's a wildcard
                        -- FIXME: call into C to check the variable's kind
                        return $ var : acc

-- | Get a list of relations, one per output variable, with the same
-- input and output dimensions as the original, but whose constraints
-- mention only one output variable and no existential constraints.
--
-- This function is needed to create a high-level Rel from a low-level
-- OmegaRel.
separateRelationDimensions :: OmegaRel -> IO [OmegaRel]
separateRelationDimensions r = do
  -- Allocate an array to store outputs
  allocaArray numOutputs $ \outputArray -> do
    -- Call into C
    withPresburger r $ \ptr -> separate_relation_dimensions outputArray ptr

    -- Wrap each output as a relation
    mapM readRelation =<< peekArray numOutputs outputArray

    where
      numInputs  = length $ rDom r
      numOutputs = length $ rRng r

      readRelation rel = do
        -- Look up the IDs for the input and output variables.
        outputVarIds <- peekOutputVars (fromIntegral numOutputs) rel
        inputVarIds <- peekInputVars (fromIntegral numInputs) rel

        wrapOmegaRel rel inputVarIds outputVarIds

-------------------------------------------------------------------------------
-- Queries

-- | Determine a lower bound on whether the formula is satisfiable.
-- The lower bound is based on treating all UNKNOWN constraints as false.
lowerBoundSatisfiable :: Presburger a => a -> IO Bool

-- | Determine an upper bound on whether the formula is satisfiable.
-- The upper bound is based on treating all UNKNOWN constraints as true.
upperBoundSatisfiable :: Presburger a => a -> IO Bool

-- | Use simple, fast tests to decide whether the formula is a tautology.
obviousTautology      :: Presburger a => a -> IO Bool

-- | True if the formula is a tautology.
definiteTautology     :: Presburger a => a -> IO Bool

-- | True if the formula has no UNKNOWN constraints.
exact                 :: Presburger a => a -> IO Bool

-- | True if the formula has UNKNOWN constraints.
inexact               :: Presburger a => a -> IO Bool

-- | True if the formula is UNKNOWN.
unknown               :: Presburger a => a -> IO Bool

lowerBoundSatisfiable rel = withPresburger rel hsw_is_lower_bound_satisfiable
upperBoundSatisfiable rel = withPresburger rel hsw_is_upper_bound_satisfiable
obviousTautology rel      = withPresburger rel hsw_is_obvious_tautology
definiteTautology rel     = withPresburger rel hsw_is_definite_tautology
exact rel                 = withPresburger rel hsw_is_exact
inexact rel               = withPresburger rel hsw_is_inexact
unknown rel               = withPresburger rel hsw_is_unknown

-------------------------------------------------------------------------------
-- Creating new sets and relations from old ones

-- | Compute the upper bound of a set or relation by setting all UNKNOWN
-- constraints to true.
upperBound :: Presburger a => a -> IO a
upperBound rel = fromPtr =<< withPresburger rel hsw_upper_bound

-- | Compute the lower bound of a set or relation by setting all UNKNOWN
-- constraints to false.
lowerBound :: Presburger a => a -> IO a
lowerBound rel = fromPtr =<< withPresburger rel hsw_lower_bound

-- | Test whether two sets or relations are equal.
-- The sets or relations must have the same arity.
--
-- The answer is precise if both arguments are 'exact'.
-- If either argument is inexact, this function returns @False@.
equal :: Presburger a => a -> a -> IO Bool
equal rel1 rel2
    | sameArity rel1 rel2 = do
          eq <- withPresburger2 rel1 rel2 hsw_equal
          return $! eq /= 0
    | otherwise = error "equal: arguments have different arities"

-- | Compute the union of two sets or relations.  The sets or relations
-- must have the same arity.
union :: Presburger a => a -> a -> IO a
union rel1 rel2
    | sameArity rel1 rel2 =
          fromPtr =<< withPresburger2 rel1 rel2 hsw_union 
    | otherwise = error "union: arguments have different arities"

-- | Compute the intersection of two sets or relations.  The sets or relations
-- must have the same arity.
intersection :: Presburger a => a -> a -> IO a
intersection rel1 rel2
    | sameArity rel1 rel2 =
          fromPtr =<< withPresburger2 rel1 rel2 hsw_intersection
    | otherwise = error "intersection: arguments have different arities"

-- | Compute the composition of two sets or relations.  The
-- first relation's domain must be the same dimension as the second's range.
composition :: OmegaRel -> OmegaRel -> IO OmegaRel
composition rel1 rel2
    | length (rDom rel1) == length (rRng rel2) =
          fromPtr =<< withPresburger2 rel1 rel2 hsw_composition
    | otherwise = error "composition: argument arities do not agree"

-- | Restrict the domain of a relation.
--   If @domain r@ is @d@, then @domain =<< restrictDomain r s@
--   is @intersection d s@.
restrictDomain :: OmegaRel -> OmegaSet -> IO OmegaRel
restrictDomain rel1 set
    | length (rDom rel1) == length (sDom set) =
          fromPtr =<< withPresburger2 rel1 set hsw_restrict_domain
    | otherwise = error "restrictDomain: argument arities do not agree"

-- | Restrict the range of a relation.
--   If @range r@ is @d@, then @range =<< restrictRange r s@
--   is @intersection d s@.
restrictRange :: OmegaRel -> OmegaSet -> IO OmegaRel
restrictRange rel1 set
    | length (rDom rel1) == length (sDom set) =
          fromPtr =<< withPresburger2 rel1 set hsw_restrict_range
    | otherwise = error "restrictRange: argument arities do not agree"

-- | Compute the difference of two relations or sets.
--   The members of @difference x y@ are the members of @x@ that are not
--   members of @y@.
difference :: Presburger a => a -> a -> IO a
difference rel1 rel2
    | sameArity rel1 rel2 =
        fromPtr =<< withPresburger2 rel1 rel2 hsw_difference
    | otherwise = error "difference: arguments have different arities"

-- | Compute the cross product of two sets.
--   The cross product relates every element of @s@ to every element of @y@.
crossProduct :: OmegaSet -> OmegaSet -> IO OmegaRel
crossProduct set1 set2 =
    fromPtr =<< withPresburger2 set1 set2 hsw_cross_product

-- | Get the gist of a set or relation, given some background truth.  The
-- gist operator uses heuristics to make a set or relation simpler, while
-- still retaining sufficient information to regenerate the original by
-- re-introducing the background truth.  The sets or relations
-- must have the same arity.
--
-- Given @x@ computed by
--
-- > x <- intersection given =<< gist effort r given
--
-- we have @x == r@.
gist :: Presburger a => Effort -> a -> a -> IO a
gist effort rel given
    | sameArity rel given =
        withPresburger2 rel given $ \ptr ptrGiven ->
          fromPtr =<< hsw_gist ptr ptrGiven (fromIntegral $ fromEnum effort)
    | otherwise = error "gist: arguments have different arities"

-- | Get the transitive closure of a relation.  In some cases, the transitive
-- closure cannot be computed exactly, in which case a lower bound is
-- returned.
transitiveClosure :: OmegaRel -> IO OmegaRel
transitiveClosure rel = fromPtr =<< withPresburger rel hsw_transitive_closure

-- | Get the domain of a relation.
domain :: OmegaRel -> IO OmegaSet
domain rel = fromPtr =<< withPresburger rel hsw_domain

-- | Get the range of a relation.
range :: OmegaRel -> IO OmegaSet
range rel = fromPtr =<< withPresburger rel hsw_range

-- | Get the inverse of a relation.
inverse :: OmegaRel -> IO OmegaRel
inverse rel = fromPtr =<< withPresburger rel hsw_inverse

-- | Get the complement of a set or relation.
complement :: Presburger a => a -> IO a
complement rel = fromPtr =<< withPresburger rel hsw_complement

-- | Get the deltas of a relation.
-- The relation's input dimensionality must be the same as its output
-- dimensionality.
deltas :: OmegaRel -> IO OmegaSet
deltas rel
    | length (rDom rel) == length (rRng rel) =
        fromPtr =<< withPresburger rel hsw_deltas
    | otherwise =
        error "deltas: relation has different input and output dimensionality"

-- | Approximate a set or relation by allowing all existentially quantified
-- variables to take on rational values.  This allows these variables to be
-- eliminated from the formula.
approximate :: Presburger a => a -> IO a
approximate rel = fromPtr =<< withPresburger rel hsw_approximate

-------------------------------------------------------------------------------
-- Formulae

-- | A boolean-valued Presburger formula.

-- This is actually a function that builds a Presburger formula.
newtype Formula = FD {runFD :: forall a. Logical a => a -> IO ()}

-- | Logical conjunction (and).
conjunction :: [Formula] -> Formula
conjunction formulaDefs = FD $ \f -> do
  newF <- add_and f
  mapM_ (\func -> runFD func newF) formulaDefs
  finalize newF

-- | Logical disjunction (or).
disjunction :: [Formula] -> Formula
disjunction formulaDefs = FD $ \f -> do
  newF <- add_or f
  mapM_ (\func -> runFD func newF) formulaDefs
  finalize newF

-- | Logical negation.
negation :: Formula -> Formula
negation formulaDef = FD $ \f -> do
  newF <- add_not f
  runFD formulaDef newF
  finalize newF

-- | Universal quantification.  The 'VarHandle' parameter is the variable
-- bound by the quantifier.
qForall :: (VarHandle -> Formula) -> Formula
qForall makeBody = FD $ \f -> do
  newFormula <- add_forall f
  localVar <- hsw_declaration_declare newFormula
  runFD (makeBody (VarHandle localVar)) newFormula
  finalize newFormula

-- | Existential quantification.  The 'VarHandle' parameter is the variable
-- bound by the quantifier.
qExists :: (VarHandle -> Formula) -> Formula
qExists makeBody = FD $ \f -> do
  newFormula <- add_exists f
  localVar <- hsw_declaration_declare newFormula
  runFD (makeBody (VarHandle localVar)) newFormula
  finalize newFormula

-- Add an equality or inequality constraint to a conjunction.
addConstraint :: Bool -> [Coefficient] -> Int -> C_And -> IO ()
addConstraint kind terms constant formula = do
  let numTerms     = length terms
      numTermsCInt = fromIntegral numTerms
      constantCInt = fromIntegral constant
      coefficients = map (fromIntegral . coeffValue) terms
      variables    = map ((\(VarHandle h) -> h) . coeffVar) terms

  -- Marshal the coefficients and variables to C as arrays
  withArray coefficients $ \coeffPtr ->
      withArray variables $ \varPtr ->
          -- then, call code to set the constraint
          hsw_add_constraint formula kind numTermsCInt coeffPtr varPtr constantCInt

-- | Construct an inequation of the form @a*x + b*y + ... + d >= 0@.
inequality :: [Coefficient] -> Int -> Formula
inequality terms constant = FD $ \formula ->
    addConstraint False terms constant =<< convert_to_and formula

-- | Construct an equation of the form @a*x + b*y + ... + d = 0@.
equality :: [Coefficient] -> Int -> Formula
equality terms constant = FD $ \formula ->
    addConstraint True terms constant =<< convert_to_and formula

-- | Truth.
true :: Formula
true = equality [] 0

-- | Falsity.
false :: Formula
false = equality [] 1

-- | A variable in a formula.

-- These data structures are owned by OmegaSet or OmegaRel instances,
-- which take care of allocation and deallocation.
newtype VarHandle = VarHandle { unVarHandle :: C_Var } deriving(Eq)

-- | An integer-valued term @n*v@ in a formula.
data Coefficient =
    Coefficient { coeffVar :: {-# UNPACK #-} !VarHandle
                , coeffValue :: {-# UNPACK #-} !Int
                }

instance Show Coefficient where
    show (Coefficient v n) =
        "(" ++ show n ++ " * " ++ show (unVarHandle v) ++ ")"

instance Storable Coefficient where
    sizeOf _ = #{size Variable_Info_struct}
    alignment _ = #{alignof Variable_Info_struct}
    peek p = do
      var  <- #{peek Variable_Info_struct, var} p :: IO C_Var
      coef <- #{peek Variable_Info_struct, coef} p :: IO #{type coefficient_t}
      return $ Coefficient { coeffVar = VarHandle var
                           , coeffValue = fromIntegral coef
                           }

