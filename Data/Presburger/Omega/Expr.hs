
-- | Expressions are the high-level interface for creating Presburger
-- formulae.  As in Presburger arithmetic, expressions can
-- represent addition, subtraction, quantification, inequalities, and boolean
-- operators.
--
-- Expressions allow formulas to be input in a freeform manner.  When
-- converted to a formula with 'expToFormula', they will be simplified to a
-- form that the underlying library can use.
-- Multplication is unrestricted; however, if an
-- expression involves the product of two non-constant terms, it cannot be
-- converted to a formula.
-- 
-- This module handles expressions and converts them to formulas.
-- Sets and relations are managed by the "Data.Presburger.Omega.Set"
-- and "Data.Presburger.Omega.Rel" modules.

{-# OPTIONS_GHC -XBangPatterns
                -XTypeFamilies
                -XEmptyDataDecls
                -XFlexibleInstances
                -XFlexibleContexts
                -XUndecidableInstances #-}
module Data.Presburger.Omega.Expr
    (-- * Expressions
     Exp, IntExp, BoolExp,
     Var,

     -- ** Construction
     nthVariable, takeFreeVariables, takeFreeVariables',
     varE, nthVarE, intE, boolE, trueE, falseE, negateE,
     sumE, prodE, notE, conjE, disjE,
     (|&&|),
     sumOfProductsE,
     (|+|), (|-|), (|*|), (*|),
     isZeroE, isNonnegativeE,
     (|==|), (|/=|), (|>|), (|>=|), (|<|), (|<=|),
     forallE, existsE,

     -- ** Destruction
     foldIntExp, foldBoolExp,

     -- ** Internal data structures
     --
     -- | These are exported to allow other modules to build the low-level
     -- representation of expressions, and avoid the cost of simplifying
     -- expressions.  Normally, the 'Exp' functions are sufficient.
     Expr, IntExpr, BoolExpr,
     PredOp(..),
     Quantifier(..),
     wrapExpr, wrapSimplifiedExpr,
     varExpr, sumOfProductsExpr, conjExpr, disjExpr, testExpr, existsExpr,

     -- ** Operations on expressions
     expEqual,
     expToFormula,

     -- ** Manipulating variables
     rename,
     adjustBindings,
     variablesWithinRange,
    )
where

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.IntMap as IntMap
import Data.IntMap(IntMap)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Unique
import Debug.Trace
import System.IO.Unsafe

import Data.Presburger.Omega.LowLevel

infixl 7 |*|
infixl 7 *|
infixl 6 |+|, |-|
infix 4 |>|, |>=|, |<|, |<=|, |==|, |/=|
infixr 3 |&&|

-- | Integer and boolean-valued expressions.

-- Expressions can convert themselves to a normalized form, under the hood.
-- Normalization is only done when necessary.  The IORef allows an expression
-- to be updated with its normalized form.
newtype Exp t = Exp (IORef (ExprBox t))

type IntExp = Exp Int
type BoolExp = Exp Bool

instance Show (Exp Int) where
    showsPrec n e =
        showsIntExprPrec emptyShowsEnv n (getSimplifiedExpr e)

instance Show (Exp Bool) where
    showsPrec n e =
        showsBoolExprPrec emptyShowsEnv n (getSimplifiedExpr e)

-- | We keep track of whether an expression is simplified.
data ExprBox t =
    ExprBox
    { isSimplified :: {-# UNPACK #-} !Bool
    , expression   :: !(Expr t)
    }

-- | Get an expression, without trying to simplify it.
getExpr :: Exp t -> Expr t
getExpr (Exp ref) = expression $ unsafePerformIO $ readIORef ref

-- | Get the simplified form of an expression.
getSimplifiedExpr :: Exp t -> Expr t
getSimplifiedExpr (Exp ref) =
    unsafePerformIO $ readIORef ref >>= simplifyAndUpdate
    where
      simplifyAndUpdate (ExprBox True  e) = return e
      simplifyAndUpdate (ExprBox False e) =
          let e'     = simplify e
              newBox = ExprBox True e'
          in do writeIORef ref $! newBox
                return e'

-- | Wrap an expression.
wrapExpr :: Expr t -> Exp t
wrapExpr e = Exp $ unsafePerformIO $ newIORef (ExprBox False e)

-- | Wrap an expression that is known to be in simplified form.
wrapSimplifiedExpr :: Expr t -> Exp t
wrapSimplifiedExpr e = Exp $ unsafePerformIO $ newIORef (ExprBox True e)

-- 'wrap' and 'get' are inverses
{-# RULES
"wrap/getExpr"		 forall x. getExpr (wrapExpr x) = x
"wrapSimplified/getExpr" forall x. getExpr (wrapSimplifiedExpr x) = x
"wrap/getSimplifiedExpr" forall x. getSimplifiedExpr (wrapSimplifiedExpr x) = x
 #-}

-- | Variables.  Variables are represented internally by de Bruijn indices.

-- Variables are represented by a de Bruijn index.  The "innermost" variable
-- is zero, and outer variables have higher indices.

-- The 'Quantified' constructor is used temporarily when building a quantified
-- expression.  It is only seen by 'rename' and 'adjustBindings'.
data Var = Bound {-# UNPACK #-} !Int
         | Quantified !Unique
           deriving(Eq, Ord)

-- | Produce the Nth bound variable.  Zero is the innermost variable index.
nthVariable :: Int -> Var
nthVariable = Bound

-- | Construct a new quantified variable.
newQuantified :: IO Var
newQuantified = do u <- newUnique
                   return (Quantified u)

freeVariables :: [Var]
freeVariables = map Bound [0..]

-- | Produce a set of variables to use as free variables in an expression.
-- This produces the list @[nthVariable 0, nthVariable 1, ...]@
takeFreeVariables :: Int -> [Var]
takeFreeVariables n = take n freeVariables

-- | Like 'takeFreeVariables', but produce the expression corresponding to
-- each variable.
takeFreeVariables' :: Int -> [IntExp]
takeFreeVariables' n = map varE $ take n freeVariables

-------------------------------------------------------------------------------
-- Building expressions

varE :: Var -> IntExp
varE v = wrapExpr $ VarE v

nthVarE :: Int -> IntExp
nthVarE n = varE (nthVariable n)

intE :: Int -> IntExp
intE n = wrapExpr $ LitE n

boolE :: Bool -> BoolExp
boolE b = wrapExpr $ LitE b

trueE, falseE :: BoolExp
trueE = boolE True
falseE = boolE False

-- | Multiplication by -1
negateE :: IntExp -> IntExp
negateE e = wrapExpr $ CAUE Prod (-1) [getExpr e]

-- | Summation
sumE :: [IntExp] -> IntExp
sumE es = wrapExpr $ CAUE Sum 0 $ map getExpr es

-- | Multiplication
prodE :: [IntExp] -> IntExp
prodE es = wrapExpr $ CAUE Prod 1 $ map getExpr es

-- | Logical negation
notE :: BoolExp -> BoolExp
notE e = wrapExpr $ NotE $ getExpr e

-- | Conjunction
conjE :: [BoolExp] -> BoolExp
conjE es = wrapExpr $ CAUE Conj True $ map getExpr es

-- | Disjunction
disjE :: [BoolExp] -> BoolExp
disjE es = wrapExpr $ CAUE Disj False $ map getExpr es

-- | Conjunction
(|&&|) :: BoolExp -> BoolExp -> BoolExp
e |&&| f = wrapExpr $ CAUE Conj True [getExpr e, getExpr f]

-- | Add
(|+|) :: IntExp -> IntExp -> IntExp
e |+| f = sumE [e, f]

-- | Subtract
(|-|) :: IntExp -> IntExp -> IntExp
e |-| f = sumE [e, negateE f]

-- | Multiply
(|*|) :: IntExp -> IntExp -> IntExp
e |*| f = prodE [e, f]

-- | Multiply by an integer
(*|) :: Int -> IntExp -> IntExp
n *| f = wrapExpr $ CAUE Prod n [getExpr f]

-- | Test whether an integer expression is zero
isZeroE :: IntExp -> BoolExp
isZeroE e = wrapExpr $ PredE IsZero $ getExpr e

-- | Test whether an integer expression is nonnegative
isNonnegativeE :: IntExp -> BoolExp
isNonnegativeE e = wrapExpr $ PredE IsGEZ $ getExpr e

-- | Equality test
(|==|) :: IntExp -> IntExp -> BoolExp
e |==| f = isZeroE (e |-| f)

-- | Inequality test
(|/=|) :: IntExp -> IntExp -> BoolExp
e |/=| f = disjE [e |>| f, e |<| f]

-- | Greater than
(|>|) :: IntExp -> IntExp -> BoolExp
e |>| f = isNonnegativeE (wrapExpr $ CAUE Sum (-1) [getExpr e, getExpr $ negateE f])

-- | Less than
(|<|) :: IntExp -> IntExp -> BoolExp
e |<| f = f |>| e

-- | Greater than or equal
(|>=|) :: IntExp -> IntExp -> BoolExp
e |>=| f = isNonnegativeE (e |-| f)

-- | Less than or equal
(|<=|) :: IntExp -> IntExp -> BoolExp
e |<=| f = f |>=| e

-- | Build a universally quantified formula.
forallE :: (Var -> BoolExp) -> BoolExp
forallE f = wrapExpr $ QuantE Forall $ getExpr $ withFreshVariable f

-- | Build an existentially quantified formula.
existsE :: (Var -> BoolExp) -> BoolExp
existsE f = wrapExpr $ QuantE Exists $ getExpr $ withFreshVariable f

-- | Reduce an integer expression to a value.  Values for free variables
-- are provided explicitly in an environment.
foldIntExp :: forall a.
              (Int -> [a] -> a)   -- ^ summation
           -> (Int -> [a] -> a)   -- ^ multiplication
           -> (Int -> a)          -- ^ integer literal
           -> [a]                 -- ^ environment
           -> (IntExp -> a)
foldIntExp sumE prodE litE env expression =
    foldIntExp' sumE prodE litE env (getSimplifiedExpr expression)

foldIntExp' :: forall a.
               (Int -> [a] -> a)   -- ^ summation
            -> (Int -> [a] -> a)   -- ^ multiplication
            -> (Int -> a)          -- ^ integer literal
            -> [a]                -- ^ environment
            -> (Expr Int -> a)
foldIntExp' sumE prodE litE env expression = rec env expression
    where
      rec :: forall. [a] -> Expr Int -> a
      rec env expression =
          case expression
          of CAUE Sum  lit es -> sumE lit $ map (rec env) es
             CAUE Prod lit es -> prodE lit $ map (rec env) es
             LitE n           -> litE n
             VarE (Bound i)   -> env `genericIndex` i
             VarE _           -> error "Expr.fold: unexpected variable"

      -- Like (!!), but throws a useful error message
      index (x:_)  0 = x
      index (_:xs) n = index xs (n-1)
      index []     _ = error "Expr.fold: variable index out of range"

-- | Reduce a boolean expression to a value.  Values for free variables
-- are provided explicitly in an environment.
foldBoolExp :: forall a b.
               (Int -> [b] -> b)  -- ^ summation
            -> (Int -> [b] -> b)  -- ^ multiplication
            -> (Int -> b)         -- ^ integer literal
            -> ([a] -> a)         -- ^ disjunction
            -> ([a] -> a)         -- ^ conjunction
            -> (a -> a)           -- ^ negation
            -> (Quantifier -> (b -> a) -> a) -- ^ quantification
            -> (PredOp -> b -> a) -- ^ an integer predicate
            -> a                  -- ^ true
            -> a                  -- ^ false
            -> [b]                -- ^ environment
            -> (BoolExp -> a)
foldBoolExp sumE prodE litE orE andE notE quantE predE trueE falseE
            env expression = rec env (getSimplifiedExpr expression)
    where
      rec :: forall. [b] -> Expr Bool -> a
      rec env expression =
          case expression
          of CAUE Disj True  es -> trueE
             CAUE Disj False es -> orE $ map (rec env) es
             CAUE Conj True  es -> andE $ map (rec env) es
             CAUE Conj False es -> falseE
             PredE pred e       -> predE pred (integral env e)
             NotE e             -> notE (rec env e)
             LitE True          -> trueE
             LitE False         -> falseE
             QuantE q e         -> quantE q (quantifier env e)

      -- Handle a quantifier: the variable gets the specified value
      quantifier env e value = rec (value:env) e

      -- Call foldIntExp for integer expressions
      integral env e = foldIntExp' sumE prodE litE env e

-- | Use a fresh variable in an expression.  After the expression is
-- constructed, rename/adjust variable indices so that the fresh variable
-- has index 0 and all other free variables' indices are incremented
-- by 1.
withFreshVariable :: (Var -> Exp t) -> Exp t
withFreshVariable f = unsafePerformIO $ do
  v <- newQuantified
  return $ rename v (Bound 0) $ adjustBindings 0 1 $ f v

-------------------------------------------------------------------------------

-- | The internal representation of expressions.
data Expr t where
    -- Application of a commutative and associative operator
    CAUE :: !(CAUOp t)          -- operator
         -> !t                  -- literal operand
         -> [Expr t]             -- other operands
         -> Expr t

    -- A predicate on an integer expression
    PredE :: !PredOp            -- operator
          -> Expr Int            -- integer operand
          -> Expr Bool

    -- Boolean negation
    NotE :: Expr Bool -> Expr Bool

    -- A literal
    LitE :: !t -> Expr t

    -- A variable.  Only integer-valued variables are permitted.
    VarE :: !Var -> Expr Int

    -- An expression quantified over an integer variable
    QuantE :: !Quantifier -> Expr Bool -> Expr Bool

type IntExpr = Expr Int
type BoolExpr = Expr Bool

-- | A commutative and associative operator with a unit.
-- The type parameter 't' gives the operator's parameter and return type.
data CAUOp t where
    Sum  :: CAUOp Int
    Prod :: CAUOp Int
    Conj :: CAUOp Bool 
    Disj :: CAUOp Bool

instance Eq (CAUOp t) where
    Sum  == Sum  = True
    Prod == Prod = True
    Conj == Conj = True
    Disj == Disj = True
    _    == _    = False

instance Show (CAUOp t) where
    show Sum  = "Sum"
    show Prod = "Prod"
    show Conj = "Conj"
    show Disj = "Disj"

-- | A predicate on an integer expresion.
data PredOp = IsZero | IsGEZ
              deriving(Eq, Show)

-- Quantifiers.
data Quantifier = Forall | Exists
                  deriving(Eq, Show)

varExpr :: Var -> IntExpr
varExpr = VarE

-- | Create a sum of products expression
sumOfProductsE :: Int           -- ^ constant part of sum
               -> [(Int, [Var])] -- ^ product terms
               -> IntExp
sumOfProductsE n prods = wrapSimplifiedExpr $ CAUE Sum n $ map prod prods
    where
      prod (n, vars) = CAUE Prod n $ map VarE vars

sumOfProductsExpr :: Int           -- ^ constant part of sum
                  -> [(Int, [Var])] -- ^ product terms
                  -> IntExpr
sumOfProductsExpr n prods = CAUE Sum n $ map prod prods
    where
      prod (n, vars) = CAUE Prod n $ map VarE vars

testExpr :: PredOp -> IntExpr -> BoolExpr
testExpr p e = PredE p e

conjExpr :: [BoolExpr] -> BoolExpr
conjExpr = CAUE Conj True

disjExpr :: [BoolExpr] -> BoolExpr
disjExpr = CAUE Disj False

existsExpr :: BoolExpr -> BoolExpr
existsExpr e = QuantE Exists e

-------------------------------------------------------------------------------

isLitE :: Expr t -> Bool
isLitE (LitE _) = True
isLitE _        = False

data Term = Term {-# UNPACK #-} !Int [IntExpr]

deconstructProduct :: IntExpr -> Term
deconstructProduct (CAUE Prod n xs) = Term n xs
deconstructProduct e                = Term (unit Prod) [e]

rebuildProduct :: Term -> IntExpr
rebuildProduct (Term 1 [e]) = e
rebuildProduct (Term n es)  = CAUE Prod n es

deconstructSum :: IntExpr -> Term
deconstructSum (CAUE Sum n xs) = Term n xs
deconstructSum e               = Term (unit Sum) [e]

rebuildSum :: Term -> IntExpr
rebuildSum (Term 1 [e]) = e
rebuildSum (Term n es)  = CAUE Sum n es

-- Get the 'equality' operator for type t.
cauEq :: CAUOp t -> t -> t -> Bool
cauEq Sum  = (==)
cauEq Prod = (==)
cauEq Conj = (==)
cauEq Disj = (==)

-- Get the zero for a CAU op (if one exists)
zero :: CAUOp t -> Maybe t
zero Sum  = Nothing
zero Prod = Just 0
zero Conj = Just False
zero Disj = Just True

-- Get the unit for a CAU op
unit :: CAUOp t -> t
unit Sum  = 0
unit Prod = 1
unit Conj = True
unit Disj = False

-- | True if the literal is the operator's zero.
isZeroOf :: t -> CAUOp t -> Bool
l `isZeroOf` op = case zero op
                  of Nothing -> False
                     Just z  -> cauEq op l z

-- | True if the literal is the operator's unit.
isUnitOf :: t -> CAUOp t -> Bool
l `isUnitOf` op = cauEq op (unit op) l

-- Evaluate an operator on a list of literals
evalCAUOp :: CAUOp t -> [t] -> t
evalCAUOp Sum  = sum
evalCAUOp Prod = product
evalCAUOp Conj = and
evalCAUOp Disj = or

-- Evaluate a predicate
evalPred :: PredOp -> Int -> Bool
evalPred IsZero = (0 ==)
evalPred IsGEZ  = (0 <=)

-------------------------------------------------------------------------------
-- Showing expressions

appPrec = 10
mulPrec = 7
addPrec = 6
cmpPrec = 4                     -- Less-than, equal
andPrec = 3
lamPrec = 0                     -- Body of lambda

-- An environment for showing expressions.
--
-- Quantified variables are shown as lambda-bound variables.  This structure
-- keeps track of lambda-bound variable names and how to show them.

data ShowsEnv =
    ShowsEnv
    { -- How to show the n_th bound variable, given a precedence context
      showNthVar :: ![Int -> ShowS]
      -- Number of bound variables we know about.
      --   numBound e == length (showNthVar e)
    , numBound   :: !Int
      -- Names for new bound variables
    , varNames   :: [ShowS]
    }

emptyShowsEnv =
    ShowsEnv
    { showNthVar = []
    , numBound = 0
    , varNames = map showChar $
                 ['x', 'y', 'z'] ++
                 ['a' .. 'w'] ++
                 [error "out of variable names"]
    }

-- Add a variable binding to the environment
bindVariable :: ShowsEnv -> (ShowS, ShowsEnv)
bindVariable env =
    case varNames env
    of nm : nms ->
           let env' = ShowsEnv
                      { showNthVar = showVar nm : showNthVar env
                      , numBound   = 1 + numBound env
                      , varNames   = nms
                      }
           in (nm, env')
    where
      -- Showing a variable produces "varE varName"
      showVar nm n = showParen (n >= appPrec) $ showString "varE " . nm

bindVariables :: Int -> ShowsEnv -> ([ShowS], ShowsEnv)
bindVariables 0 env = ([], env)
bindVariables n env = let (v, env')   = bindVariable env
                          (vs, env'') = bindVariables (n-1) env
                      in (v:vs, env'')

showsVarPrec :: ShowsEnv -> Int -> Var -> ShowS
showsVarPrec env prec (Bound i) =
    if i < numBound env
    then (showNthVar env !! i) prec
    else shift (numBound env)
    where
      -- The variable is not bound locally, so show its constructor.
      -- We have to subtract an offset to account for the local variable
      -- bindings, basically undoing the shift that 'withFreshVariable'
      -- applies.
      shift n = showParen (prec >= appPrec) $
                    showString "nthVarE " . shows (i-n)

-- Unique is not showable, but users shouldn't see quantified variables anyway
showsVarPrec _ _ (Quantified u) = showString "(Quantified _)"

showsInt :: Int -> ShowS
showsInt n | n >= 0    = showString "intE " . shows n
           | otherwise = showString "intE " . showParen True (shows n)


showsIntExprPrec :: ShowsEnv -> Int -> IntExpr -> ShowS
showsIntExprPrec env n expression =
    case expression
    of CAUE Sum lit es  -> showParen (n >= addPrec) $ showSum env lit es
       CAUE Prod lit es -> showParen (n >= mulPrec) $ showProd env lit es
       LitE l           -> showParen (n >= appPrec) $
                           showsInt l
       VarE v           -> showsVarPrec env n v

showsBoolExprPrec :: ShowsEnv -> Int -> BoolExpr -> ShowS
showsBoolExprPrec env n expression =
    case expression
    of CAUE Conj lit es
           | lit        -> let texts = map (showsBoolExprPrec env 0) es
                           in showParen (n >= fromIntegral andPrec) $
                              texts `showSepBy` showString " |&&| "
           | otherwise  -> showString "falseE"
       CAUE Disj lit es
           | lit        -> showString "trueE"
           | otherwise  -> let texts = map (showsBoolExprPrec env 0) es
                           in showParen (n >= appPrec) $
                              showString "disjE " . showsList texts
       PredE IsGEZ e    -> showGEZ env n e
       PredE IsZero e   -> showEQZ env n e
       NotE e           -> showString "notE " . showsBoolExprPrec env appPrec e
       LitE True        -> showString "trueE"
       LitE False       -> showString "falseE"
       QuantE q e       -> showParen (n >= appPrec) $
                           showQuantifier showsBoolExprPrec env q e

-- Show an inequality prettily.
-- First, eliminate minus-signs.
-- Then, choose between the ">", ">=", or "<" for displaying a term.
--
-- If one side of the inequality is a literal, it 
-- Use ">" if it gets rid of a term, otherwise use ">=".
-- If the left side of the inequality is an integer literal,
-- then move it to the right 
showGEZ :: ShowsEnv -> Int -> IntExpr -> ShowS
showGEZ env prec e =
    showParen (prec >= cmpPrec) $
    case e
    of LitE n          -> showGEZ' env n []
       CAUE Sum lit es -> showGEZ' env lit es
       _               -> showGEZ' env 0 [e]

showGEZ' env lit es =
    -- Partition into terms that will go on the left (positive) and right
    -- (negative) sides of the inequality.  Try to get rid of a '1' by
    -- using a greater-than sign.
    case partitionSumBySign lit es
    of (-1, neg, pos) -> balanceInequality False 0 neg pos
       (n, neg, pos)  -> balanceInequality True n neg pos
    where
      -- If the left side is empty, flip the direction of the inequality
      balanceInequality True n neg [] =
          showInequality le (negate n) [] neg

      balanceInequality False n neg [] =
          showInequality lt (negate n) [] neg

      balanceInequality True n neg pos =
          showInequality ge n neg pos
          
      balanceInequality False n neg pos =
          showInequality gt n neg pos

      -- Show the inequality.  Put the literal on whichever side makes it
      -- positive.
      showInequality symbol lit neg pos =
          let (pos', neg') =
                  if lit >= 0
                  then (CAUE Sum lit pos, CAUE Sum 0 neg)
                  else (CAUE Sum 0 pos, CAUE Sum (negate lit) neg)
          in showsIntExprPrec env cmpPrec pos' .
             symbol .
             showsIntExprPrec env cmpPrec neg'

      ge = showString " |>=| "
      gt = showString " |>| "
      le = showString " |<=| "
      lt = showString " |<| "


-- Show a comparison-to-zero prettily.
-- If it is a summation with only one term, then we show it as
--   "term |==| literal"
-- Otherwise we show it as an application of "isZeroE".
showEQZ :: ShowsEnv -> Int -> IntExpr -> ShowS
showEQZ env prec (CAUE Sum lit [e]) =
    showParen (prec >= cmpPrec) $
    (showsIntExprPrec env cmpPrec e .
     showString " |==| " .
     showsIntExprPrec env cmpPrec (LitE lit)
    )

showEQZ env prec e =
    showParen (prec >= appPrec) $
    showString "isZeroE " . showsIntExprPrec env appPrec e

-- Partition a sum term based on the sign it is displayed with.
-- Negative-signed terms are multiplied by -1 to make them positive.
partitionSumBySign n es =
    case partition hasNegativeMultiplier es
    of (neg, pos) -> let neg' = map negateMultiplier neg
                     in (n, neg', pos)
    where
      hasNegativeMultiplier :: IntExpr -> Bool
      hasNegativeMultiplier (CAUE Prod n es) = n < 0
      hasNegativeMultiplier (LitE n) = n < 0
      hasNegativeMultiplier _ = False

      negateMultiplier :: IntExpr -> IntExpr
      negateMultiplier (CAUE Prod n es) = CAUE Prod (negate n) es
      negateMultiplier (LitE n) = LitE (negate n)
      negateMultiplier _ = error "partitionSumBySign: unexpected term"

-- Show a sum term
showSum env lit es =
    -- The first element of the summation gets shown a little differently.
    -- There are a couple of cases, depending on what is the first element.
    if lit == 0
    then case es
         of e : es' -> showsIntExprPrec env addPrec e . showSumTail es'
            []      -> showsInt 0
    else showsInt lit . showSumTail es
    where
      -- Show the tail of a sum term.  Each expression is preceded by
      -- the |+| or |-| operator.
      showSumTail es = foldr (.) id $ map showSumTailElement es

      showSumTailElement e =
          case deconstructProduct e
          of Term 1 es             -> add . showProd env 1 es
             Term (-1) es          -> sub . showProd env 1 es
             Term n es | n >= 0    -> add . showProd env n es
                       | otherwise -> sub . showProd env (negate n) es

      add = showString " |+| "
      sub = showString " |-| "

-- Show a product term
showProd env lit es =
    let text = map (showsIntExprPrec env mulPrec) es
        textLit = if lit == 1
                  then id
                  else showsPrec mulPrec lit . showString " *| "
    in textLit . (text `showSepBy` showString " |*| ")

-- Show a list in [,,] syntax
showsList :: [ShowS] -> ShowS
showsList ss =
    showChar '[' . (ss `showSepBy` showString ", ") . showChar ']'

-- Show a list with a separator interspersed
showSepBy :: [ShowS] -> ShowS -> ShowS
xs `showSepBy` sep = foldr (.) id (intersperse sep xs)

-- Show a quantified expression, e.g. (forallE $ \x -> varE x |+| intE 1)
showQuantifier :: (ShowsEnv -> Int -> Expr t -> ShowS)
               -> ShowsEnv -> Quantifier -> Expr t -> ShowS
showQuantifier showExpr env q e =
    let quantifier = case q
                     of Forall -> showString "forallE $ \\"
                        Exists -> showString "existsE $ \\"

        -- Take a new variable name
        (varName, env') = bindVariable env

    in quantifier . varName . showString " -> " . showExpr env' lamPrec e

-- Show a term with a lambda-bound parameter
showLambdaBound :: (ShowsEnv -> Int -> ShowS)
                -> ShowsEnv -> Int -> ShowS
showLambdaBound showBody env prec =
  let -- Take a new variable name
      (varName, env') = bindVariable env

  in showParen (prec >= appPrec) $
     showChar '\\' . varName . showString " -> " .
     showBody env' lamPrec

-- Show a term with a lambda-bound list parameter
showLambdaList :: Int
               -> (ShowsEnv -> Int -> ShowS)
               -> ShowsEnv -> Int -> ShowS
showLambdaList n_params showBody env prec =
  let -- Take a new variable name
      (varNames, env') = bindVariables n_params env

  in showParen (prec >= appPrec) $
     showChar '\\' . showsList varNames . showString " -> " .
     showBody env' lamPrec

-------------------------------------------------------------------------------
-- Syntactic equality on expressions

-- | Decide whether two expressions are syntactically equal, modulo
-- commutativity, associativity, and alpha-renaming.
expEqual :: Eq t => Expr t -> Expr t -> Bool
expEqual expr1 expr2 =
    case (expr1, expr2)
    of (CAUE op1 l1 es1, CAUE op2 l2 es2) ->
          op1 == op2 && l1 == l2 && expListsEqual es1 es2

       (PredE op1 e1, PredE op2 e2) ->
          op1 == op2 && expEqual e1 e2

       (NotE e1, NotE e2) -> expEqual e1 e2

       (LitE l1, LitE l2) -> l1 == l2

       (VarE v1, VarE v2) -> v1 == v2

       (QuantE q1 e1, QuantE q2 e2) ->
          q1 == q2 && expEqual e1 e2

       (_, _) -> False          -- Different constructors

-- Decide whether two unordered expression lists are equal.
-- For each element of the first list, find
-- a matching element of the second list and repeat.
expListsEqual :: Eq t => [Expr t] -> [Expr t] -> Bool
expListsEqual (e:es1) es2 =
    case findEqualExpr e es2
    of Just (_, es2') -> expListsEqual es1 es2'
       Nothing        -> False

expListsEqual [] [] = True      -- All elements matched
expListsEqual [] _  = False     -- Some leftover elements in es2

-- Find an equal expression in the list.
findEqualExpr :: Eq t => Expr t -> [Expr t] -> Maybe (Expr t, [Expr t])
findEqualExpr searchE es = go es id
    where
      go (e:es) h | expEqual searchE e = Just (e, h es)
                  | otherwise          = go es (h . (e:))
      go []     _                      = Nothing

-------------------------------------------------------------------------------
-- Simplification rules

-- This is the main rule for simplifying an expression.
--
-- First, subexpressions are simplified (simplifyRec).
-- Then "basic" simplifications are performed.  These restructure the
-- current term, but no other terms.
-- Then complex simplifications are performed that restructure the current
-- term and subtems.

-- | Normalize an expression.
simplify :: Expr t -> Expr t
simplify e =
    complexSimplifications $ basicSimplifications $ simplifyRec e

simplifyRec :: Expr t -> Expr t
simplifyRec expr =
    case expr
    of CAUE op lit es -> CAUE op lit $ map simplify es
       PredE op e1 -> PredE op $ simplify e1
       NotE e -> NotE $ simplify e
       LitE _ -> expr
       VarE v -> expr
       QuantE q e -> QuantE q $ simplify e 

basicSimplifications :: Expr t -> Expr t
basicSimplifications = zus . peval . flatten

-- Some complex simplifications require steps of simplification to be re-run.
complexSimplifications :: Expr t -> Expr t
complexSimplifications e =
    case e
    of CAUE Sum _ _  -> basicSimplifications $ collect e
       CAUE Prod _ _ -> posToSop e
       _             -> e

-- Convert a product of sums to a sum of products.  If conversion happens,
-- simplification is re-run.

posToSop :: Expr Int -> Expr Int
posToSop expr@(CAUE Prod n es)
    | all isSingletonTerm terms =
        -- If no terms are sums, then the expression is unchanged
        expr

    | otherwise =
          let -- Make a list of lists.
              -- The expression corresponds to
              --   product (map sum terms')
              terms' = [LitE n] : map mkTermList terms

              -- 'sequence' converts terms' to sum of products from.
              expr'  = CAUE Sum 0 [CAUE Prod 1 t | t <- sequence terms']
          in simplify expr'
    where
      terms = map deconstructSum es
      mkTermList (Term n es) = LitE n : es

      -- True if we've deconstructed something that's not really a sum.
      -- Compare with eliminations in 'zus'.
      isSingletonTerm (Term 0 [_]) = True
      isSingletonTerm (Term _ [])  = True
      isSingletonTerm (Term _ _  ) = False

posToSop expr = expr            -- Terms other than products are not modified

-- Flatten nested CA expressions
flatten :: forall t. Expr t -> Expr t
flatten (CAUE op lit es) =
    case flat lit id es of (lit', es') -> CAUE op lit' es'
    where
      -- Wherever a nested CA expression with the same operator appears,
      -- include its terms in the list
      flat lit hd (e:es) =
          case e
          of CAUE op2 lit2 es2
                 | op == op2 -> let lit' = evalCAUOp op [lit, lit2]
                                in flat lit' hd (es2 ++ es)
             _ -> flat lit (hd . (e:)) es
      flat lit hd [] = (lit, hd [])

flatten e = e

-- Partially evaluate an expression
peval :: Expr t -> Expr t
peval exp@(CAUE op l es) =
    case partition isLitE es
    of ([], _)         -> exp
       (lits, notLits) -> let literals = l : map fromLitE lits
                          in CAUE op (evalCAUOp op literals) notLits
    where
      fromLitE (LitE l) = l
      fromLitE _        = error "peval: unexpected expression"

peval exp@(PredE op e) =
    case e
    of LitE l -> LitE $ evalPred op l
       _      -> exp

peval exp@(NotE e) =
    case e
    of LitE l -> LitE $ not l
       _      -> exp

peval e = e

-- Zero, unit, singleton rules.  May eliminate an
-- expression here.
zus :: Expr t -> Expr t
zus exp@(CAUE op l es) =
    case es
    of []                    -> LitE l
       _   | l `isZeroOf` op -> LitE l -- zero * x = zero
       [e] | l `isUnitOf` op -> e      -- unit * x = x
       _                     -> exp    -- no simplification

zus e = e

-- Given a sum of products, collect terms that differ only in their
-- constant multiplier.
--
-- For example:
--
--  collect (2xy + 3x - 3xy)
--  becomes (-1)xy + 3x

collect :: Expr Int -> Expr Int
collect (CAUE Sum literal es) =
    let es' = map simplify $
              map rebuildProduct $
              collectTerms $
              map deconstructProduct es
    in CAUE Sum literal es'

    where
      collectTerms :: [Term] -> [Term]
      collectTerms (t:ts) =
          case collectTerm t ts of (t', ts') -> t':collectTerms ts'
      collectTerms [] = []

      -- Collect together all terms from the list that differ from
      -- the first term only in their multiplier.  The collected terms'
      -- multipliers are summed.  The result is the collected term
      -- and the unused terms from the list.
      collectTerm :: Term -> [Term] -> (Term, [Term])
      collectTerm (Term factor t) terms =
          let (equalTerms, terms') = partition (sameTerms t) terms
              factor'              = factor + sum [n | Term n _ <- equalTerms]
          in (Term factor' t, terms')

      -- True if the terms are the same modulo a constant factor.
      sameTerms t (Term _ t') = expListsEqual t t'

collect e = e                   -- Terms other than sums do not change

-------------------------------------------------------------------------------
-- Converting expressions to formulas

-- | Look up a variable in a list.  The variable's position is its
-- de Bruijn index.

lookupVar :: Int -> [VarHandle] -> VarHandle
lookupVar n (v : vars) | n > 0  = lookupVar (n - 1) vars
                       | n == 0 = v
                       | otherwise = error "lookupVar: negative index"

lookupVar _ [] = error "lookupVar: variable index out of range"

-- | Convert a boolean expression to a formula.
--
-- The expression must be a Presburger formula.  In particular, if an
-- expression involves the product of two non-constant terms, it cannot be
-- converted to a formula.  The library
-- internally simplifies expressions to sum-of-products form, so complex
-- expressions are valid as long as each simplified product has at most
-- one variable.

expToFormula :: [VarHandle]     -- ^ Free variables
             -> BoolExp         -- ^ Expression to convert
             -> Formula
expToFormula freeVars e = exprToFormula freeVars (getSimplifiedExpr e)

exprToFormula :: [VarHandle]     -- ^ Free variables
              -> BoolExpr        -- ^ Expression to convert
              -> Formula
exprToFormula freeVars expr =
    case expr
    of CAUE op lit es
           | lit `isUnitOf` op ->
               case op
               of Conj -> conjunction $ map (exprToFormula freeVars) es
                  Disj -> disjunction $ map (exprToFormula freeVars) es
                  _    -> expToFormulaError "unhandled operator"
           | otherwise ->
               -- This boolean literal overrides all other terms
               if lit then true else false

       PredE op e ->
           case sumToConstraint freeVars e
           of (terms, constant) ->
                  case op
                  of IsZero -> equality terms constant
                     IsGEZ  -> inequality terms constant

       NotE e -> negation $ exprToFormula freeVars e

       LitE True  -> true
       LitE False -> false

       QuantE q e -> let body v = exprToFormula (v:freeVars) e
                     in case q
                        of Forall -> qForall body
                           Exists -> qExists body

-- | Convert an integer term to a coefficients for an equality or
-- inequality constraint.
sumToConstraint :: [VarHandle]  -- ^ free variables
                -> IntExpr      -- ^ expression to convert
                -> ([Coefficient], Int)
sumToConstraint freeVars expr =
    case deconstructSum expr
    of Term constant terms -> (map deconstructTerm terms, constant)
    where
      deconstructTerm :: IntExpr -> Coefficient
      deconstructTerm expr =
          case deconstructProduct expr
          of Term n [VarE (Bound i)] -> Coefficient (lookupVar i freeVars) n
             _ -> expToFormulaError "expression is non-affine"

expToFormulaError :: String -> a
expToFormulaError s = error $ "expToFormula: " ++ s

-- | Substitute a single variable in an expression.
rename  :: Var               -- ^ variable to replace
        -> Var               -- ^ its replacement
        -> Exp t             -- ^ expression to rename
        -> Exp t             -- ^ renamed expression
rename v1 v2 e = wrapExpr $ renameExpr v1 v2 $ getExpr e

renameExpr :: Var               -- ^ variable to replace
           -> Var               -- ^ its replacement
           -> Expr t            -- ^ expression to rename
           -> Expr t            -- ^ renamed expression
renameExpr !v1 v2 expr = rn expr
    where
      rn :: forall t. Expr t -> Expr t
      rn (CAUE op lit es) = CAUE op lit $ map rn es
      rn (PredE op e)     = PredE op $ rn e
      rn (NotE e)         = NotE $ rn e
      rn expr@(LitE _)    = expr
      rn expr@(VarE v)    | v == v1   = VarE v2
                          | otherwise = expr
      rn (QuantE q e)     = QuantE q $ renameExpr (bump v1) (bump v2) e

      -- Increment a de Bruijn index
      bump (Bound n)        = Bound (n+1)
      bump v@(Quantified _) = v

-- | Adjust bound variable bindings by adding an offset to all bound variable
-- indices beyond a given level.
adjustBindings :: Int           -- ^ first variable to change
               -> Int           -- ^ Amount to shift by
               -> Exp t         -- ^ Input expression
               -> Exp t         -- ^ Adjusted expression
adjustBindings firstBound shift e =
    wrapExpr $ adjustBindingsExpr firstBound shift $ getExpr e

adjustBindingsExpr :: Int       -- ^ first variable to change
                   -> Int       -- ^ Amount to shift by
                   -> Expr t    -- ^ Input expression
                   -> Expr t    -- ^ Adjusted expression
adjustBindingsExpr !firstBound !shift e = adj e
    where
      adj :: Expr t -> Expr t
      adj (CAUE op lit es) = CAUE op lit (map adj es)
      adj (PredE op e)     = PredE op (adj e)
      adj (NotE e)         = NotE (adj e)
      adj expr@(LitE _)    = expr
      adj expr@(VarE v)    = case v
                             of Bound n
                                    | n >= firstBound ->
                                        VarE $ Bound (n + shift)
                                    | otherwise ->
                                        expr
                                Quantified _ -> expr
      adj (QuantE q e)     = QuantE q $
                             adjustBindingsExpr (firstBound + 1) shift e

-- | True if the expression has no more than the specified number
-- of free variables.
variablesWithinRange :: Int -> Exp t -> Bool
variablesWithinRange n e = check n $ getExpr e
    where
      check :: Int -> Expr t -> Bool
      check n e = check' e
          where
            check' :: Expr t -> Bool
            check' (CAUE _ _ es)         = all check' es
            check' (PredE _ e)           = check' e
            check' (NotE e)              = check' e
            check' (LitE _)              = True
            check' (VarE (Bound i))      = i < n
            check' (VarE (Quantified _)) = quantifiedVar
            check' (QuantE _ e)          = check (n+1) e

      quantifiedVar :: Bool
      quantifiedVar = error "variablesWithinRange: unexpected variable"
