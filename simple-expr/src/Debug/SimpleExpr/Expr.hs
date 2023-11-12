{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wcpp-undef #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  Debug.SimpleExpr.Expr
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Simple expressions base types and manipulations.
module Debug.SimpleExpr.Expr
  ( -- * Expression manipulation
    number,
    variable,
    unaryFunc,
    binaryFunc,
    simplifyExpr,
    simplifyStep,
    simplify,

    -- * Base types
    SimpleExprF (NumberF, VariableF, SymbolicFuncF),
    SimpleExpr,
    SE,
    Expr,

    -- * Auxiliary functions
    ListOf,
    content,
    dependencies,
    showWithBrackets,
  )
where

import Control.ExtendableMap (ExtandableMap, extendMap)
import Control.Monad (guard)
import Control.Monad.Fix (fix)
import Data.Fix (Fix (Fix, unFix))
import Data.Functor.Classes (Eq1, liftEq)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Hashable.Lifted (Hashable1(liftHashWithSalt))
import Data.Hashable.Generic (genericLiftHashWithSalt)
import Data.List (intercalate, null, uncons, unsnoc, (++))
import Data.Maybe (isJust)
import GHC.Base
  ( Applicative (pure),
    Bool(False),
    Eq ((==)),
    Functor (fmap),
    Maybe (Just, Nothing),
    String,
    not,
    seq,
    ($),
    (&&),
    (.),
    (<>),
    (>=)
  )
import GHC.Generics (Generic1)
import GHC.Natural (Natural)
import GHC.Num (Num)
import GHC.Show (Show (show))
import NumHask
  ( Additive,
    Divisive,
    ExpField,
    FromInteger,
    Multiplicative,
    Subtractive,
    TrigField,
    fromIntegral,
    one,
    zero,
  )
import qualified NumHask as NH
import qualified Prelude as P

-- | Expression F-algebra functional.
data SimpleExprF a
  = NumberF Natural
  | VariableF String
  | SymbolicFuncF String [a]
  deriving (Functor, Eq, Generic1)

instance Hashable1 SimpleExprF where
  liftHashWithSalt = genericLiftHashWithSalt

instance Hashable a => Hashable (SimpleExprF a) where
  hashWithSalt salt (NumberF n) = hashWithSalt salt n
  hashWithSalt salt (VariableF s) = hashWithSalt salt s
  hashWithSalt salt (SymbolicFuncF name xs) =
    salt `hashWithSalt` name `hashWithSalt` xs

-- | Equality comparison for `SimpleExprF` lifted over its parameter.
instance Eq1 SimpleExprF where
  liftEq :: (a -> b -> Bool) -> SimpleExprF a -> SimpleExprF b -> Bool
  liftEq eq e1 e2 = case (e1, e2) of
    (NumberF n1, NumberF n2) -> n1 == n2
    (VariableF v1, VariableF v2) -> v1 == v2
    (SymbolicFuncF name1 args1, SymbolicFuncF name2 args2) -> (name1 == name2) && liftEq eq args1 args2
    _ -> False

-- | `SimpleExprF` instance of `NumHask.FromIntegral` typeclass.
instance
  (NH.FromIntegral Natural n) =>
  NH.FromIntegral (SimpleExprF a) n
  where
  fromIntegral = NumberF . fromIntegral

-- | Simple expression type, see
-- [tutorial](Debug.SimpleExpr.Tutorial.hs)
type SimpleExpr = Fix SimpleExprF

-- | Short type alias for `SimpleExpr`.
type SE = SimpleExpr

-- | Initializes a single integer number expression.
--
-- ==== __Examples of usage__
--
-- >>> a = number 42
-- >>> a
-- 42
-- >>> :t a
-- a :: SimpleExpr
number :: Natural -> SimpleExpr
number n = Fix (NumberF n)

-- | Initializes a single symbolic variable expression.
--
-- ==== __Examples of usage__
--
-- >>> x = variable "x"
-- >>> x
-- x
-- >>> :t x
-- x :: SimpleExpr
variable :: String -> SimpleExpr
variable name = Fix (VariableF name)

-- | Returns the list of head dependencies of an expression.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (($), id)
-- >>> import NumHask ((+), (*))
--
-- >>> dependencies (variable "x" + (variable "y" * variable "z"))
-- [x,y*z]
dependencies :: SimpleExpr -> [SimpleExpr]
dependencies (Fix e) = case e of
  NumberF _ -> []
  VariableF _ -> []
  SymbolicFuncF _ args -> args

-- | `SimpleExpr` instance of `NumHask.FromIntegral` typeclass.
instance
  (NH.FromIntegral Natural n) =>
  NH.FromIntegral SimpleExpr n
  where
  fromIntegral = number . fromIntegral

-- | Entity that is representable as a list of in general other entities.
-- In particular, @X@ is a list of single @[X]@, see the example below.
--
-- ==== __Examples of usage__
--
-- >>> data Atom = Atom String deriving Show
-- >>> type Particle = ListOf Atom
--
-- >>> content (Atom "He") :: [Atom]
-- [Atom "He"]
--
-- >>> content (Atom "H", Atom "H") :: [Atom]
-- [Atom "H",Atom "H"]
--
-- >>> content [Atom "H", Atom "O", Atom "H"] :: [Atom]
-- [Atom "H",Atom "O",Atom "H"]
class ListOf inner outer where
  -- | Returns a list of entities the argument consists of.
  content :: outer -> [inner]

-- | Base case instance of `ListOf`.
instance ListOf inner () where
  content = P.const []

-- | Identity instance of `ListOf`.
instance ListOf inner inner where
  content e = [e]

-- | Tuple instance of `ListOf`.
instance
  (ListOf inner outer1, ListOf inner outer2) =>
  ListOf inner (outer1, outer2)
  where
  content (x1, x2) = content x1 ++ content x2

-- | Triple instance of `ListOf`.
instance
  (ListOf inner outer1, ListOf inner outer2, ListOf inner outer3) =>
  ListOf inner (outer1, outer2, outer3)
  where
  content (x1, x2, x3) = content x1 ++ content x2 ++ content x3

-- | 4-tuple instance of `ListOf`.
instance
  (ListOf inner outer1, ListOf inner outer2, ListOf inner outer3, ListOf inner outer4) =>
  ListOf inner (outer1, outer2, outer3, outer4)
  where
  content (x1, x2, x3, x4) = content x1 ++ content x2 ++ content x3 ++ content x4

-- | 5-tuple instance of `ListOf`.
instance
  (ListOf inner outer1, ListOf inner outer2, ListOf inner outer3, ListOf inner outer4, ListOf inner outer5) =>
  ListOf inner (outer1, outer2, outer3, outer4, outer5)
  where
  content (x1, x2, x3, x4, x5) = content x1 ++ content x2 ++ content x3 ++ content x4 ++ content x5

-- | List `[]` instance of `ListOf`.
instance (ListOf inner outer) => ListOf inner [outer] where
  content = (content P.=<<)

-- | Expression typeclass.
-- It includes `SimpleExpr` as well as list and tuples of `SimpleExpr` etc.
type Expr = ListOf SimpleExpr

-- | `SimpleExpr` instance of `Show` typeclass.
instance {-# OVERLAPPING #-} Show SimpleExpr where
  show (Fix e) = case e of
    NumberF n -> show n
    VariableF name -> name
    sf@(SymbolicFuncF name args) -> case matchBinnaryFuncPattern (Fix sf) of
      Just (name', leftArg, rightArg) -> showWithBrackets leftArg <> name' <> showWithBrackets rightArg
      Nothing -> name <> "(" <> intercalate "," (fmap show args) <> ")"

-- | Checks whether expression needs brackets in a context like binary function argument.
needBrackets :: SimpleExpr -> Bool
needBrackets = isJust . matchBinnaryFuncPattern

-- | Shows expression adding brackets if it is needed for a context.
showWithBrackets :: SimpleExpr -> String
showWithBrackets e =
  if needBrackets e
    then "(" <> show e <> ")"
    else show e

-- | Matches binary function pattern.
matchBinnaryFuncPattern :: SimpleExpr -> Maybe (String, SimpleExpr, SimpleExpr)
matchBinnaryFuncPattern (Fix (SymbolicFuncF name [x, y])) = do
  (first, rest1) <- uncons name
  (body, lastCh) <- unsnoc rest1
  guard $ first == '('
  guard $ lastCh == ')'
  guard $ not (null body)
  pure (body, x, y)
matchBinnaryFuncPattern _ = Nothing

-- | Inituialize unarry function
--
-- ==== __Examples of usage__
--
-- >>> x = variable "x"
-- >>> f = unaryFunc "f"
-- >>> f x
-- f(x)
-- >>> :t x
-- x :: SimpleExpr
-- >>> :t f
-- f :: SimpleExpr -> SimpleExpr
unaryFunc :: String -> SimpleExpr -> SimpleExpr
unaryFunc name x = Fix (SymbolicFuncF name [x])

-- | Inituialize unarry function
--
-- ==== __Examples of usage__
--
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> (-*-) = binaryFunc "-*-"
-- >>> x -*- y
-- x-*-y
-- >>> :t x
-- x :: SimpleExpr
-- >>> :t (-*-)
-- (-*-) :: SimpleExpr -> SimpleExpr -> SimpleExpr
-- >>> :t x-*-y
-- x-*-y :: SimpleExpr
binaryFunc :: String -> SimpleExpr -> SimpleExpr -> SimpleExpr
binaryFunc name x y = Fix $ SymbolicFuncF ("(" <> name <> ")") [x, y]

-- | `SimpleExpr` instance of `NumHask.FromInteger` typeclass.
instance FromInteger SimpleExpr where
  fromInteger n =
    if n >= 0
      then number $ fromIntegral n
      else NH.negate $ number $ fromIntegral $ P.abs n

-- | `SimpleExpr` instance of `NumHask.Additive` typeclass.
instance Additive SimpleExpr where
  zero = number 0
  (+) = binaryFunc "+"

-- | `SimpleExpr` instance of `NumHask.Subtractive` typeclass.
instance Subtractive SimpleExpr where
  negate = unaryFunc "-"
  (-) = binaryFunc "-"

-- | `SimpleExpr` instance of `NumHask.Multiplicative` typeclass.
instance Multiplicative SimpleExpr where
  one = number 1
  (*) = binaryFunc "*"

#if MIN_VERSION_numhask(0,11,0)
#else
instance NH.Distributive SimpleExpr
#endif

-- | `SimpleExpr` instance of `NumHask.Divisive` typeclass.
instance Divisive SimpleExpr where
  (/) = binaryFunc "/"

#if MIN_VERSION_numhask(0,11,0)
#else
instance NH.Field SimpleExpr
#endif

-- | `SimpleExpr` instance of `NumHask.ExpField` typeclass.
instance ExpField SimpleExpr where
  exp = unaryFunc "exp"
  log = unaryFunc "log"
  (**) = binaryFunc "^"
  sqrt = unaryFunc "sqrt"

-- | `SimpleExpr` instance of `NumHask.TrigField` typeclass.
instance TrigField SimpleExpr where
  pi = variable "pi"
  sin = unaryFunc "sin"
  cos = unaryFunc "cos"
  tan = unaryFunc "tg"
  asin = unaryFunc "arcsin"
  acos = unaryFunc "arccos"
  atan = unaryFunc "arctan"
  sinh = unaryFunc "sh"
  cosh = unaryFunc "ch"
  tanh = unaryFunc "th"
  atan2 a b = Fix $ SymbolicFuncF "atan2" [a, b]
  asinh = unaryFunc "arcsh"
  acosh = unaryFunc "arcch"
  atanh = unaryFunc "arcth"

-- | Numeric typeclass instance for `SimpleExpr`.
--
-- This instance enables standard numeric operations on symbolic expressions,
-- allowing for more natural mathematical notation in symbolic computations.
--
-- ==== __Examples of usage__
--
-- >>> import GHC.Num ((+))
--
-- The primary benefit of this instance is enabling direct use of numeric
-- literals in symbolic expressions without explicit conversion. This allows
-- you to write natural mathematical expressions:
--
-- >>> x = variable "x"
-- >>> x + 1
-- x+1
--
-- This concise notation is equivalent to the more verbose explicit form:
--
-- >>> x + (number 1)
-- x+1
instance Num SimpleExpr where
  (+) = (NH.+)
  (-) = (NH.-)
  (*) = (NH.*)
  negate = NH.negate
  abs = unaryFunc "abs"
  signum = unaryFunc "sign"
  fromInteger = fromIntegral

-- | Applies a function recursivelly until it has no effect.
-- Strict.
-- Unsafe due to possible inifinite recursion.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Integer, div)
-- >>> iterateUntilEqual (`div` 2) (1000 :: Integer)
-- 0
iterateUntilEqual :: (Eq x) => (x -> x) -> x -> x
iterateUntilEqual f x =
  let fx = f x
   in if fx == x
        then x
        else seq fx (iterateUntilEqual f fx)

-- | Minimalistic simplification step.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (($), id)
-- >>> import NumHask ((+), (*), (**))
--
-- >>> simplifyStep id (0 + (0 + (0 + 10)))
-- 0+(0+10)
--
-- >>> simplifyStep id (1 * (0 + (10 ** 1)))
-- 0+(10^1)
simplifyStep :: (SimpleExpr -> SimpleExpr) -> SimpleExpr -> SimpleExpr
simplifyStep f e = case e of
  n@(Fix (NumberF _)) -> n
  c@(Fix (VariableF _)) -> c
  Fix (SymbolicFuncF name [arg]) -> case name of
    "-" -> case unFix (f arg) of
      NumberF 0 -> Fix $ NumberF 0
      SymbolicFuncF "-" [arg'] -> f arg'
      SymbolicFuncF "(-)" [leftArg, rightArg] -> Fix $ SymbolicFuncF "(-)" [f rightArg, f leftArg]
      _ -> Fix $ SymbolicFuncF "-" [f arg]
    _ -> Fix $ SymbolicFuncF name [f arg]
  Fix (SymbolicFuncF name [leftArg, rightArg]) -> case name of
    "(+)" -> case (unFix leftArg, unFix rightArg) of
      (NumberF 0, _) -> f rightArg
      (_, NumberF 0) -> f leftArg
      (NumberF n, NumberF m) -> Fix (NumberF (n P.+ m))
      _ -> Fix (SymbolicFuncF "(+)" [f leftArg, f rightArg])
    "(-)" -> case (unFix leftArg, unFix rightArg) of
      (NumberF 0, _) -> NH.negate f rightArg
      (_, NumberF 0) -> f leftArg
      (NumberF n, NumberF m) -> Fix (NumberF (n P.- m))
      _ ->
        if fX == fY
          then zero
          else Fix (SymbolicFuncF "(-)" [fX, fY])
        where
          fX = f leftArg
          fY = f rightArg
    "(*)" -> case (unFix leftArg, unFix rightArg) of
      (NumberF 0, _) -> zero
      (_, NumberF 0) -> zero
      (NumberF 1, _) -> f rightArg
      (_, NumberF 1) -> f leftArg
      (NumberF n, NumberF m) -> Fix (NumberF (n P.* m))
      (SymbolicFuncF "-" [leftArg'], SymbolicFuncF "-" [rightArg']) ->
        Fix $ SymbolicFuncF "(*)" [f leftArg', f rightArg']
      (SymbolicFuncF "-" [leftArg'], rightArg') ->
        Fix $ SymbolicFuncF "-" [Fix $ SymbolicFuncF "(*)" [leftArg', Fix rightArg']]
      (leftArg', SymbolicFuncF "-" [rightArg']) ->
        Fix $ SymbolicFuncF "-" [Fix $ SymbolicFuncF "(*)" [Fix leftArg', rightArg']]
      _ -> Fix (SymbolicFuncF "(*)" [f leftArg, f rightArg])
    "(^)" -> case (unFix leftArg, unFix rightArg) of
      (NumberF n, NumberF m) -> Fix (NumberF (n P.^ m))
      (NumberF 0, _) -> zero
      (_, NumberF 0) -> one
      (NumberF 1, _) -> one
      (_, NumberF 1) -> f leftArg
      _ -> Fix (SymbolicFuncF "(^)" [f leftArg, f rightArg])
    _ -> Fix (SymbolicFuncF name [f leftArg, f rightArg])
  Fix (SymbolicFuncF name args) -> Fix (SymbolicFuncF name (fmap f args))

-- | Simplify expression using some primitive rules
-- like '0 * x -> 0' specified in 'simplifyStep' implementation.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (($))
-- >>> import Debug.SimpleExpr (variable, simplify)
-- >>> import NumHask ((+), (-), (*))
--
-- >>> x = variable "x"
-- >>> simplifyExpr $ (x + 0) * 1 - x * (3 - 2)
-- 0
simplifyExpr :: SimpleExpr -> SimpleExpr
simplifyExpr = fix $ iterateUntilEqual . simplifyStep

-- | Simplify expression using some primitive rules
-- like '0 * x -> 0' specified in `simplifyStep` implementation.
simplify :: (ExtandableMap SimpleExpr SimpleExpr a a) => a -> a
simplify = extendMap simplifyExpr
