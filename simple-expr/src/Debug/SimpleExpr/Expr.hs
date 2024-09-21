{-# LANGUAGE CPP #-}
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
module  Debug.SimpleExpr.Expr
  ( -- * Expression manipulation
    number,
    variable,
    unaryFunc,
    binaryFunc,
    simplifyExpr,
    simplifyStep,
    simplify,

    -- * Base types
--    SimpleExprF (NumberF, VariableF, BinaryFuncF, SymbolicFuncF),
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

import Control.Monad.Fix (fix)
import Data.Fix (Fix (Fix, unFix))
import Data.Functor.Classes (Eq1, liftEq)
import Data.List (intercalate, (++))
import NumHask (Additive, Divisive, ExpField, Multiplicative, Subtractive, TrigField, one, zero, 
  fromIntegral)
import qualified NumHask as NH
import Prelude
  ( Bool (False),
    Eq,
    Functor,
    Num,
    Show,
    fmap,
    seq,
    show,
    ($),
    (&&),
    (.),
    (<>),
    (==),
    (>=),
    Maybe(Just, Nothing),
    pure,
--    undefined,
    String,
    last,
    init,
    head,
    length,
    tail,
    const,
    id,
  )
--import BasicPrelude (Show)
import Data.Maybe (isJust)
import qualified Prelude as P
import GHC.Natural (Natural)
--import Data.Text (Text, length, head, last, tail, init, unpack)
import Control.Monad (guard)
import Control.ExtendableMap (ExtandableMap, extendMap)

-- | Expression F-algebra functional.
data SimpleExprF a
  = NumberF Natural
  | VariableF String
--  | BinaryFuncF Text a a
  | SymbolicFuncF String [a]
  deriving (Functor, Eq)

instance Eq1 SimpleExprF where
  liftEq :: (a -> b -> Bool) -> SimpleExprF a -> SimpleExprF b -> Bool
  liftEq eq e1 e2 = case (e1, e2) of
    (NumberF n1, NumberF n2) -> n1 == n2
    (VariableF v1, VariableF v2) -> v1 == v2
--    (BinaryFuncF name1 x1 y1, BinaryFuncF name2 x2 y2) -> (name1 == name2) && eq x1 x2 && eq y1 y2
    (SymbolicFuncF name1 args1, SymbolicFuncF name2 args2) -> (name1 == name2) && liftEq eq args1 args2
    _ -> False

instance NH.FromIntegral Natural n => 
  NH.FromIntegral (SimpleExprF a) n where
    fromIntegral = NumberF . fromIntegral

-- | Simple expression type, see
-- [tutorial](Debug.SimpleExpr.Tutorial.hs)
type SimpleExpr = Fix SimpleExprF
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
--  BinaryFuncF _ leftArg rightArg -> [leftArg, rightArg]
  SymbolicFuncF _ args -> args

instance NH.FromIntegral Natural n => 
  NH.FromIntegral SimpleExpr n  where
    fromIntegral = number . fromIntegral

--instance NH.FromIntegral SimpleExpr Integer where
--  fromIntegral = Fix . NumberF

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
--  map :: (inner -> inner) -> outer -> outer

instance ListOf inner () where
  content = P.const []
--  map _ = const ()

instance ListOf inner inner where
  content e = [e]
--  map = id

instance (ListOf inner outer1, ListOf inner outer2) => 
  ListOf inner (outer1, outer2) 
  where
    content (x1, x2) = content x1 ++ content x2
--    map f (x0, x1) = (f x0, f x1)  

instance (ListOf inner outer1, ListOf inner outer2, ListOf inner outer3) => 
  ListOf inner (outer1, outer2, outer3) 
  where
    content (x1, x2, x3) = content x1 ++ content x2 ++ content x3
--    map f (x0, x1, x2) = (f x0, f x1, f x2)  

instance
  (ListOf inner outer1, ListOf inner outer2, ListOf inner outer3, ListOf inner outer4) =>
    ListOf inner (outer1, outer2, outer3, outer4)
    where
    content (x1, x2, x3, x4) = content x1 ++ content x2 ++ content x3 ++ content x4

instance
  (ListOf inner outer1, ListOf inner outer2, ListOf inner outer3, ListOf inner outer4, ListOf inner outer5) =>
  ListOf inner (outer1, outer2, outer3, outer4, outer5)
  where
  content (x1, x2, x3, x4, x5) = content x1 ++ content x2 ++ content x3 ++ content x4 ++ content x5

instance (ListOf inner outer) => ListOf inner [outer] where
  content = (content P.=<<)

-- | Expression typeclass.
-- It includes `SimpleExpr` as well as list and tuples of `SimpleExpr` etc.
type Expr = ListOf SimpleExpr

instance {-# OVERLAPPING #-} Show SimpleExpr where
  show (Fix e) = case e of
    NumberF n -> show n
    VariableF name -> name
    -- BinaryFuncF name leftArg rightArg -> showWithBrackets leftArg <> name <> showWithBrackets rightArg
    sf@(SymbolicFuncF name args) -> case matchBinnaryFuncPattern (Fix sf) of
      Just (name', leftArg, rightArg) -> showWithBrackets leftArg <> name' <> showWithBrackets rightArg
      Nothing -> name <> "(" <> intercalate "," (fmap show args) <> ")"

needBrackets :: SimpleExpr -> Bool
needBrackets = isJust . matchBinnaryFuncPattern

-- | Shows expression adding brackets if it is needed for a context.
showWithBrackets :: SimpleExpr -> String
showWithBrackets e = if needBrackets e
  then "(" <> show e <> ")"
  else show e

--showWithBrackets e = case e of
--  n@(Fix NumberF {}) -> unpack $ show n
--  c@(Fix VariableF {}) -> show c
--  -- bf@(Fix BinaryFuncF {}) -> "(" <> show bf <> ")"
--  sf@(Fix SymbolicFuncF {}) -> case matchBinnaryFuncPattern sf of
--    Just name -> "(" <> show sf <> ")"
--    Nothing -> show sf

matchBinnaryFuncPattern :: SimpleExpr -> Maybe (String, SimpleExpr, SimpleExpr)
matchBinnaryFuncPattern (Fix (SymbolicFuncF name [x, y])) = do
  guard $ length name >= 3
  guard $ head name == '('
  guard $ last name == ')'
  pure (init (tail name), x, y)
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
--binaryFunc name x y = Fix (BinaryFuncF name x y)
binaryFunc name x y = Fix $ SymbolicFuncF ("(" <> name <> ")") [x, y]

instance Additive SimpleExpr where
  zero = number 0
  (+) = binaryFunc "+"

instance Subtractive SimpleExpr where
  negate = unaryFunc "-"
  (-) = binaryFunc "-"

instance Multiplicative SimpleExpr where
  one = number 1
  (*) = binaryFunc "*"

#if MIN_VERSION_numhask(0,11,0)
#else
instance NH.Distributive SimpleExpr
#endif

instance Divisive SimpleExpr where
  (/) = binaryFunc "/"

#if MIN_VERSION_numhask(0,11,0)
#else
instance NH.Field SimpleExpr
#endif

instance ExpField SimpleExpr where
  exp = unaryFunc "exp"
  log = unaryFunc "log"
  (**) = binaryFunc "^"
  sqrt = unaryFunc "sqrt"

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
iterateUntilEqual :: Eq x => (x -> x) -> x -> x
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
    _ ->  Fix $ SymbolicFuncF name [f arg]
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
      (SymbolicFuncF "-" [leftArg'], SymbolicFuncF "-" [rightArg']) 
        -> Fix $ SymbolicFuncF "(*)" [f leftArg', f rightArg']
      (SymbolicFuncF "-" [leftArg'], rightArg') 
        -> Fix $ SymbolicFuncF "-" [Fix $ SymbolicFuncF "(*)" [leftArg', Fix rightArg']]
      (leftArg', SymbolicFuncF "-" [rightArg']) 
        -> Fix $ SymbolicFuncF "-" [Fix $ SymbolicFuncF "(*)" [Fix leftArg', rightArg']]
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

-- | Simplify expression using some primitive rules like '0 * x -> 0' specified in 'simplifyStep' implementation.
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
simplifyExpr = fix $ iterateUntilEqual . simplifyStep -- simplify = iterateUntilEqual (simplifyStep simplify)

simplify :: ExtandableMap SimpleExpr SimpleExpr a b => a -> b
simplify = extendMap simplifyExpr