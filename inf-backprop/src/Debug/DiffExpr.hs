{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | Module    :  Debug.SimpleExpr
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Tools for symbolic differentiation expressions.
module Debug.DiffExpr where

import Data.Fix (Fix (Fix))
import Debug.SimpleExpr.Expr
  ( SimpleExpr,
    SimpleExprF (SymbolicFuncF),
    unaryFunc,
  )
import Debug.SimpleExpr.Utils.Traced (Traced (MkTraced))
import Debug.Trace (trace)
import NumHask
  ( Additive,
    Distributive,
    Multiplicative,
    (*),
    (+),
  )
import Numeric.InfBackprop (RevDiff (MkRevDiff))
import Prelude (Show, String, show, ($), (<>))

-- | Create a binary function expression.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable)
--
-- >>> twoArgFunc "f" (variable "x") (variable "y")
-- f(x,y)
twoArgFunc :: String -> SimpleExpr -> SimpleExpr -> SimpleExpr
twoArgFunc name x y = Fix (SymbolicFuncF name [x, y])

-- | This typecalss is for creating symbolic unary function expressions.
--
-- It is used in conjunction with automatic differentiation to represent
-- functions symbolically.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable)
-- >>> import Numeric.InfBackprop (simpleDerivative)
--
-- >>> :{
--  f :: SymbolicFunc a => a -> a
--  f = unarySymbolicFunc "f"
-- :}
--
-- >>> f (variable "x")
-- f(x)
--
-- >>> simpleDerivative f (variable "x")
-- f'(x)*1
class SymbolicFunc a where
  unarySymbolicFunc :: String -> a -> a

-- | `SimpleExpr` instance of `SymbolicFunc` typeclass.
instance SymbolicFunc SimpleExpr where
  unarySymbolicFunc = unaryFunc

-- | `RevDiff` instance of `SymbolicFunc` typeclass.
instance
  (SymbolicFunc a, Multiplicative a) =>
  SymbolicFunc (RevDiff t a a)
  where
  unarySymbolicFunc :: String -> RevDiff t a a -> RevDiff t a a
  unarySymbolicFunc funcName (MkRevDiff x bp) =
    MkRevDiff
      (unarySymbolicFunc funcName x)
      (\cy -> bp $ f' * cy)
    where
      f' = unarySymbolicFunc (funcName <> "'") x

-- | This typecalss is for creating symbolic binary function expressions.
--
-- It is used in conjunction with automatic differentiation to represent
-- functions symbolically. See `SymbolicFunc` for unary functions.
class BinarySymbolicFunc a where
  binarySymbolicFunc :: String -> a -> a -> a

-- | `SimpleExpr` instance of `BinarySymbolicFunc` typeclass.
instance BinarySymbolicFunc SimpleExpr where
  binarySymbolicFunc = twoArgFunc

-- | `RevDiff` instance of `BinarySymbolicFunc` typeclass.
instance
  (BinarySymbolicFunc a, Distributive a, Additive t) =>
  BinarySymbolicFunc (RevDiff t a a)
  where
  binarySymbolicFunc funcName (MkRevDiff x bpx) (MkRevDiff y bpy) =
    MkRevDiff
      (binarySymbolicFunc funcName x y)
      (\cz -> bpx (f'1 * cz) + bpy (f'2 * cz))
    where
      f'1 = binarySymbolicFunc (funcName <> "'_1") x y
      f'2 = binarySymbolicFunc (funcName <> "'_2") x y

-- | A traced version of `SimpleExpr` for debugging purposes.
type TracedSimpleExpr = Traced SimpleExpr

-- | A type alias for `Traced` version of `SimpleExpr`.
type TSE = TracedSimpleExpr

-- | `Traced` instance of `SymbolicFunc` typeclass.
instance
  (SymbolicFunc a, Show a) =>
  SymbolicFunc (Traced a)
  where
  unarySymbolicFunc name (MkTraced x) =
    trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " >>>") $
      MkTraced $
        unarySymbolicFunc name x

-- | `Traced` instance of `BinarySymbolicFunc` typeclass.
instance
  (BinarySymbolicFunc a, Show a) =>
  BinarySymbolicFunc (Traced a)
  where
  binarySymbolicFunc name (MkTraced x) (MkTraced y) =
    trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " and " <> show y <> " >>>") $
      MkTraced $
        binarySymbolicFunc name x y
