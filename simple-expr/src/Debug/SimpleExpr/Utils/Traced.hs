{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}

-- | Module    :  Debug.SimpleExpr.Utils.Traced
-- Copyright   :  (C) 2025 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- This module provides a `Traced` type that wraps values with and adds automatic
-- tracing functionality from
-- 'Debug.Trace'
-- for debugging purposes. When operations are performed
-- on `Traced` values, they output trace messages showing what computations
-- are being performed.
--
-- = Overview
--
-- The `Traced` type is particularly useful for:
--
--   * Debugging complex mathematical computations
--   * Understanding the order of operations in lazy evaluation
--   * Tracking intermediate values in symbolic computations
--   * Educational purposes to visualize computation flow
--
-- = Basic Usage
--
-- >>> import Debug.SimpleExpr.Utils.Traced (traced)
--
-- * Create traced values
--
-- >>> x = traced 3
-- >>> y = traced 4
--
-- * Operations automatically trace
--
-- >>> x + y
--  <<< TRACING: Calculating (+) of 3 and 4 >>>
-- 7
--
-- = Integration with NumHask
--
-- This module integrates with the
-- [numhask](https://hackage.haskell.org/package/numhask)
-- hierarchy, providing instances for:
--
-- >>> import NumHask (Additive, Subtractive, Multiplicative, Divisive, ExpField, TrigField)
--
--   * `Additive` - addition and zero
--   * `Subtractive` - subtraction and negation
--   * `Multiplicative` - multiplication and one
--   * `Divisive` - division
--   * `ExpField` - exponential, logarithm, and power
--   * `TrigField` - trigonometric functions
module Debug.SimpleExpr.Utils.Traced
  ( -- * The Traced Type
    Traced (MkTraced, getTraced),

    -- * Creating Traced Values
    traced,
    untraced,

    -- * Tracing Combinators
    addTraceUnary,
    addTraceBinary,
    addTraceTernary,

    -- * Utility Functions
    withTrace,
    traceShow,
  )
where

import Control.ExtendableMap (ExtandableMap (extendMap))
import Data.Hashable (Hashable)
import Debug.SimpleExpr.Utils.Algebra (AlgebraicPower ((^^)), MultiplicativeAction ((*|)))
import Debug.Trace (trace)
import GHC.Base (Eq, Functor (fmap), String, ($), (.), (<>))
import GHC.Int (Int, Int16, Int32, Int64, Int8)
import GHC.Integer (Integer)
import GHC.Natural (Natural)
import GHC.Num (Num)
import qualified GHC.Num as GN
import GHC.Show (Show (show))
import GHC.Word (Word, Word16, Word32, Word64, Word8)
import NumHask
  ( Additive (zero, (+)),
    Distributive,
    Divisive,
    ExpField (exp, log, (**)),
    FromInteger,
    Integral,
    Multiplicative,
    Subtractive (negate, (-)),
    TrigField (acos, acosh, asin, asinh, atan, atan2, atanh, cos, cosh, pi, sin, sinh),
    fromInteger,
    one,
    zero,
    (*),
    (/),
  )

-- | A wrapper type that adds tracing to any value.
--
-- When operations are performed on `Traced` values, they output
-- trace messages to stderr showing what computations are happening.
--
-- ==== __Examples__
--
-- Basic arithmetic with tracing:
--
-- >>> x = traced 5
-- >>> y = traced 3
-- >>> x * y
--  <<< TRACING: Calculating (*) of 5 and 3 >>>
-- 15
--
-- Tracing can be nested:
--
-- >>> (x + y) * (x - y)
--  <<< TRACING: Calculating (+) of 5 and 3 >>>
--  <<< TRACING: Calculating (-) of 5 and 3 >>>
--  <<< TRACING: Calculating (*) of 8 and 2 >>>
-- 16
newtype Traced a = MkTraced {getTraced :: a}
  deriving (Eq, Hashable, Functor, FromInteger)

-- | Smart constructor for creating traced values.
--
-- This is equivalent to using the `MkTraced` constructor directly,
-- but provides a more descriptive name.
--
-- ==== __Examples__
--
-- >>> traced 42
-- 42
traced :: a -> Traced a
traced = MkTraced

-- | Extract the underlying value from a `Traced` wrapper.
-- It is equivalent to using the `getTraced`.
--
-- ==== __Examples__
--
-- >>> untraced (traced 42)
-- 42
untraced :: Traced a -> a
untraced = getTraced

-- | Apply a unary function with tracing.
--
-- This is the core building block for traced unary operations.
-- It outputs a trace message before applying the function.
--
-- ==== __Examples__
--
-- >>> import GHC.Num (abs)
--
-- >>> absoluteTraced = addTraceUnary "abs" abs
-- >>> absoluteTraced (traced (-5))
--  <<< TRACING: Calculating abs of -5 >>>
-- 5
--
-- Custom unary operations:
--
-- >>> double = addTraceUnary "double" (\x -> x * 2)
-- >>> double (traced 7)
--  <<< TRACING: Calculating double of 7 >>>
-- 14
addTraceUnary :: (Show a) => String -> (a -> b) -> Traced a -> Traced b
addTraceUnary name f (MkTraced x) =
  trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " >>>") $
    MkTraced $
      f x

-- | Apply a binary function with tracing.
--
-- This is the core building block for traced binary operations.
-- It outputs a trace message before applying the function.
--
-- ==== __Examples__
--
-- Basic binary operation:
--
-- >>> two = traced 2
-- >>> three = traced 3
-- >>> addTraced = addTraceBinary "(+)" (+)
-- >>> addTraced two three
--  <<< TRACING: Calculating (+) of 2 and 3 >>>
-- 5
--
-- With symbolic expressions (assuming 'Debug.SimpleExpr' is imported):
--
-- >>> import Debug.SimpleExpr (variable)
-- >>> x = traced $ variable "x"
-- >>> y = traced $ variable "y"
-- >>> z = x + y
-- >>> z ** 2
--  <<< TRACING: Calculating (+) of x and y >>>
--  <<< TRACING: Calculating (**) of x+y and 2 >>>
-- (x+y)^2
addTraceBinary ::
  (Show a, Show b) =>
  String ->
  (a -> b -> c) ->
  Traced a ->
  Traced b ->
  Traced c
addTraceBinary name f (MkTraced x) (MkTraced y) =
  trace
    ( " <<< TRACING: Calculating "
        <> name
        <> " of "
        <> show x
        <> " and "
        <> show y
        <> " >>>"
    )
    $ MkTraced
    $ f x y

-- | Apply a ternary function with tracing.
--
-- Useful for functions that take three arguments.
--
-- ==== __Examples__
--
-- >>> import Data.Ord (Ord(min, max))
--
-- >>> clamp = addTraceTernary "clamp" (\low high x -> max low (min high x))
-- >>> clamp (traced 0) (traced 10) (traced 15)
--  <<< TRACING: Calculating clamp of 0, 10, and 15 >>>
-- 10
addTraceTernary ::
  (Show a, Show b, Show c) =>
  String ->
  (a -> b -> c -> d) ->
  Traced a ->
  Traced b ->
  Traced c ->
  Traced d
addTraceTernary name f (MkTraced x) (MkTraced y) (MkTraced z) =
  trace
    ( " <<< TRACING: Calculating "
        <> name
        <> " of "
        <> show x
        <> ", "
        <> show y
        <> ", and "
        <> show z
        <> " >>>"
    )
    $ MkTraced
    $ f x y z

-- | Execute a computation with a custom trace message.
--
-- This allows you to add custom trace points in your code.
--
-- ==== __Examples__
--
-- >>> withTrace "Starting computation" $ (traced 3) + (traced 4)
--  <<< TRACING: Starting computation >>>
--  <<< TRACING: Calculating (+) of 3 and 4 >>>
-- 7
withTrace :: String -> Traced a -> Traced a
withTrace msg = trace (" <<< TRACING: " <> msg <> " >>>")

-- | Trace the current value with a custom message.
--
-- ==== __Examples__
--
-- >>> x = traced 42
-- >>> traceShow "Current value" x
--  <<< TRACING: Current value: 42 >>>
-- 42
traceShow :: (Show a) => String -> Traced a -> Traced a
traceShow msg t@(MkTraced x) =
  trace (" <<< TRACING: " <> msg <> ": " <> show x <> " >>>") t

-- | Standard `GHC.Base.Num` instance for compatibility with base Haskell.
instance (GN.Num a, Show a) => GN.Num (Traced a) where
  (+) = addTraceBinary "(+)" (GN.+)
  (*) = addTraceBinary "(*)" (GN.*)
  (-) = addTraceBinary "(-)" (GN.-)
  negate = addTraceUnary "negate" GN.negate
  abs = addTraceUnary "abs" GN.abs
  signum = addTraceUnary "signum" GN.signum
  fromInteger = MkTraced . GN.fromInteger

-- | NumHask `Additive` instance for addition operations.
instance (Additive a, Show a) => Additive (Traced a) where
  (+) = addTraceBinary "(+)" (NumHask.+)
  zero = MkTraced zero

-- | NumHask `Subtractive'`instance for subtraction operations.
instance (Subtractive a, Show a) => Subtractive (Traced a) where
  (-) = addTraceBinary "(-)" (NumHask.-)
  negate = addTraceUnary "negate" NumHask.negate

-- | NumHask `Multiplicative` instance for multiplication operations.
instance (Multiplicative a, Show a) => Multiplicative (Traced a) where
  (*) = addTraceBinary "(*)" (NumHask.*)
  one = MkTraced one

#if MIN_VERSION_numhask(0,11,0)
#else
instance Distributive Traced
#endif

-- | NumHask `Divisive` instance for division operations.
instance (Divisive a, Show a) => Divisive (Traced a) where
  (/) = addTraceBinary "(/)" (/)

-- | NumHask `ExpField` instance for exponential and logarithmic operations.
--
-- >>> import NumHask (exp, log)
-- >>> exp (traced 1)
--  <<< TRACING: Calculating exp of 1.0 >>>
-- 2.718281828459045
--
-- >>> log (traced 10)
--  <<< TRACING: Calculating log of 10.0 >>>
-- 2.302585092994046
--
-- >>> (traced 2) ** (traced 3)
--  <<< TRACING: Calculating (**) of 2.0 and 3.0 >>>
-- 8.0
instance (ExpField a, Show a) => ExpField (Traced a) where
  exp = addTraceUnary "exp" exp
  log = addTraceUnary "log" log
  (**) = addTraceBinary "(**)" (**)

-- | NumHask `TrigField` instance for trigonometric operations.
--
-- >>> cos (traced 0)
--  <<< TRACING: Calculating cos of 0.0 >>>
-- 1.0
instance (TrigField a, Show a) => TrigField (Traced a) where
  sin = addTraceUnary "sin" sin
  cos = addTraceUnary "cos" cos
  pi = MkTraced pi
  asin = addTraceUnary "asin" asin
  acos = addTraceUnary "acos" acos
  atan = addTraceUnary "atan" atan
  atan2 = addTraceBinary "atan2" atan2
  sinh = addTraceUnary "sinh" sinh
  cosh = addTraceUnary "cosh" cosh
  asinh = addTraceUnary "asinh" asinh
  acosh = addTraceUnary "acosh" acosh
  atanh = addTraceUnary "atanh" atanh

-- | Show instance that displays the wrapped value.
--
-- Note that this shows the wrapped value, not the `Traced` constructor.
--
-- >>> show (traced 42)
-- "42"
instance (Show a) => Show (Traced a) where
  show (MkTraced a) = show a

-- `Traced` instance of `AlgebraicPower` typeclass from `Debug.SimpleExpr.Utils.Algebra`.
instance
  (Show b, AlgebraicPower a b) =>
  AlgebraicPower a (Traced b)
  where
  x ^^ n = addTraceUnary "(^^)" (^^ n) x

-- `Traced` instance of `MultiplicativeAction` typeclass
-- from `Debug.SimpleExpr.Utils.Algebra`.
instance
  (Show b, MultiplicativeAction a b) =>
  MultiplicativeAction a (Traced b)
  where
  n *| x = addTraceUnary "(*|)" (n *|) x

-- | `Traced` instance fo `ExtandableMap`typecalss.
instance
  (ExtandableMap a b c d) =>
  ExtandableMap a b (Traced c) (Traced d)
  where
  extendMap f (MkTraced x) = MkTraced $ extendMap f x
