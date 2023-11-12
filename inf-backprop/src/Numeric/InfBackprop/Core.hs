{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Module    :  Data.Vector.InfBackpropExtra
-- Copyright   :  (C) 2025 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Backpropagation differentiation core types and functions.
module Numeric.InfBackprop.Core
  ( -- * Common

    -- ** Base
    Tangent,
    Dual,
    Cotangent,
    CT,
    RevDiff (MkRevDiff, value, backprop),
    RevDiff',
    DifferentiableFunc,
    initDiff,
    call,
    derivativeOp,
    toLensOps,
    constDiff,
    StopDiff (stopDiff),
    HasConstant (constant),
    simpleDifferentiableFunc,

    -- ** Relation to lens and profunctors
    toLens,
    fromLens,
    fromProfunctors,
    toProfunctors,
    fromVanLaarhoven,
    toVanLaarhoven,

    -- ** Derivative operators
    AutoDifferentiableArgument,
    DerivativeRoot,
    DerivativeCoarg,
    DerivativeArg,
    AutoDifferentiableValue,
    DerivativeValue,
    autoArg,
    autoVal,
    sameTypeDerivative,
    simpleDerivative,
    simpleValueAndDerivative,
    customArgDerivative,
    customValDerivative,
    customArgValDerivative,

    -- * Differentiable functions

    -- ** Basic
    differentiableSum,
    differentiableSub,
    differentiableNegate,
    differentiableMult,
    differentiableDiv,
    differentiableRecip,
    differentiableMultAction,
    differentiableConv,

    -- ** Exponential and logarithmic functions
    differentiablePow,
    differentiableExp,
    differentiableLog,
    differentiableLogBase,
    differentiableSqrt,

    -- ** Trigonometric functions
    differentiableSin,
    differentiableCos,
    differentiableTan,
    differentiableSinh,
    differentiableCosh,
    differentiableTanh,
    differentiableAsin,
    differentiableAcos,
    differentiableAtan,
    differentiableAtan2,
    differentiableAsinh,
    differentiableAcosh,
    differentiableAtanh,

    -- * Differentiable types

    -- ** Scalar
    scalarArg,
    scalarVal,
    scalarArgDerivative,
    scalarValDerivative,

    -- ** Tuple
    mkTupleArg,
    tupleArg,
    tupleArgDerivative,
    tupleDerivativeOverX,
    tupleDerivativeOverY,
    twoArgsDerivative,
    twoArgsDerivativeOverX,
    twoArgsDerivativeOverY,
    mkTupleVal,
    tupleVal,
    tupleValDerivative,

    -- ** Triple
    threeArgsToTriple,
    tripleArg,
    mkTripleArg,
    tripleArgDerivative,
    tripleDerivativeOverX,
    tripleDerivativeOverY,
    tripleDerivativeOverZ,
    threeArgsDerivative,
    derivative3ArgsOverX,
    derivative3ArgsOverY,
    derivative3ArgsOverZ,
    mkTripleVal,
    tripleVal,
    tripleValDerivative,

    -- ** BoxedVector
    boxedVectorArg,
    mkBoxedVectorArg,
    boxedVectorArgDerivative,
    boxedVectorVal,
    mkBoxedVectorVal,
    boxedVectorValDerivative,

    -- ** Stream
    streamArg,
    mkStreamArg,
    streamArgDerivative,
    streamVal,
    mkStreamVal,
    streamValDerivative,

    -- ** FiniteSupportStream
    finiteSupportStreamArg,
    mkFiniteSupportStreamArg,
    finiteSupportStreamArgDerivative,
    finiteSupportStreamVal,
    mkFiniteSupportStreamVal,
    finiteSupportStreamValDerivative,

    -- ** Maybe
    maybeArg,
    mkMaybeArg,
    maybeArgDerivative,
    maybeVal,
    mkMaybeVal,
    maybeValDerivative,
  )
where

import Control.Applicative ((<$>), (<*>))
import Control.Comonad.Identity (Identity (Identity, runIdentity))
import Control.ExtendableMap (ExtandableMap, extendMap)
import qualified Control.Lens as CL
import Control.Monad.ST (runST)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Composition ((.:))
import Data.Finite (Finite)
import Data.FiniteSupportStream (FiniteSupportStream (MkFiniteSupportStream, toVector), cons, empty, head, singleton, tail, unsafeMap)
import Data.Function (on)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Const (Const (Const, getConst))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (xor)
import Data.Primitive (Prim)
import Data.Profunctor (Profunctor (dimap))
import Data.Profunctor.Strong (Costrong (unfirst, unsecond))
import Data.Proxy (Proxy (Proxy))
import Data.Stream (Stream)
import qualified Data.Stream as DS
import Data.Tuple (curry, fst, snd, uncurry)
import Data.Tuple.Extra ((***))
import Data.Type.Equality (type (~))
import qualified Data.Vector as DV
import qualified Data.Vector.Fixed.Boxed as DVFB
import Data.Vector.Fusion.Util (Box (Box, unBox))
import qualified Data.Vector.Generic as DVG
import Data.Vector.Generic.Base
  ( Vector
      ( basicLength,
        basicUnsafeCopy,
        basicUnsafeFreeze,
        basicUnsafeIndexM,
        basicUnsafeSlice,
        basicUnsafeThaw,
        elemseq
      ),
  )
import qualified Data.Vector.Generic.Base as DVGB
import qualified Data.Vector.Generic.Mutable as DVGM
import qualified Data.Vector.Generic.Mutable.Base as DVGBM
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Generic.Sized.Internal as DVGSI
import qualified Data.Vector.Primitive as DVP
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector.Unboxed.Mutable as DVUM
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Debug.SimpleExpr (SimpleExpr, SimpleExprF)
import Debug.SimpleExpr.Expr (SE, number)
import Debug.SimpleExpr.Utils.Algebra
  ( AlgebraicPower ((^^)),
    Convolution ((|*|)),
    IntegerPower,
    MultiplicativeAction ((*|)),
    (^),
  )
import Debug.SimpleExpr.Utils.Traced (Traced (MkTraced))
import Debug.Trace (trace)
import Foreign (oneBits)
import GHC.Base
  ( Applicative,
    Eq ((==)),
    Float,
    Functor,
    Int,
    Maybe (Just, Nothing),
    Ord (compare, max, min, (<), (<=), (>), (>=)),
    Type,
    const,
    flip,
    fmap,
    id,
    pure,
    return,
    undefined,
    ($),
    (++),
    (.),
    (<*>),
  )
import GHC.Generics (C, Generic, type (:.:) (unComp1))
import GHC.Integer (Integer)
import GHC.Natural (Natural)
import qualified GHC.Num as GHCN
import GHC.Real (Integral, fromIntegral, realToFrac, toInteger)
import qualified GHC.Real as GHCR
import GHC.Show (Show (show))
import GHC.TypeLits (KnownChar)
import GHC.TypeNats (KnownNat, Nat)
import GHC.Types (Int)
import NumHask
  ( Additive,
    AdditiveAction,
    Complex,
    Distributive,
    Divisive,
    ExpField,
    Field,
    FromInteger (fromInteger),
    FromIntegral,
    Multiplicative,
    Subtractive,
    TrigField,
    acos,
    acosh,
    asin,
    asinh,
    atan,
    atan2,
    atanh,
    cos,
    cosh,
    exp,
    fromIntegral,
    log,
    logBase,
    negate,
    one,
    pi,
    recip,
    sin,
    sinh,
    sqrt,
    tan,
    tanh,
    two,
    zero,
    (*),
    (**),
    (+),
    (-),
    (/),
  )
import NumHask.Data.Integral (FromInteger)
import Numeric.InfBackprop.Instances.NumHask ()
import Numeric.InfBackprop.Utils.SizedVector (BoxedVector, boxedVectorBasis, boxedVectorSum)
import Numeric.InfBackprop.Utils.Tuple (cross, cross3, curry3, fork, fork3, uncurry3)
import Optics (Lens, Lens', getting, lens, set, simple, view, (%))

-- | Converts a type into its tangent space type.
type family Tangent (a :: Type) :: Type

type instance Tangent Float = Float

type instance Tangent GHCN.Integer = GHCN.Integer

type instance Tangent SimpleExpr = SimpleExpr

type instance Tangent (a0, a1) = (Tangent a0, Tangent a1)

type instance Tangent (a0, a1, a2) = (Tangent a0, Tangent a1, Tangent a2)

type instance Tangent [a] = [Tangent a]

type instance Tangent (DVFB.Vec n a) = DVFB.Vec n (Tangent a)

type instance Tangent (DVGS.Vector v n a) = DVGS.Vector v n (Tangent a)

type instance Tangent (Stream a) = Stream (Tangent a)

type instance Tangent (FiniteSupportStream a) = FiniteSupportStream (Tangent a)

type instance Tangent (Maybe a) = Maybe (Tangent a)

type instance Tangent (Traced a) = Traced (Tangent a)

type instance Tangent (Complex a) = Complex (Tangent a)

-- | Converts a type into its dual space type.
type family Dual (x :: Type) :: Type

type instance Dual Float = Float

type instance Dual GHCN.Integer = GHCN.Integer

type instance Dual SimpleExpr = SimpleExpr

type instance Dual (a, b) = (Dual a, Dual b)

type instance Dual (a, b, c) = (Dual a, Dual b, Dual c)

type instance Dual [a] = [Dual a]

type instance Dual (DVFB.Vec n a) = DVFB.Vec n (Dual a)

type instance Dual (DVGS.Vector v n a) = DVGS.Vector v n (Dual a)

type instance Dual (Stream a) = FiniteSupportStream (Dual a)

type instance Dual (FiniteSupportStream a) = Stream (Dual a)

type instance Dual (SimpleExprF a) = SimpleExprF (Dual a)

type instance Dual (Maybe a) = Maybe (Dual a)

type instance Dual (Traced a) = Traced (Dual a)

type instance Dual (Complex a) = Complex (Dual a)

-- | Cotangent type alias.
type Cotangent a = Dual (Tangent a)

-- | Cotangent type alias.
type CT a = Cotangent a

-- | Base type for differentiable instances with the backpropagation.
--
-- ==== __Examples__
--
-- >>> :{
--  differentiableSin_ :: RevDiff t Float Float -> RevDiff t Float Float
--  differentiableSin_ (MkRevDiff v bp) = MkRevDiff (sin v) (bp . (cos v *))
-- :}
--
-- >>> value $ differentiableSin_ (MkRevDiff 0.0 id)
-- 0.0
--
-- >>> backprop (differentiableSin_ (MkRevDiff 0.0 id)) 1.0
-- 1.0
--
-- === `GHC.Num.Num` typeclass instance
--
-- This instance enables the use of standard numeric operations and literals
-- directly with `RevDiff` values, simplifying the syntax for
-- automatic differentiation computations.--
-- The instance supports `GHC.Num.Num` operations including arithmetic
-- operators @(+), (-), (*)@, comparison functions (`GHC.Num.abs`, `GHC.Num.signum`), and automatic
-- conversion from integer literals via `fromInteger`.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import GHC.Integer (Integer)
--
-- >>> x = variable "x"
--
-- ===== Using numeric literals in automatic differentiation
--
-- This instance allows `RevDiff` values to be created directly from integer
-- literals, eliminating the need for explicit conversion functions.
--
-- Consider computing the partial derivative:
--
-- \[
--  \left.\frac{\partial}{\partial y} (x \cdot y)\right|_{y=2}
-- \]
--
-- Without the `GHC.Num.Num` instance, we would need explicit conversion:
--
-- >>> simplify $ twoArgsDerivativeOverY (*) x (stopDiff $ number 2) :: SE
-- x
--
-- With the `GHC.Num.Num` instance for `RevDiff`, this simplifies to:
--
-- >>> simplify $ twoArgsDerivativeOverY (*) x (number 2) :: SE
-- x
--
-- And combined with the `GHC.Num.Num` instance for `SE`,
-- we achieve the most concise form:
--
-- >>> simplify $ twoArgsDerivativeOverY (*) x 2
-- x
--
-- This progression shows how the typeclass instances work together to enable
-- increasingly natural mathematical notation.
--
-- ===== Power function differentiation
--
-- The instance enables natural exponentiation syntax with automatic differentiation:
--
-- >>> x ** 3 :: SE
-- x^3
-- >>> simplify $ simpleDerivative (** 3) x :: SE
-- 3*(x^2)
-- >>> simplify $ simpleDerivative (simpleDerivative (** 3)) x :: SE
-- (2*x)*3
--
-- ===== Absolute value and signum functions
--
-- The instance provides symbolic differentiation for absolute value and signum:
--
-- >>> simplify $ simpleDerivative GHCN.abs (variable "x") :: SE
-- sign(x)
--
-- >>> simplify $ simpleDerivative GHCN.signum (variable "x") :: SE
-- 0
--
-- For numeric evaluation, the second derivative of absolute value at a point
-- gives the expected result:
--
-- >>> (simpleDerivative (simpleDerivative GHCN.abs)) (1 :: Float) :: Float
-- 0.0
--
-- Notice that the signum function returns zero for all values, including zero.
--
-- >>> simpleDerivative GHCN.signum (0 :: Float) :: Float
-- 0.0
--
-- >>> simplify $ (simpleDerivative (simpleDerivative GHCN.abs)) (variable "x") :: SE
-- 0
--
-- === `GHCR.Fractional` typeclass instance
--
-- Thank to this instance we can use numerical literals like '1.0', '2.0', etc.,
-- see the examples below.
--
-- ==== __Examples__
--
-- >>> import GHC.Float (Float)
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
--
-- >>> f x = 8 / x
-- >>> simpleDerivative f (2.0 :: Float)
-- -2.0
-- >>> simplify $ simpleDerivative f (variable "x") :: SE
-- -((8/x)/x)
data RevDiff a b c = MkRevDiff {value :: c, backprop :: b -> a}
  deriving (Generic)

-- | Type alias for common case where the backpropagation is in the cotangent space.
type RevDiff' a b = RevDiff (CT a) (CT b) b

type instance Tangent (RevDiff a b c) = RevDiff a (Tangent b) (Tangent c)

type instance Dual (RevDiff a b c) = RevDiff a (Dual b) (Dual c)

-- | Converts a differentiable function into a regular function.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable)
-- >>> import Debug.DiffExpr (unarySymbolicFunc)
--
-- >>> :{
--  differentiableCos_ :: RevDiff t Float Float -> RevDiff t Float Float
--  differentiableCos_ (MkRevDiff v bp) = MkRevDiff (cos v) (bp . negate . (sin v *))
-- :}
--
-- >>> call differentiableCos_ 0.0
-- 1.0
--
-- >>> x = variable "x"
-- >>> f = unarySymbolicFunc "f"
-- >>> f x
-- f(x)
--
-- >>> call f x
-- f(x)
call :: (RevDiff' a a -> RevDiff' a b) -> a -> b
call f = value . f . initDiff

-- | Converts a differentiable function into into its derivative in the form of
-- multiplicative operator.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable)
-- >>> import Debug.DiffExpr (unarySymbolicFunc)
--
-- >>> :{
--   differentiableSin_ :: RevDiff t Float Float -> RevDiff t Float Float
--   differentiableSin_ (MkRevDiff v bp) = MkRevDiff (sin v) (bp . (cos v *))
-- :}
--
-- >>> (derivativeOp differentiableSin_ 0.0) 1.0
-- 1.0
--
-- >>> c = variable "c"
-- >>> x = variable "x"
-- >>> f = unarySymbolicFunc "f"
-- >>> f x
-- f(x)
-- >>> (derivativeOp f x) c
-- f'(x)*c
derivativeOp :: (RevDiff' a a -> RevDiff' a b) -> a -> CT b -> CT a
derivativeOp f = backprop . f . initDiff

-- | Converts a function into a pair of its value and backpropagation function,
-- which are the lense get and set functions, respectively.
toLensOps :: (RevDiff ca ca a -> RevDiff ca cb b) -> a -> (b, cb -> ca)
toLensOps f x = (y, bp)
  where
    MkRevDiff y bp = f $ initDiff x

-- | Creates a differentiable function from a function and its derivative.
-- This is a convenience function for defining new differentiable operations.
--
-- ==== __Examples__
--
-- >>> :{
--  differentiableCos_ :: RevDiff t Float Float -> RevDiff t Float Float
--  differentiableCos_ = simpleDifferentiableFunc cos (negate . sin)
-- :}
--
-- >>> call differentiableCos_ 0.0
-- 1.0
--
-- >>> simpleDerivative differentiableCos_ 0.0
-- -0.0
simpleDifferentiableFunc ::
  (Multiplicative b) =>
  (b -> b) ->
  (b -> b) ->
  RevDiff a b b ->
  RevDiff a b b
simpleDifferentiableFunc f f' (MkRevDiff x bpc) = MkRevDiff (f x) (\cy -> bpc $ f' x * cy)

-- | Initializes a `MkRevDiff` instance with given value
-- and identity backpropagation function.
-- This is useful for starting the backpropagation chain.
--
-- ==== __Examples__
--
-- >>> :{
--   differentiableCos_ :: RevDiff t Float Float -> RevDiff t Float Float
--   differentiableCos_ (MkRevDiff v bp) = MkRevDiff (cos v) (bp . negate . (sin v *))
-- :}
--
-- >>> value $ differentiableCos_ (initDiff 0.0)
-- 1.0
--
-- >>> backprop (differentiableCos_ (initDiff 0.0)) 1.0
-- -0.0
initDiff :: a -> RevDiff b b a
initDiff x = MkRevDiff x id

-- | Converts a differentiable function into a /law-breaking/ 'Lens'.
-- This is mutually inverse with 'fromLens'.
--
-- ==== __Examples__
--
-- >>> import Optics (Lens', lens, view, set, getting, (%))
-- >>> import Debug.SimpleExpr (variable, SE)
--
-- >>> sinLens = toLens sin :: Lens' SE SE
-- >>> x = variable "x"
-- >>> c = variable "c"
-- >>> (view . getting) sinLens x
-- sin(x)
-- >>> set sinLens c x
-- cos(x)*c
-- >>> squareLens = toLens (^2) :: Lens' SE SE
-- >>> (view . getting) (squareLens % sinLens) x
-- sin(x^2)
toLens :: (RevDiff b b a -> RevDiff b d c) -> Lens a b c d
toLens f = lens (value . bp) (backprop . bp)
  where
    bp = f . initDiff

-- | Converts a /law-breaking/ 'Lens' into a differentiable function.
-- This is mutually inverse with 'toLens'.
--
-- ==== __Examples__
--
-- >>> import Optics (lens)
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
--
-- >>> sinV2 = fromLens $ lens sin (\x -> (cos x *))
-- >>> x = variable "x"
-- >>> c = variable "c"
-- >>> call sinV2 x
-- sin(x)
-- >>> simplify $ simpleDerivative sinV2 x :: SE
-- cos(x)
fromLens :: Lens a (CT a) b (CT b) -> RevDiff' a a -> RevDiff' a b
fromLens l (MkRevDiff x bp) = MkRevDiff ((view . getting) l x) (\cy -> bp $ set l cy x)

-- | Profunctor instance for `RevDiff`.
instance Profunctor (RevDiff t) where
  dimap :: (a -> b) -> (c -> d) -> RevDiff t b c -> RevDiff t a d
  dimap f g (MkRevDiff v bp) = MkRevDiff (g v) (bp . f)

-- | Costrong instance for `RevDiff`.
instance Costrong (RevDiff t) where
  unfirst :: RevDiff t (a, d) (b, d) -> RevDiff t a b
  unfirst (MkRevDiff v bp) = MkRevDiff (fst v) (bp . (,snd v))
  unsecond :: RevDiff t (d, a) (d, b) -> RevDiff t a b
  unsecond (MkRevDiff v bp) = MkRevDiff (snd v) (bp . (fst v,))

-- | Type `DifferentiableFunc`@ a b@ may be associated with the differentiable
-- functions from @a@ to @b@.
-- Composition `(.)` of
-- @DifferentiableFunc b c@ and @DifferentiableFunc a b@ is @DifferentiableFunc a c@
-- by definition.
--
-- See `fromProfunctors`, `toProfunctors`, `fromVanLaarhoven` and `fromVanLaarhoven`
-- for illustraing how to use this type.
--
-- ==== __Examples__
--
-- >>> :{
--  differentiableCos_ :: DifferentiableFunc Float Float
--  differentiableCos_ (MkRevDiff x bpc) = MkRevDiff (cos x) (bpc . ((negate $ sin x) *))
-- :}
--
-- >>> call differentiableCos_ 0.0
-- 1.0
--
-- >>> simpleDerivative differentiableCos_ 0.0
-- -0.0
type DifferentiableFunc a b = forall t. RevDiff t (CT a) a -> RevDiff t (CT b) b

-- Profunctor and Van Laarhoven representations.

-- | Transorfms profunctor (Costrong) map into a 'RevDiff' map.
-- Inverse of 'toProfunctors'.
fromProfunctors ::
  (forall p. (Costrong p) => p (CT a) a -> p (CT b) b) -> DifferentiableFunc a b
fromProfunctors = id

-- | Profunctor representation of the `RevDiff` like for lens map in the spirit of optics.
-- Inverse of `fromProfunctors`.
toProfunctors ::
  -- (RevDiff a a -> RevDiff a b) ->
  -- (RevDiff (CT a) (CT a) a -> RevDiff (CT a) (CT b) b) ->
  (Costrong p) =>
  DifferentiableFunc a b ->
  p (CT a) a ->
  p (CT b) b
toProfunctors f = unsecond . dimap (uncurry u) (fork id v)
  where
    v = call f
    u = derivativeOp f

-- Van Laarhoven representation of the `RevDiff` type.

-- | Converts a Van Laarhoven representation to a function over `RevDiff` types
-- Inverse of `toVanLaarhoven`.
fromVanLaarhoven ::
  forall a b.
  (forall f. (Functor f) => (b -> f (CT b)) -> a -> f (CT a)) ->
  DifferentiableFunc a b
-- RevDiff t a ->
-- RevDiff t b
fromVanLaarhoven vll (MkRevDiff x bpx) = MkRevDiff y (bpx . bp)
  where
    (y, bp) = getCompose $ vll (\y_ -> Compose (y_, id)) x

-- | Converts a function over `RevDiff` types into a Van Laarhoven representation.
-- Inverse of `fromVanLaarhoven`.
toVanLaarhoven ::
  (Functor f) =>
  -- (RevDiff a a -> RevDiff a b) ->
  DifferentiableFunc a b ->
  (b -> f (CT b)) ->
  a ->
  f (CT a)
toVanLaarhoven g f x = fmap bp (f y)
  where
    MkRevDiff y bp = g $ initDiff x

-- -- | Performs backpropagation starting from 'one' and returns the result.
-- -- In particular,
-- -- for constant functions, this will return zero since their derivative is zero.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> diff $ initDiff (42.0 :: Float) :: Float
-- -- 1.0
-- --
-- -- >>> diff (constDiff 42.0 :: RevDiff Float Float Float) :: Float
-- -- 0.0
-- diff :: (Multiplicative b) => RevDiff a b c -> a
-- diff x = backprop x one

-- | Creates a constant differentiable function.
-- The derivative of a constant function is always zero.
--
-- ==== __Examples__
--
-- >>> value (constDiff 42.0 :: RevDiff' Float Float)
-- 42.0
--
-- >>> backprop (constDiff 42.0 :: RevDiff' Float Float) 1.0
-- 0.0
constDiff :: (Additive a) => c -> RevDiff a b c
constDiff x = MkRevDiff x (const zero)

-- | Derivative for a scalar-to-scalar function.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SimpleExpr)
-- >>> import Debug.DiffExpr (unarySymbolicFunc)
--
-- >>> simpleDerivative sin (0.0 :: Float)
-- 1.0
--
-- >>> x = variable "x"
--
-- >>> simplify $ simpleDerivative (^ 2) x
-- 2*x
--
-- >>> f = unarySymbolicFunc "f"
--
-- >>> simplify $ simpleDerivative f x :: SimpleExpr
-- f'(x)
simpleDerivative ::
  forall a b.
  (Multiplicative (CT b)) =>
  (RevDiff' a a -> RevDiff' a b) ->
  a ->
  CT a
simpleDerivative f x = backprop (f (initDiff x)) one

-- | Derivative of a function from any type to the same type.
-- The type structure of the input and output values must be the same.
--
-- ==== __Examples__
--
-- >>> f = sin :: TrigField a => a -> a
-- >>> f' = sameTypeDerivative f :: Float -> Float
--
-- >>> f' 0.0
-- 1.0
sameTypeDerivative ::
  (Multiplicative (CT a)) =>
  (RevDiff (CT a) (CT a) a -> RevDiff (CT a) (CT a) a) ->
  a ->
  CT a
sameTypeDerivative = simpleDerivative

-- | Returns both the value and the derivative for a scalar-to-scalar function.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SimpleExpr)
-- >>> import Debug.DiffExpr (unarySymbolicFunc)
--
-- >>> simpleValueAndDerivative sin (0.0 :: Float)
-- (0.0,1.0)
--
-- >>> x = variable "x"
-- >>> f = unarySymbolicFunc "f"
--
-- >>> simplify $ simpleValueAndDerivative f x :: (SimpleExpr, SimpleExpr)
-- (f(x),f'(x))
simpleValueAndDerivative ::
  forall a b.
  (Multiplicative (CT b)) =>
  (RevDiff' a a -> RevDiff' a b) ->
  a ->
  (b, CT a)
simpleValueAndDerivative f x = (value out, backprop out one)
  where
    out = f (initDiff x)

-- | Derivative of a function from any type to any type.
-- The type structure of the input and output values must be specified
-- in the first and second arguments, respectively.
-- The output value type of the derivative is infereced automatically.
--
-- ==== __Examples__
--
-- >>> :{
--    sphericToVec :: (TrigField a) =>
--      (a, a) -> BoxedVector 3 a
--    sphericToVec (theta, phi) = DVGS.fromTuple (cos theta * cos phi, cos theta * sin phi, sin theta)
-- :}
--
-- >>> sphericToVec' = customArgValDerivative tupleArg boxedVectorVal sphericToVec
--
-- Here 'tupleArg' manifests that the argument type is a tuple.
-- The second term 'boxedVectorVal' specifies that the output value type is a boxed vector.
--
-- >>> sphericToVec' (0 :: Float, 0 :: Float)
-- Vector [(0.0,0.0),(0.0,1.0),(1.0,0.0)]
customArgValDerivative ::
  (RevDiff (CT a) (CT a) a -> b) ->
  (c -> d) ->
  (b -> c) ->
  a ->
  d
customArgValDerivative argTerm valTerm f = valTerm . f . argTerm . initDiff

-- | Axulary type for building nested argument structure descriptors.
type RevDiffArg a b c d = RevDiff a b c -> d

-- | Typeclass needed for the automatic agrument descriptor derivation.
-- See instance implementations for `RevDiff`, tuple and `BoxedVector` below.
--
-- ==== __Examples__
--
-- >>> :{
--  sphericToVector :: (TrigField a) =>
--    (a, a) -> BoxedVector 3 a
--  sphericToVector (theta, phi) =
--    DVGS.fromTuple (cos theta * cos phi, cos theta * sin phi, sin theta)
-- :}
--
-- >>> sphericToVector' = customArgValDerivative autoArg boxedVectorVal sphericToVector
-- >>> sphericToVector' (0 :: Float, 0 :: Float)
-- Vector [(0.0,0.0),(0.0,1.0),(1.0,0.0)]
class
  (Additive (DerivativeRoot a), Additive (DerivativeCoarg a)) =>
  AutoDifferentiableArgument a
  where
  -- | Differentiable function root
  type DerivativeRoot a :: Type

  -- | Differentiable function coargument
  type DerivativeCoarg a :: Type

  -- | Differentiable functin argument
  type DerivativeArg a :: Type

  -- | Automatic argument descriptor.
  autoArg :: RevDiff (DerivativeRoot a) (DerivativeCoarg a) (DerivativeArg a) -> a

-- | `AutoDifferentiableArgument` instance for the scalar argument term.
instance
  (Additive a, Additive b) =>
  AutoDifferentiableArgument (RevDiff a b c)
  where
  type DerivativeRoot (RevDiff a b c) = a
  type DerivativeCoarg (RevDiff a b c) = b
  type DerivativeArg (RevDiff a b c) = c
  autoArg = id

-- | Typeclass needed for the automatic value term derivation.
--
-- ==== __Examples__
--
-- >>> :{
--    sphericToVector :: (TrigField a) =>
--      (a, a) -> BoxedVector 3 a
--    sphericToVector (theta, phi) = DVGS.fromTuple (cos theta * cos phi, cos theta * sin phi, sin theta)
-- :}
--
-- >>> sphericToVector' = customArgValDerivative tupleArg autoVal sphericToVector
-- >>> sphericToVector' (0 :: Float, 0 :: Float)
-- Vector [(0.0,0.0),(0.0,1.0),(1.0,0.0)]
class AutoDifferentiableValue a where
  -- | Differentiable function value type.
  type DerivativeValue a :: Type

  -- | Automatic value descriptor.
  autoVal :: a -> DerivativeValue a

-- | Scalar value term.
--
-- ==== __Examples__
--
-- >>> :{
--    product :: (Multiplicative a) => (a, a) -> a
--    product (x, y) = x * y
-- :}
--
-- >>> product' = customArgValDerivative tupleArg scalarVal product
--
-- >>> product' (2 :: Float, 3 :: Float)
-- (3.0,2.0)
--
-- >>> import Debug.SimpleExpr (variable, simplify, SimpleExpr)
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> simplify $ product' (x, y) :: (SimpleExpr, SimpleExpr)
-- (y,x)
scalarVal ::
  (Multiplicative b) =>
  RevDiff a b c ->
  a
scalarVal (MkRevDiff _ bp) = bp one

-- | `AutoDifferentiableValue` instance for the scalar value term.
instance
  (Multiplicative b) =>
  AutoDifferentiableValue (RevDiff a b c)
  where
  type DerivativeValue (RevDiff a b c) = a
  autoVal :: RevDiff a b c -> a
  autoVal = scalarVal

-- | Derivative operator for a function with a specified argument type,
-- but with the value type derived automatically.
--
-- ==== __Examples__
--
-- >>> :{
--    sphericToVec :: (TrigField a) =>
--      (a, a) -> BoxedVector 3 a
--    sphericToVec (theta, phi) = DVGS.fromTuple (cos theta * cos phi, cos theta * sin phi, sin theta)
-- :}
--
-- >>> sphericToVec' = customArgDerivative tupleArg sphericToVec
--
-- Here 'tupleArg' indicates that the argument type is a tuple.
--
-- >>> sphericToVec' (0 :: Float, 0 :: Float)
-- Vector [(0.0,0.0),(0.0,1.0),(1.0,0.0)]
customArgDerivative ::
  (AutoDifferentiableValue c) =>
  (RevDiff (CT a) (CT a) a -> b) ->
  (b -> c) ->
  a ->
  DerivativeValue c
customArgDerivative arg = customArgValDerivative arg autoVal

-- | Derivative operator for a function with specified argument type
-- but automatically derived value type.
--
-- ==== __Examples__
--
-- >>> :{
--    sphericToVector :: (TrigField a) =>
--      (a, a) -> BoxedVector 3 a
--    sphericToVector (theta, phi) = DVGS.fromTuple (cos theta * cos phi, cos theta * sin phi, sin theta)
-- :}
--
-- >>> sphericToVector' = customValDerivative boxedVectorVal sphericToVector
--
-- The term 'boxedVectorVal' specifies that the output value type is a boxed vector.
--
-- >>> sphericToVector' (0 :: Float, 0 :: Float)
-- Vector [(0.0,0.0),(0.0,1.0),(1.0,0.0)]
customValDerivative ::
  ( DerivativeRoot b ~ CT (DerivativeArg b),
    DerivativeCoarg b ~ CT (DerivativeArg b),
    AutoDifferentiableArgument b
  ) =>
  (c -> d) ->
  (b -> c) ->
  DerivativeArg b ->
  d
customValDerivative = customArgValDerivative autoArg

-- Scalar

-- | Scalar (trivial) argument descriptor for differentiable functions.
--
-- ==== __Examples__
--
-- >>> import Debug.DiffExpr (unarySymbolicFunc, SymbolicFunc)
-- >>> import Debug.SimpleExpr (variable, SimpleExpr, simplify, SE)
--
-- >>> scalarArgDerivative = customArgDerivative scalarArg
--
-- >>> t = variable "t"
-- >>> :{
--   v :: SymbolicFunc  a => a -> BoxedVector 3 a
--   v t = DVGS.fromTuple (
--      unarySymbolicFunc "v_x" t,
--      unarySymbolicFunc "v_y" t,
--      unarySymbolicFunc "v_z" t
--    )
-- :}
--
-- >>> v t
-- Vector [v_x(t),v_y(t),v_z(t)]
--
-- >>> v' = simplify . scalarArgDerivative v :: SE -> BoxedVector 3 SE
-- >>> v' t
-- Vector [v_x'(t),v_y'(t),v_z'(t)]
scalarArg :: RevDiff a b c -> RevDiff a b c
scalarArg = id

-- | Derivative operator for a function from a scalar to any supported value type.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
--
-- >>> :{
--   f :: TrigField a => a -> (a, a)
--   f t = (cos t, sin t)
-- :}
--
-- >>> f' = scalarArgDerivative f
--
-- >>> f (0 :: Float)
-- (1.0,0.0)
-- >>> f' (0 :: Float)
-- (-0.0,1.0)
--
-- >>> t = variable "t"
-- >>> f t
-- (cos(t),sin(t))
-- >>> simplify $ scalarArgDerivative f t :: (SE, SE)
-- (-(sin(t)),cos(t))
scalarArgDerivative ::
  (AutoDifferentiableValue c) =>
  (RevDiff' a a -> c) ->
  a ->
  DerivativeValue c
scalarArgDerivative = customArgValDerivative id autoVal

-- | Derivative operator for a function
-- from any supported argument type to a scalar value.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
--
-- >>> :{
--   f :: Additive a => (a, a) -> a
--   f (x, y) = x + y
-- :}
--
-- >>> f (2 :: Float, 3 :: Float)
-- 5.0
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> f (x, y)
-- x+y
--
-- >>> :{
--   f' :: (Additive a, Distributive (CT a)) => (a, a) -> (CT a, CT a)
--   f' = scalarValDerivative f
-- :}
--
-- >>> f' (2 :: Float, 3 :: Float)
-- (1.0,1.0)
-- >>> simplify $ f' (x, y) :: (SE, SE)
-- (1,1)
scalarValDerivative ::
  ( DerivativeRoot b ~ CT a,
    DerivativeCoarg b ~ CT a,
    DerivativeArg b ~ a,
    Multiplicative (CT c),
    AutoDifferentiableArgument b
  ) =>
  (b -> RevDiff d (CT c) c) ->
  a ->
  d
scalarValDerivative = customArgValDerivative autoArg scalarVal

-- RevDiff type instances

-- | `RevDiff` instance for the `Show` typeclass.
instance (Show (b -> a), Show c) => Show (RevDiff a b c) where
  show (MkRevDiff x bp) = "MkRevDiff " ++ show x ++ " " ++ show bp

-- | Typeclass for the automatic iterrupt of the backpropagation.
--
-- ==== __Examples__
--
-- >>> :{
--    simpleDerivative
--      (\x -> x * twoArgsDerivativeOverY (+) x (stopDiff (1 :: Float)))
--      (2024 :: Float)
-- :}
-- 1.0
class StopDiff a b where
  -- | Stops differentiation by converting a nested `RevDiff` type
  -- into a non-differentiable type.
  stopDiff :: a -> b

-- | Base case: stopping differentiation for the same type.
instance StopDiff a a where
  stopDiff = id

-- | Recursive case: stopping differentiation for `RevDiff` type.
instance
  (StopDiff a d, Additive b) =>
  StopDiff a (RevDiff b c d)
  where
  stopDiff = constDiff . stopDiff

-- | Typeclass for creating constant differentiable functions.
class HasConstant a b c d where
  constant :: Proxy a -> b -> c -> d

-- | Base case: constant function for the same type.
instance HasConstant a b a b where
  constant _ x _ = x

-- | Recursive case: constant function for `RevDiff` type.
instance
  forall a b c d e f t.
  (HasConstant a b c d, Additive t, e ~ CT c, f ~ CT d) =>
  HasConstant a b (RevDiff t e c) (RevDiff t f d)
  where
  constant _ x (MkRevDiff v _) = constDiff $ constant (Proxy @a) x v

-- | Differentiable version of sum `(+)` for the `RevDiff` type.
--
-- This function implements automatic differentiation for addition by applying
-- the sum rule:
-- \[
--  \frac{d}{dx} (f(x) + g(x)) = \frac{df(x)}{dx} + \frac{dg(x)}{dx}
-- \].
-- The gradient flows equally to
-- both operands during backpropagation.
differentiableSum ::
  (Additive c) =>
  RevDiff a (b, b) (c, c) ->
  RevDiff a b c
differentiableSum (MkRevDiff (x0, x1) bpc) =
  MkRevDiff (x0 + x1) (\cy -> bpc (cy, cy))

-- | `Additive` instance for the `RevDiff` type.
instance
  (Additive a, Additive c) =>
  Additive (RevDiff a b c)
  where
  zero = constDiff zero
  (+) = differentiableSum .: twoArgsToTuple

-- | Differentiable version of subtraction `(-)` for the `RevDiff` type.
--
-- Implements the difference rule:
-- \[
--  \frac{d}{dx} (f(x) - g(x)) = \frac{df(x)}{dx} - \frac{dg(x)}{dx}.
-- \]
-- Duringt the backpropagation, the gradient flows positively to the first operand
-- and negatively to the second operand.
differentiableSub ::
  (Subtractive b, Subtractive c) =>
  RevDiff a (b, b) (c, c) ->
  RevDiff a b c
differentiableSub (MkRevDiff (x0, x1) bpc) =
  MkRevDiff (x0 - x1) (\cy -> bpc (cy, negate cy))

-- | Differentiable version of sign change function `negate` for `RevDiff` type.
--
-- Implements the negation rule:
-- \[
--  \frac{d}{dx} (-f(x)) = -\frac{df(x)}{dx}.
-- \]
-- The gradient is simply
-- negated during backpropagation.
differentiableNegate ::
  (Subtractive a, Subtractive c) =>
  RevDiff a b c ->
  RevDiff a b c
differentiableNegate (MkRevDiff x bp) = MkRevDiff (negate x) (negate . bp)

-- | `Subtractive` instance for the `RevDiff` type.
instance
  ( Additive a,
    Subtractive a,
    Subtractive b,
    Subtractive c
  ) =>
  Subtractive (RevDiff a b c)
  where
  negate = differentiableNegate
  (-) = differentiableSub .: twoArgsToTuple

-- | Differentiable version of commutative multiplication `(*)` for the `RevDiff` type.
--
-- Implements the product rule:
-- \[
--  \frac{d}{dx} (f(x) \cdot g(x)) = f(x) \cdot \frac{d g(x)}{dx} + \frac{df(x)}{dx} \cdot g(x).
-- \]
-- Each operand receives the gradient multiplied by the value of the other operand.
differentiableMult ::
  (Multiplicative b) =>
  RevDiff a (b, b) (b, b) ->
  RevDiff a b b
differentiableMult (MkRevDiff (x0, x1) bpc) =
  MkRevDiff (x0 * x1) (\cy -> bpc (x1 * cy, x0 * cy))

-- | `Multiplicative` instance for the `RevDiff` type.
instance
  (Additive a, Multiplicative b) =>
  Multiplicative (RevDiff a b b)
  where
  one = constDiff one
  (*) = differentiableMult .: twoArgsToTuple

#if MIN_VERSION_numhask(0,11,0)
#else
instance (Additive (CT a), b ~ CT b, Distributive b) => Distributive (RevDiff' a b)
instance (Additive (CT a), Ring b) => Ring (RevDiff' a b)
instance (Additive (CT a), Field b) => Field (RevDiff' a b)
#endif

instance
  (MultiplicativeAction Integer b, MultiplicativeAction Integer cb) =>
  MultiplicativeAction Integer (RevDiff ct cb b)
  where
  c *| (MkRevDiff x bp) = MkRevDiff (c *| x) (bp . (c *|))

-- | Differentiable version of multiplicative action `(*|)` for the `RevDiff` type.
--
-- Implements the product rule for scalar \( f \)
-- and, for example, vector \( g_i \):
--
-- \[
--  \frac{d}{dx} \left( f(x) \cdot g_i(x) \right) =
--  f(x) \cdot \frac{d g_i(x)}{dx} + \frac{df(x)}{dx} \cdot g_i(x).
-- \]
-- Each operand receives the gradient multiplied by the value of the other operand.
differentiableMultAction ::
  (MultiplicativeAction a b, MultiplicativeAction a cb, Convolution b cb ca) =>
  RevDiff ct (ca, cb) (a, b) ->
  RevDiff ct cb b
differentiableMultAction (MkRevDiff (x, y) bpc) =
  MkRevDiff (x *| y) (\cz -> bpc (y |*| cz, x *| cz))

instance
  (MultiplicativeAction a b, MultiplicativeAction a cb, Convolution b cb ca, Additive ct) =>
  MultiplicativeAction (RevDiff ct ca a) (RevDiff ct cb b)
  where
  (*|) = differentiableMultAction .: twoArgsToTuple

-- | Differentiable version of convolution `(|*|)` for the `RevDiff` type.
--
-- Implements the product rule for, for example, vectors
-- \( f_i \)
-- and
-- \( g_i \):
--
-- \[
--  \frac{d}{dx} \sum_i f_i(x) \cdot g_i(x) =
--  \sum_i f_i(x) \cdot \frac{d g_i(x)}{dx} + \frac{d f_i(x)}{dx} \cdot g_i(x)
-- \]
-- Each operand receives the gradient multiplied by the value of the other operand.
differentiableConv ::
  (Convolution a b c, Convolution cc b ca, Convolution a cc cb) =>
  RevDiff ct (ca, cb) (a, b) ->
  RevDiff ct cc c
differentiableConv (MkRevDiff (x, y) bpc) =
  MkRevDiff (x |*| y) (\cz -> bpc (cz |*| y, x |*| cz))

instance
  (Convolution a b c, Convolution cc b ca, Convolution a cc cb, Additive ct) =>
  Convolution (RevDiff ct ca a) (RevDiff ct cb b) (RevDiff ct cc c)
  where
  (|*|) = differentiableConv .: twoArgsToTuple

-- | Differentiable version of division `(/)` for the `RevDiff` type.
--
-- Implements the quotient rule:
-- \[
--  \frac{d}{dx} (f(x)/g(x)) =
--  \frac{\frac{df(x)}{dx} \cdot g(x) - f(x) \cdot \frac{dg(x)}{dx}}{g^2(x)}.
-- \]
-- The numerator receives gradient divided by the denominator, while the
-- denominator receives negative gradient scaled by the quotient divided by itself.
differentiableDiv ::
  (Subtractive b, Divisive b) =>
  RevDiff a (b, b) (b, b) ->
  RevDiff a b b
differentiableDiv (MkRevDiff (x0, x1) bpc) =
  MkRevDiff (x0 / x1) (\cy -> bpc (cy / x1, negate $ x0 / x1 / x1 * cy))

-- | Differentiable version of `recip` for `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \frac{1}{f(x)} = -\frac{1}{f^2(x)} \cdot \frac{df(x)}{dx}.
-- \]
-- The gradient is scaled by the negative
-- square of the reciprocal.
differentiableRecip ::
  (Divisive b, Subtractive b, IntegerPower b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableRecip (MkRevDiff x bpc) = MkRevDiff r (bpc . negate . (r ^ 2 *))
  where
    r = recip x

-- | `Divisive` instance for the `RevDiff` type.
instance
  (Additive a, Divisive b, Subtractive b, IntegerPower b) =>
  Divisive (RevDiff a b b)
  where
  recip = differentiableRecip
  (/) = differentiableDiv .: twoArgsToTuple

-- | Differentiable version of exponentiation `(**)` for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} f^{g(x)}(x) = f^{g(x)}(x) \cdot (\log f(x) \cdot \frac{dg(x)}{dx} + \frac{g(x)}{f(x)} \cdot \frac{df(x)}{dx}),
-- \]
-- handling both base
-- and exponent dependencies in the gradient computation.
differentiablePow ::
  (ExpField b) =>
  RevDiff a (b, b) (b, b) ->
  RevDiff a b b
differentiablePow (MkRevDiff (x, p) bpc) =
  MkRevDiff xp (\cy -> bpc (p * (x ** (p - one)) * cy, log x * xp * cy))
  where
    xp = x ** p

-- | Differentiable version of `exp` for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \exp{f(x)} = \exp{f(x)} \cdot \frac{df(x)}{dx}.
-- \]
-- The exponential function is its own derivative,
-- making the gradient computation particularly elegant.
differentiableExp ::
  (ExpField b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableExp (MkRevDiff x bp) = MkRevDiff y (bp . (y *))
  where
    y = exp x

-- | Differentiable version of natural logarithm for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \log \left| f(x) \right| = \frac{1}{f(x)} \cdot \frac{df(x)}{dx}.
-- \]
-- For real numbers, this computes
-- the derivative of
-- \(\log |x|\),
-- which is defined for all non-zero values.
--
-- Unsafety note: This function and derivative will raise an error if @f@ is zero, as the
-- logarithm and `recip` from @numhask@ is undefined at zero point.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
--
-- >>> simplify $ simpleDerivative differentiableLog (variable "x") :: SE
-- 1/x
differentiableLog ::
  (ExpField b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableLog (MkRevDiff x bp) = MkRevDiff (log x) (bp . (/ x))

-- | Differentiable version of `logBase` for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \log_b f(x)
-- \]
-- where both base and argument may be differentiable.
-- Uses the change of base formula and applies the chain rule appropriately.
differentiableLogBase ::
  (ExpField b, IntegerPower b) =>
  RevDiff a (b, b) (b, b) ->
  RevDiff a b b
differentiableLogBase (MkRevDiff (b, x) bpc) =
  MkRevDiff
    (logX / logB)
    (\cy -> bpc (negate $ logX / (logB ^ 2) / b * cy, recip x / logB * cy))
  where
    logX = log x
    logB = log b

-- | Differentiable version of `sqrt` for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \sqrt{f(x)} = \frac{1}{2 \sqrt {f(x)}} \cdot \frac{df(x)}{dx}.
-- The gradient is scaled by the
-- reciprocal of twice the square root of the input.
differentiableSqrt ::
  (ExpField b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableSqrt (MkRevDiff x bp) = MkRevDiff y (\cy -> bp $ recip (two * y) * cy)
  where
    y = sqrt x

-- | `ExpField` instance for the `RevDiff` type.
instance
  (ExpField b, Additive a, Subtractive a, IntegerPower b) =>
  ExpField (RevDiff a b b)
  where
  exp = differentiableExp
  log = differentiableLog
  (**) = differentiablePow .: twoArgsToTuple
  logBase = differentiableLogBase .: twoArgsToTuple
  sqrt = differentiableSqrt

-- | Differentiable version of `atan2` for the `RevDiff` type.
--
-- Computes the two-argument arctangent function:
-- \[
--  \mathrm{arctg2}(y, x) = \arctg\left(\frac{y}{x}\right)
-- \]
--
-- The gradient computation accounts for both arguments using the formula:
-- \[
--  \frac{d}{dx} \mathrm{arctg2}(f(x),g(x)) =
--  - \frac{g(x)}{f(x)^2+g(x)^2} \cdot \frac{df(x)}{dx}
--  + \frac{f(x)}{f(x)^2+g(x)^2} \cdot \frac{dg(x)}{dx}
-- \]
differentiableAtan2 ::
  (TrigField b, IntegerPower b) =>
  RevDiff a (b, b) (b, b) ->
  RevDiff a b b
differentiableAtan2 (MkRevDiff (y, x) bpc) =
  MkRevDiff
    (atan2 y x)
    (\cy -> bpc (x / r2 * cy, negate $ y / r2 * cy))
  where
    r2 = x ^ 2 + y ^ 2

instance
  ( AlgebraicPower Int a,
    MultiplicativeAction Int a,
    Multiplicative a
  ) =>
  AlgebraicPower Int (RevDiff c a a)
  where
  x ^^ n = f x -- differentiablePow .: twoArgsToTuple
    where
      f =
        simpleDifferentiableFunc
          (^^ n)
          (\x' -> n *| (x' ^^ (n - 1)))

-- (fromIntegral n * integralPow (n - one))

instance
  ( AlgebraicPower Integer a,
    MultiplicativeAction Integer a,
    Multiplicative a
  ) =>
  AlgebraicPower Integer (RevDiff c a a)
  where
  x ^^ n = f x
    where
      f =
        simpleDifferentiableFunc
          (^^ n)
          (\x' -> n *| (x' ^^ (n - 1)))

-- | Differentiable version of sine function for the `RevDiff` type.
--
-- Implements
-- \[
-- d\frac{d}{dx} \sin f(x) = \cos f(x) * \frac{df(x)}{dx}
-- \]
-- using the standard trigonometric derivative.
--
-- ==== __Examples__
--
-- >>> call differentiableSin 0.0 :: Float
-- 0.0
-- >>> simpleDerivative differentiableSin 0.0 :: Float
-- 1.0
differentiableSin ::
  (TrigField b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableSin = simpleDifferentiableFunc sin cos

-- | Differentiable version of cosine function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \cos f(x) = -\sin f(x) \cdot \frac{df(x)}{dx}
-- \]
-- using the standard trigonometric derivative.
--
-- ==== __Examples__
--
-- >>> call differentiableCos 0.0 :: Float
-- 1.0
-- >>> simpleDerivative differentiableCos 0.0 :: Float
-- -0.0
differentiableCos ::
  (TrigField b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableCos = simpleDifferentiableFunc cos (negate . sin)

-- | Differentiable version of tangent function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d]{dx} \tg f(x) =
--  \sec^2 f(x) * \frac{df(x)}{dx} = \frac{1}{cos^2 f(x)} \cdot \frac{df(x)}{dx}.
-- \]
--
-- ==== __Examples__
--
-- >>> call differentiableTan 0.0 :: Float
-- 0.0
-- >>> simpleDerivative differentiableTan 0.0 :: Float
-- 1.0
differentiableTan ::
  (TrigField b, IntegerPower b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableTan = simpleDifferentiableFunc tan ((^ (-2)) . cos)

-- | Differentiable version of arcsine function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \arcsin f(x) = \frac{1}{\sqrt{1-f^2(x)}} \cdot \frac{df(x)}{dx}.
-- \]
--
-- ==== __Examples__
--
-- >>> call differentiableAsin 0.0 :: Float
-- 0.0
-- >>> simpleDerivative differentiableAsin 0.0 :: Float
-- 1.0
differentiableAsin ::
  (TrigField b, ExpField b, IntegerPower b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableAsin = simpleDifferentiableFunc asin (recip . sqrt . (one -) . (^ 2))

-- | Differentiable version of arccosine function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \arccos f(x) = -\frac{1}{\sqrt{1-f^2(x)}} \cdot \frac{df(x)}{dx}.
-- \]
--
-- ==== __Examples__
--
-- >>> call differentiableAcos 0.0 :: Float
-- 1.5707964
-- >>> simpleDerivative differentiableAcos 0.0 :: Float
-- -1.0
differentiableAcos ::
  (TrigField b, ExpField b, IntegerPower b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableAcos =
  simpleDifferentiableFunc acos (negate . recip . sqrt . (one -) . (^ 2))

-- | Differentiable version of arctangent function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \mathrm{arctg} f(x) = \frac{1}{1 + f^2(x)} \cdot \frac{df(x)}{dx}.
-- \]
--
-- ==== __Examples__
--
-- >>> call differentiableAtan 0.0 :: Float
-- 0.0
-- >>> simpleDerivative differentiableAtan 0.0 :: Float
-- 1.0
differentiableAtan ::
  (TrigField b, IntegerPower b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableAtan = simpleDifferentiableFunc atan (recip . (one +) . (^ 2))

-- | Differentiable version of hyperbolic sine function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \sinh f(x) = \cosh f(x) \cdot \frac{df(x)}{dx}.
-- \]
--
-- ==== __Examples__
--
-- >>> call differentiableSinh 0.0 :: Float
-- 0.0
-- >>> simpleDerivative differentiableSinh 0.0 :: Float
-- 1.0
differentiableSinh ::
  (TrigField b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableSinh = simpleDifferentiableFunc sinh cosh

-- | Differentiable version of hyperbolic cosine function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \mathrm{csch} f(x) = \mathrm{sh} f(x) \cdot \frac{df(x)}{dx}.
-- \]
--
-- ==== __Examples__
--
-- >>> call differentiableCosh 0.0 :: Float
-- 1.0
-- >>> simpleDerivative differentiableCosh 0.0 :: Float
-- 0.0
differentiableCosh ::
  (TrigField b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableCosh = simpleDifferentiableFunc cosh sinh

-- | Differentiable version of hyperbolic tangent function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \mathrm{th} f(x) =
--  \mathrm{sech}^2 f(x) \cdot \frac{df}{dx} = \frac{1}{\mathrm{ch}^2 f(x)} \cdot \frac{df}{dx}.
-- \]
--
-- ==== __Examples__
--
-- >>> call differentiableTanh 0.0 :: Float
-- 0.0
-- >>> simpleDerivative differentiableTanh 0.0 :: Float
-- 1.0
differentiableTanh ::
  (TrigField b, IntegerPower b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableTanh = simpleDifferentiableFunc tanh ((^ (-2)) . cosh)

-- | Differentiable version of inverse hyperbolic sine function for the `RevDiff` type.
--
-- Implements
-- \[
--  \DeclareMathOperator{\arcsh}{arcsh}
--  \frac{d}{dx} \arcsh f(x) = \frac{1}{\sqrt{1 + f^2 (x)}} \cdot \frac{df}{dx}.
-- \]
--
-- ==== __Examples__
--
-- >>> call differentiableAsinh 0.0 :: Float
-- 0.0
-- >>> simpleDerivative differentiableAsinh 0.0 :: Float
-- 1.0
differentiableAsinh ::
  (TrigField b, ExpField b, IntegerPower b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableAsinh = simpleDifferentiableFunc asinh (recip . sqrt . (one +) . (^ 2))

-- | Differentiable version of inverse hyperbolic cosine function for the `RevDiff` type.
--
-- Implements
-- \[
--  \DeclareMathOperator{\arcch}{arcch}
--  \frac{d}{dx} \arcch f(x) = \frac{1}{f^2(x) - 1} \cdot \frac{df}{dx}.
-- \]
differentiableAcosh ::
  (TrigField b, ExpField b, IntegerPower b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableAcosh = simpleDifferentiableFunc acosh (recip . sqrt . (one -) . (^ 2))

-- | Differentiable version of inverse hyperbolic tangent function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \arcth f(x) = \frac{1}{1 - f^2 (x)} \cdot \frac{df}{dx}.
--
-- ==== __Examples__
--
-- >>> call differentiableAtanh 0.0 :: Float
-- 0.0
-- >>> simpleDerivative differentiableAtanh 0.0 :: Float
-- 1.0
differentiableAtanh ::
  (TrigField b, IntegerPower b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableAtanh = simpleDifferentiableFunc atanh (recip . (one -) . (^ 2))

-- | `TrigField` instance for the `RevDiff` type.
instance
  (Additive a, Subtractive a, ExpField b, TrigField b, IntegerPower b) =>
  TrigField (RevDiff a b b)
  where
  -- Constants
  pi = constDiff pi

  -- Basic trig functions
  sin = differentiableSin
  cos = differentiableCos
  tan = differentiableTan

  -- Inverse trig functions
  asin = differentiableAsin
  acos = differentiableAcos
  atan = differentiableAtan
  atan2 = differentiableAtan2 .: twoArgsToTuple

  -- Hyperbolic functions
  sinh = differentiableSinh
  cosh = differentiableCosh
  tanh = differentiableTanh

  -- Inverse hyperbolic functions
  asinh = differentiableAsinh
  acosh = differentiableAcosh
  atanh = differentiableAtanh

-- | Differentiable version of absolute value function for the `RevDiff` type.
--
-- Implements
-- \[
--  \frac{d}{dx} \left_| f(x) \right_| = \sign(f) \cdot \frac{df}{dx},
-- \]
-- where \( \sign(f) \) is the signum function.
-- The derivative is undefined at zero but returns zero in this implementation.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
--
-- >>> simplify $ simpleDerivative differentiableAbs (variable "x") :: SE
-- sign(x)
--
-- >>> simpleDerivative differentiableAbs (10 :: Float) :: Float
-- 1.0
-- >>> simpleDerivative differentiableAbs (-10 :: Float) :: Float
-- -1.0
-- >>> simpleDerivative differentiableAbs (0 :: Float) :: Float
-- 0.0
differentiableAbs ::
  (GHCN.Num b, Multiplicative b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableAbs (MkRevDiff x bpc) =
  MkRevDiff (GHCN.abs x) (bpc . (GHCN.signum x *))

-- | Differentiable version of signum function for the `RevDiff` type.
--
-- The signum function has derivative zero everywhere except at zero (where it's undefined).
-- This implementation returns zero for all inputs, including zero.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
--
-- >>> simplify $ simpleDerivative differentiableSign (variable "x") :: SE
-- 0
--
-- >>> simpleDerivative differentiableSign (10 :: Float) :: Float
-- 0.0
-- >>> simpleDerivative differentiableSign (-10 :: Float) :: Float
-- 0.0
-- >>> simpleDerivative differentiableSign (0 :: Float) :: Float
-- 0.0
differentiableSign ::
  (Additive a, GHCN.Num b) =>
  RevDiff a b b ->
  RevDiff a b b
differentiableSign (MkRevDiff x _) = constDiff $ GHCN.signum x

-- | `RevDiff` instance for the `GHC.Num.Num` typeclass.
instance
  ( Additive a,
    Subtractive a,
    GHCN.Num b,
    Subtractive b,
    Multiplicative b
  ) =>
  GHCN.Num (RevDiff a b b)
  where
  (+) = (GHCN.+)
  (-) = (GHCN.-)
  (*) = (GHCN.*)
  negate = differentiableNegate
  abs = differentiableAbs
  signum = differentiableSign
  fromInteger = constDiff . GHCN.fromInteger

-- | `RevDiff` instance of the `NumHask.Data.Integral.FromInteger` typeclass.
instance
  (FromInteger c, Additive a) =>
  FromInteger (RevDiff a b c)
  where
  fromInteger = constDiff . fromInteger

-- | `RevDiff` and `Int8` instance of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Int8, Additive a) =>
  FromIntegral (RevDiff a b c) Int8
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Int16` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Int16, Additive a) =>
  FromIntegral (RevDiff a b c) Int16
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Int32` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Int32, Additive a) =>
  FromIntegral (RevDiff a b c) Int32
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Int64` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Int64, Additive a) =>
  FromIntegral (RevDiff a b c) Int64
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Int` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Int, Additive a) =>
  FromIntegral (RevDiff a b c) Int
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Word8` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Word8, Additive a) =>
  FromIntegral (RevDiff a b c) Word8
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Word16` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Word16, Additive a) =>
  FromIntegral (RevDiff a b c) Word16
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Word32` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Word32, Additive a) =>
  FromIntegral (RevDiff a b c) Word32
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Word64` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Word64, Additive a) =>
  FromIntegral (RevDiff a b c) Word64
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Word` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Word, Additive a) =>
  FromIntegral (RevDiff a b c) Word
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Integer` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Integer, Additive a) =>
  FromIntegral (RevDiff a b c) Integer
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` and `Natural` instance
-- of the `NumHask.Data.Integral.FromIntegral` typeclass.
instance
  (FromIntegral c Natural, Additive a) =>
  FromIntegral (RevDiff a b c) Natural
  where
  fromIntegral = constDiff . NumHask.fromIntegral

-- | `RevDiff` instance for the `GHC.Real.Fractional` typeclass.
instance
  ( Additive a,
    Subtractive a,
    Subtractive b,
    Divisive b,
    GHCR.Fractional b,
    IntegerPower b
  ) =>
  GHCR.Fractional (RevDiff a b b)
  where
  (/) = differentiableDiv .: twoArgsToTuple
  recip = differentiableRecip
  fromRational = constDiff . GHCR.fromRational

-- | Transforms two `RevDiff` instances into a 'RevDiff' instances with a tuple.
-- Inverese operation is 'tupleArg'.
twoArgsToTuple ::
  (Additive a) =>
  RevDiff a b0 c0 ->
  RevDiff a b1 c1 ->
  RevDiff a (b0, b1) (c0, c1)
twoArgsToTuple (MkRevDiff x0 bpc0) (MkRevDiff x1 bpc1) =
  MkRevDiff (x0, x1) (\(cy0, cy1) -> bpc0 cy0 + bpc1 cy1)

-- | Tuple argument descriptor for differentiable functions.
-- Transforms a `RevDiff` instances of a tuple into a tuple of `RevDiff` instances.
-- This allows applying differentiable operations to both elements of the tuple.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarySymbolicFunc)
--
-- >>> :{
--   f :: Multiplicative a => (a, a) -> a
--   f (x, y) = x * y
-- :}
--
-- >>> :{
--   f' :: (Distributive a, CT a ~ a) => (a, a) -> (a, a)
--   f' = customArgDerivative tupleArg f
-- :}
--
-- >>> simplify $ f' (variable "x", variable "y")
-- (y,x)
tupleArg ::
  (Additive b0, Additive b1) =>
  RevDiff a (b0, b1) (c0, c1) ->
  (RevDiff a b0 c0, RevDiff a b1 c1)
tupleArg (MkRevDiff (x0, x1) bpc) =
  ( MkRevDiff x0 (\cy -> bpc (cy, zero)),
    MkRevDiff x1 (\cy -> bpc (zero, cy))
  )

-- | Tuple argument descriptor builder.
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:sophisticated-45-argument-45-function-45-how-45-it-45-works)
-- for details and examples.
mkTupleArg ::
  (Additive b0, Additive b1) =>
  RevDiffArg a b0 c0 d0 ->
  RevDiffArg a b1 c1 d1 ->
  RevDiffArg a (b0, b1) (c0, c1) (d0, d1)
mkTupleArg f0 f1 = cross f0 f1 . tupleArg

-- | Tuple instance for `AutoDifferentiableArgument` typeclass.
-- It makes it possible to differntiate tuple argument funcitons.
instance
  ( AutoDifferentiableArgument a0,
    AutoDifferentiableArgument a1,
    DerivativeRoot a0 ~ DerivativeRoot a1
  ) =>
  AutoDifferentiableArgument (a0, a1)
  where
  type DerivativeRoot (a0, a1) = DerivativeRoot a0
  type DerivativeCoarg (a0, a1) = (DerivativeCoarg a0, DerivativeCoarg a1)
  type DerivativeArg (a0, a1) = (DerivativeArg a0, DerivativeArg a1)
  autoArg :: RevDiff (DerivativeRoot a0) (DerivativeCoarg a0, DerivativeCoarg a1) (DerivativeArg a0, DerivativeArg a1) -> (a0, a1)
  autoArg = mkTupleArg autoArg autoArg

-- | Tuple differentiable value builder
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples.
mkTupleVal :: (a0 -> b0) -> (a1 -> b1) -> (a0, a1) -> (b0, b1)
mkTupleVal = cross

-- | Tuple differentiable value descriptor.
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples.
tupleVal ::
  (Multiplicative b0, Multiplicative b1) =>
  (RevDiff a0 b0 c0, RevDiff a1 b1 c1) ->
  (a0, a1)
tupleVal = mkTupleVal scalarVal scalarVal

-- | Tuple instance for `AutoDifferentiableValue` typeclass.
-- It makes it possible to differntiate tuple value funcitons.
instance
  (AutoDifferentiableValue a0, AutoDifferentiableValue a1) =>
  AutoDifferentiableValue (a0, a1)
  where
  type DerivativeValue (a0, a1) = (DerivativeValue a0, DerivativeValue a1)
  autoVal :: (a0, a1) -> (DerivativeValue a0, DerivativeValue a1)
  autoVal = mkTupleVal autoVal autoVal

-- | Differentiable operator for functions with tuple argument
-- and any supported by `AutoDifferentiableValue` value type.
-- This function is equivalent to 'twoArgsDerivative' up to the curring.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarySymbolicFunc)
--
-- >>> :{
--   x = variable "x"
--   y = variable "y"
--   f :: SymbolicFunc a => a -> a
--   f = unarySymbolicFunc "f"
--   g :: SymbolicFunc a => a -> a
--   g = unarySymbolicFunc "g"
--   h :: (SymbolicFunc a, Multiplicative a) => (a, a) -> a
--   h (x, y) = f x * g y
-- :}
--
-- >>> f(x)*g(y)
-- f(x)*g(y)
--
-- >>> :{
--  h' :: (SE, SE) -> (SE, SE)
--  h' = simplify . tupleArgDerivative h
-- :}
--
-- >>> h' (x, y)
-- (f'(x)*g(y),g'(y)*f(x))
--
-- >>> :{
--  h'' :: (SE, SE) -> ((SE, SE), (SE, SE))
--  h'' = simplify . tupleArgDerivative (tupleArgDerivative h)
-- :}
--
-- >>> h'' (x, y)
-- ((f''(x)*g(y),g'(y)*f'(x)),(f'(x)*g'(y),g''(y)*f(x)))
tupleArgDerivative ::
  (Additive (CT a0), Additive (CT a1), AutoDifferentiableValue b) =>
  ((RevDiff' (a0, a1) a0, RevDiff' (a0, a1) a1) -> b) ->
  (a0, a1) ->
  DerivativeValue b
tupleArgDerivative = customArgDerivative tupleArg

-- | Differentiable operator for functions over tuple argument
-- with respect to the first argument.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarySymbolicFunc)
--
-- >>> :{
--   x = variable "x"
--   y = variable "y"
--   f :: SymbolicFunc a => a -> a
--   f = unarySymbolicFunc "f"
--   g :: SymbolicFunc a => a -> a
--   g = unarySymbolicFunc "g"
--   h :: (SymbolicFunc a, Multiplicative a) => (a, a) -> a
--   h (x, y) = f x * g y
-- :}
--
-- >>> f(x)*g(y)
-- f(x)*g(y)
--
-- >>> :{
--  h' :: (SE, SE) -> SE
--  h' = simplify . tupleDerivativeOverX h
-- :}
--
-- >>> h' (x, y)
-- f'(x)*g(y)
--
-- >>> :{
--  h'' :: (SE, SE) -> SE
--  h'' = simplify . tupleDerivativeOverX (tupleDerivativeOverX h)
-- :}
--
-- >>> h'' (x, y)
-- f''(x)*g(y)
tupleDerivativeOverX ::
  (AutoDifferentiableValue b, Additive (CT a0)) =>
  ((RevDiff' a0 a0, RevDiff' a0 a1) -> b) ->
  (a0, a1) ->
  DerivativeValue b
tupleDerivativeOverX f (x0, x1) =
  scalarArgDerivative (\x -> f (x, constDiff x1)) x0

-- | Differentiable operator for functions over tuple argument
-- with respect to the second argument.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarySymbolicFunc)
--
-- >>> :{
--   x = variable "x"
--   y = variable "y"
--   f :: SymbolicFunc a => a -> a
--   f = unarySymbolicFunc "f"
--   g :: SymbolicFunc a => a -> a
--   g = unarySymbolicFunc "g"
--   h :: (SymbolicFunc a, Multiplicative a) => (a, a) -> a
--   h (x, y) = f x * g y
-- :}
--
-- >>> f(x)*g(y)
-- f(x)*g(y)
--
-- >>> :{
--  h' :: (SE, SE) -> SE
--  h' = simplify . tupleDerivativeOverY h
-- :}
--
-- >>> h' (x, y)
-- g'(y)*f(x)
--
-- >>> :{
--  h'' :: (SE, SE) -> SE
--  h'' = simplify . tupleDerivativeOverY (tupleDerivativeOverY h)
-- :}
--
-- >>> h'' (x, y)
-- g''(y)*f(x)
tupleDerivativeOverY ::
  (Additive (CT a1), AutoDifferentiableValue b) =>
  ((RevDiff' a1 a0, RevDiff' a1 a1) -> b) ->
  (a0, a1) ->
  DerivativeValue b
tupleDerivativeOverY f (x0, x1) =
  scalarArgDerivative (\x -> f (constDiff x0, x)) x1

-- | Differentiable operator for functions over two arguments
-- and any supported by 'AutoDifferentiableValue' value type.
-- Equivalent to 'tupleArgDerivative' up to the curring.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarySymbolicFunc)
--
-- >>> :{
--   x = variable "x"
--   y = variable "y"
--   f :: SymbolicFunc a => a -> a
--   f = unarySymbolicFunc "f"
--   g :: SymbolicFunc a => a -> a
--   g = unarySymbolicFunc "g"
--   h :: (SymbolicFunc a, Multiplicative a) => a -> a -> a
--   h x y = f x * g y
-- :}
--
-- >>> f(x)*g(y)
-- f(x)*g(y)
--
-- >>> :{
--  h' :: SE -> SE -> (SE, SE)
--  h' = simplify . twoArgsDerivative h
-- :}
--
-- >>> h' x y
-- (f'(x)*g(y),g'(y)*f(x))
--
-- >>> :{
--  h'' :: SE -> SE -> ((SE, SE), (SE, SE))
--  h'' = simplify . twoArgsDerivative (twoArgsDerivative h)
-- :}
--
-- >>> h'' x y
-- ((f''(x)*g(y),g'(y)*f'(x)),(f'(x)*g'(y),g''(y)*f(x)))
twoArgsDerivative ::
  (Additive (CT a0), Additive (CT a1), AutoDifferentiableValue b) =>
  (RevDiff' (a0, a1) a0 -> RevDiff' (a0, a1) a1 -> b) ->
  a0 ->
  a1 ->
  DerivativeValue b
twoArgsDerivative f = curry (scalarArgDerivative $ uncurry f . tupleArg)

-- | Differentiable operator for functions over two arguments
-- with respect to the first argument.
-- Equivalent to `tupleDerivativeOverX` up to the curring.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarySymbolicFunc)
--
-- >>> :{
--   x = variable "x"
--   y = variable "y"
--   f :: SymbolicFunc a => a -> a
--   f = unarySymbolicFunc "f"
--   g :: SymbolicFunc a => a -> a
--   g = unarySymbolicFunc "g"
--   h :: (SymbolicFunc a, Multiplicative a) => a -> a -> a
--   h x y = f x * g y
-- :}
--
-- >>> f(x)*g(y)
-- f(x)*g(y)
--
-- >>> :{
--  h' :: SE -> SE -> SE
--  h' = simplify . twoArgsDerivativeOverX h
-- :}
--
-- >>> h' x y
-- f'(x)*g(y)
--
-- >>> :{
--  h'' :: SE -> SE -> SE
--  h'' = simplify . twoArgsDerivativeOverX (twoArgsDerivativeOverX h)
-- :}
--
-- >>> h'' x y
-- f''(x)*g(y)
twoArgsDerivativeOverX ::
  (Additive (CT a0), AutoDifferentiableValue b) =>
  (RevDiff' a0 a0 -> RevDiff' a0 a1 -> b) ->
  a0 ->
  a1 ->
  DerivativeValue b
twoArgsDerivativeOverX f x0 x1 =
  scalarArgDerivative (\x -> f x (constDiff x1)) x0

-- | Differentiable operator for functions over two arguments
-- with respect to the second argument.
-- Equivalent to `tupleDerivativeOverY` up to the curring.
twoArgsDerivativeOverY ::
  (Additive (CT a1), AutoDifferentiableValue b) =>
  (RevDiff' a1 a0 -> RevDiff' a1 a1 -> b) ->
  a0 ->
  a1 ->
  DerivativeValue b
twoArgsDerivativeOverY f = scalarArgDerivative . f . constDiff

-- | Differentiable operator for functions with tuple value and any supported by
-- `AutoDifferentiableArgument` argument type.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
--
-- >>> :{
--  f :: TrigField a => a -> (a, a)
--  f x = (sin x, cos x)
-- :}
--
-- >>> f (variable "x")
-- (sin(x),cos(x))
--
-- >>> :{
--  f' :: SE -> (SE, SE)
--  f' = simplify . tupleValDerivative f
-- :}
--
-- >>> f' (variable "x")
-- (cos(x),-(sin(x)))
tupleValDerivative ::
  ( AutoDifferentiableArgument a,
    Multiplicative c0,
    Multiplicative c1,
    DerivativeCoarg a ~ CT (DerivativeArg a),
    DerivativeRoot a ~ CT (DerivativeArg a)
  ) =>
  (a -> (RevDiff b0 c0 d0, RevDiff b1 c1 d1)) ->
  DerivativeArg a ->
  (b0, b1)
tupleValDerivative = customValDerivative tupleVal

-- boxedVectorValDerivative ::
--   ( AutoDifferentiableArgument a,
--     Multiplicative c,
--     DerivativeCoarg a ~ CT (DerivativeArg a),
--     DerivativeRoot a ~ CT (DerivativeArg a)
--   ) =>
--   (a -> BoxedVector n (RevDiff b c d)) ->
--   DerivativeArg a ->
--   BoxedVector n b
-- boxedVectorValDerivative = customValDerivative boxedVectorVal

-- Triple

-- | Differentiable operator for functions over triple arguments
-- with respect to the first argument.
tripleDerivativeOverX ::
  (AutoDifferentiableValue b, Additive (CT a0)) =>
  ((RevDiff' a0 a0, RevDiff' a0 a1, RevDiff' a0 a2) -> b) ->
  (a0, a1, a2) ->
  DerivativeValue b
tripleDerivativeOverX f (x0, x1, x2) =
  scalarArgDerivative
    (\x -> f (x, constDiff x1, constDiff x2))
    x0

-- | Differentiable operator for functions over triple arguments
-- with respect to the second argument.
tripleDerivativeOverY ::
  (AutoDifferentiableValue b, Additive (CT a1)) =>
  ((RevDiff' a1 a0, RevDiff' a1 a1, RevDiff' a1 a2) -> b) ->
  (a0, a1, a2) ->
  DerivativeValue b
tripleDerivativeOverY f (x0, x1, x2) =
  scalarArgDerivative
    (\x -> f (constDiff x0, x, constDiff x2))
    x1

-- | Differentiable operator for functions over triple arguments
-- with respect to the third argument.
tripleDerivativeOverZ ::
  (AutoDifferentiableValue b, Additive (CT a2)) =>
  ((RevDiff' a2 a0, RevDiff' a2 a1, RevDiff' a2 a2) -> b) ->
  (a0, a1, a2) ->
  DerivativeValue b
tripleDerivativeOverZ f (x0, x1, x2) =
  scalarArgDerivative
    (\x -> f (constDiff x0, constDiff x1, x))
    x2

-- | Transforms three `RevDiff` instances into a `RevDiff` instances of a triple.
-- The inverese operation is 'tripleArg'.
threeArgsToTriple ::
  (Additive a) =>
  RevDiff a b0 c0 ->
  RevDiff a b1 c1 ->
  RevDiff a b2 c2 ->
  RevDiff a (b0, b1, b2) (c0, c1, c2)
threeArgsToTriple (MkRevDiff x0 bpc0) (MkRevDiff x1 bpc1) (MkRevDiff x2 bpc2) =
  MkRevDiff (x0, x1, x2) (\(cy0, cy1, cy2) -> bpc0 cy0 + bpc1 cy1 + bpc2 cy2)

-- | Triple argument descriptor for differentiable functions.
-- Transforms a `RevDiff` instances of a triple into a triple of `RevDiff` instances.
-- This allows applying differentiable operations to each element of the triple.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarySymbolicFunc)
--
-- >>> :{
--   f :: Multiplicative a => (a, a, a) -> a
--   f (x, y, z) = x * y * z
-- :}
--
-- >>> :{
--   f' :: (Distributive a, CT a ~ a) => (a, a, a) -> (a, a, a)
--   f' = customArgDerivative tripleArg f
-- :}
--
-- >>> simplify $ f' (variable "x", variable "y", variable "z")
-- (y*z,x*z,x*y)
tripleArg ::
  (Additive b0, Additive b1, Additive b2) =>
  RevDiff a (b0, b1, b2) (c0, c1, c2) ->
  (RevDiff a b0 c0, RevDiff a b1 c1, RevDiff a b2 c2)
tripleArg (MkRevDiff (x0, x1, x2) bpc) =
  ( MkRevDiff x0 (\cx -> bpc (cx, zero, zero)),
    MkRevDiff x1 (\cy -> bpc (zero, cy, zero)),
    MkRevDiff x2 (\cz -> bpc (zero, zero, cz))
  )

-- | Triple argument builder.
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:sophisticated-45-argument-45-function-45-how-45-it-45-works)
-- for details and examples for the tuple.
mkTripleArg ::
  (Additive b0, Additive b1, Additive b2) =>
  RevDiffArg a b0 c0 d0 ->
  RevDiffArg a b1 c1 d1 ->
  RevDiffArg a b2 c2 d2 ->
  RevDiffArg a (b0, b1, b2) (c0, c1, c2) (d0, d1, d2)
mkTripleArg f0 f1 f2 = cross3 f0 f1 f2 . tripleArg

-- | Triple instance for `AutoDifferentiableArgument` typeclass.
-- It makes it possible to differntiate triple argument funcitons.
instance
  ( AutoDifferentiableArgument a0,
    AutoDifferentiableArgument a1,
    AutoDifferentiableArgument a2,
    DerivativeRoot a0 ~ DerivativeRoot a1,
    DerivativeRoot a0 ~ DerivativeRoot a2
  ) =>
  AutoDifferentiableArgument (a0, a1, a2)
  where
  type DerivativeRoot (a0, a1, a2) = DerivativeRoot a0
  type DerivativeCoarg (a0, a1, a2) = (DerivativeCoarg a0, DerivativeCoarg a1, DerivativeCoarg a2)
  type DerivativeArg (a0, a1, a2) = (DerivativeArg a0, DerivativeArg a1, DerivativeArg a2)
  autoArg :: RevDiff (DerivativeRoot a0) (DerivativeCoarg a0, DerivativeCoarg a1, DerivativeCoarg a2) (DerivativeArg a0, DerivativeArg a1, DerivativeArg a2) -> (a0, a1, a2)
  autoArg = mkTripleArg autoArg autoArg autoArg

-- | Triple differentiable value builder
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples for tuple.
mkTripleVal :: (a0 -> b0) -> (a1 -> b1) -> (a2 -> b2) -> (a0, a1, a2) -> (b0, b1, b2)
mkTripleVal = cross3

-- | Triple differentiable value descriptor.
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples.
tripleVal ::
  (Multiplicative b0, Multiplicative b1, Multiplicative b2) =>
  (RevDiff a0 b0 c0, RevDiff a1 b1 c1, RevDiff a2 b2 c2) ->
  (a0, a1, a2)
tripleVal = mkTripleVal scalarVal scalarVal scalarVal

-- | Triple instance for `AutoDifferentiableValue` typeclass.
instance
  ( AutoDifferentiableValue a0,
    AutoDifferentiableValue a1,
    AutoDifferentiableValue a2
  ) =>
  AutoDifferentiableValue (a0, a1, a2)
  where
  type DerivativeValue (a0, a1, a2) = (DerivativeValue a0, DerivativeValue a1, DerivativeValue a2)
  autoVal :: (a0, a1, a2) -> (DerivativeValue a0, DerivativeValue a1, DerivativeValue a2)
  autoVal = mkTripleVal autoVal autoVal autoVal

-- | Differentiable operator for functions with triple argument
-- and any supported by `AutoDifferentiableValue` value type.
-- The output is a triple of corresponding partial derivatives.
-- This function is equivalent to 'threeArgsDerivative' up to the curring.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.SimpleExpr.Utils.Algebra (AlgebraicPower, square, MultiplicativeAction)
-- >>> import Debug.DiffExpr (SymbolicFunc)
--
--
-- >>> :{
--   x = variable "x"
--   y = variable "y"
--   z = variable "z"
--   norm :: (AlgebraicPower Integer a, Additive a) => (a, a, a) -> a
--   norm (x, y, z) = square x + square y + square z
-- :}
--
-- >>> norm (x, y, z)
-- ((x^2)+(y^2))+(z^2)
--
-- >>> :{
--  norm' :: (SE, SE, SE) -> (SE, SE, SE)
--  norm' = simplify . tripleArgDerivative norm
-- :}
--
-- >>> simplify $ norm' (x, y, z)
-- (2*x,2*y,2*z)
--
-- >>> :{
--  norm'' :: (SE, SE, SE) -> ((SE, SE, SE), (SE, SE, SE), (SE, SE, SE))
--  norm'' = simplify . tripleArgDerivative (tripleArgDerivative norm)
-- :}
--
-- >>> norm'' (x, y, z)
-- ((2,0,0),(0,2,0),(0,0,2))
tripleArgDerivative ::
  ( Additive (CT a0),
    Additive (CT a1),
    Additive (CT a2),
    AutoDifferentiableValue b
  ) =>
  ( ( RevDiff' (a0, a1, a2) a0,
      RevDiff' (a0, a1, a2) a1,
      RevDiff' (a0, a1, a2) a2
    ) ->
    b
  ) ->
  (a0, a1, a2) ->
  DerivativeValue b
tripleArgDerivative = customArgDerivative tripleArg

-- | Differentiable operator for functions over three argument.
-- and any supported by `AutoDifferentiableValue` value type.
-- The output is a triple of corresponding partial derivatives.
-- This function is equivalent to 'tripleArgDerivative' up to the curring.
threeArgsDerivative ::
  ( AutoDifferentiableValue b,
    Additive (CT a0),
    Additive (CT a1),
    Additive (CT a2)
  ) =>
  ( RevDiff' (a0, a1, a2) a0 ->
    RevDiff' (a0, a1, a2) a1 ->
    RevDiff' (a0, a1, a2) a2 ->
    b
  ) ->
  a0 ->
  a1 ->
  a2 ->
  DerivativeValue b
threeArgsDerivative f = curry3 (scalarArgDerivative $ uncurry3 f . tripleArg)

-- | Differentiable operator for functions over three argument
-- with respect to the first argument.
-- and any supported by `AutoDifferentiableValue` value type.
derivative3ArgsOverX ::
  (AutoDifferentiableValue b, Additive (CT a0)) =>
  (RevDiff' a0 a0 -> RevDiff' a0 a1 -> RevDiff' a0 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DerivativeValue b
derivative3ArgsOverX f x0 x1 x2 =
  scalarArgDerivative
    (\x0' -> f x0' (constDiff x1) (constDiff x2))
    x0

-- | Differentiable operator for functions over three argument
-- with respect to the second argument.
-- and any supported by `AutoDifferentiableValue` value type.
derivative3ArgsOverY ::
  (AutoDifferentiableValue b, Additive (CT a1)) =>
  (RevDiff' a1 a0 -> RevDiff' a1 a1 -> RevDiff' a1 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DerivativeValue b
derivative3ArgsOverY f x0 x1 x2 =
  scalarArgDerivative
    (\x1' -> f (constDiff x0) x1' (constDiff x2))
    x1

-- | Differentiable operator for functions over three argument
-- with respect to the third argument.
-- and any supported by `AutoDifferentiableValue` value type.
derivative3ArgsOverZ ::
  (AutoDifferentiableValue b, Additive (CT a2)) =>
  (RevDiff' a2 a0 -> RevDiff' a2 a1 -> RevDiff' a2 a2 -> b) ->
  a0 ->
  a1 ->
  a2 ->
  DerivativeValue b
derivative3ArgsOverZ f x0 x1 =
  scalarArgDerivative $ f (constDiff x0) (constDiff x1)

-- | Differentiable operator for functions with tuple value and any supported by
-- `AutoDifferentiableArgument` argument type.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
--
-- >>> :{
--  f :: (Multiplicative a, IntegerPower a) => a -> (a, a, a)
--  f x = (one, x^1, x^2)
-- :}
--
-- >>> f (variable "x")
-- (1,x^1,x^2)
--
-- >>> :{
--  f' :: SE -> (SE, SE, SE)
--  f' = simplify . tripleValDerivative f
-- :}
--
-- >>> f' (variable "x")
-- (0,1,2*x)
tripleValDerivative ::
  ( AutoDifferentiableArgument a,
    Multiplicative c0,
    Multiplicative c1,
    Multiplicative c2,
    DerivativeCoarg a ~ CT (DerivativeArg a),
    DerivativeRoot a ~ CT (DerivativeArg a)
  ) =>
  (a -> (RevDiff b0 c0 d0, RevDiff b1 c1 d1, RevDiff b2 c2 d2)) ->
  DerivativeArg a ->
  (b0, b1, b2)
tripleValDerivative = customValDerivative tripleVal

-- BoxedVector

-- | `BoxedVector` differentiable value builder
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples.
mkBoxedVectorVal :: (a -> b) -> BoxedVector n a -> BoxedVector n b
mkBoxedVectorVal = fmap

-- | `BoxedVector` instance for `AutoDifferentiableValue` typeclass.
instance
  (AutoDifferentiableValue a) =>
  AutoDifferentiableValue (BoxedVector n a)
  where
  type DerivativeValue (BoxedVector n a) = BoxedVector n (DerivativeValue a)
  autoVal :: BoxedVector n a -> BoxedVector n (DerivativeValue a)
  autoVal = mkBoxedVectorVal autoVal

-- | Boxed array differentiable value descriptor.
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import Debug.DiffExpr (unarySymbolicFunc, SymbolicFunc)
--
-- >>> :{
--   v :: SymbolicFunc a => a -> BoxedVector 3 a
--   v t = DVGS.fromTuple (
--      unarySymbolicFunc "v_x" t,
--      unarySymbolicFunc "v_y" t,
--      unarySymbolicFunc "v_z" t
--    )
-- :}
--
-- >>> t = variable "t"
-- >>> v t
-- Vector [v_x(t),v_y(t),v_z(t)]
--
-- >>> v' = simplify . customValDerivative boxedVectorVal v :: SE -> BoxedVector 3 SE
-- >>> v' t
-- Vector [v_x'(t),v_y'(t),v_z'(t)]
boxedVectorVal ::
  (Multiplicative b) =>
  BoxedVector n (RevDiff a b c) ->
  BoxedVector n a
boxedVectorVal = mkBoxedVectorVal scalarVal

-- | Differentiable operator for functions with `BoxedVector` argument
-- and any supported by `AutoDifferentiableValue` value type.
-- The output is a `BoxedVector` instamce of corresponding drivatives.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import Debug.DiffExpr (unarySymbolicFunc, SymbolicFunc)
--
-- >>> :{
--   v :: SymbolicFunc a => a -> BoxedVector 3 a
--   v t = DVGS.fromTuple (
--      unarySymbolicFunc "v_x" t,
--      unarySymbolicFunc "v_y" t,
--      unarySymbolicFunc "v_z" t
--    )
-- :}
--
-- >>> t = variable "t"
-- >>> v t
-- Vector [v_x(t),v_y(t),v_z(t)]
--
-- >>> v' = simplify . boxedVectorValDerivative v :: SE -> BoxedVector 3 SE
-- >>> v' t
-- Vector [v_x'(t),v_y'(t),v_z'(t)]
boxedVectorValDerivative ::
  ( AutoDifferentiableArgument a,
    Multiplicative c,
    DerivativeCoarg a ~ CT (DerivativeArg a),
    DerivativeRoot a ~ CT (DerivativeArg a)
  ) =>
  (a -> BoxedVector n (RevDiff b c d)) ->
  DerivativeArg a ->
  BoxedVector n b
boxedVectorValDerivative = customValDerivative boxedVectorVal

-- | Boxed vector argument descriptor for differentiable functions.
-- Transforms a `RevDiff` instances of a boxed vector into a boxed vectror
-- of `RevDiff` instances.
-- This allows applying differentiable operations to each element of the boxed Vector.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarySymbolicFunc)
--
-- >>> :{
--   f :: Additive a => BoxedVector 3 a -> a
--   f = boxedVectorSum
-- :}
--
-- >>> :{
--   f' :: (Distributive a, CT a ~ a) => BoxedVector 3 a -> BoxedVector 3 a
--   f' = customArgDerivative boxedVectorArg f
-- :}
--
-- >>> simplify $ f' (DVGS.fromTuple (variable "x", variable "y", variable "z"))
-- Vector [1,1,1]
boxedVectorArg ::
  (Additive b, KnownNat n) =>
  RevDiff a (BoxedVector n b) (BoxedVector n c) ->
  BoxedVector n (RevDiff a b c)
boxedVectorArg (MkRevDiff array bpc) = DVGS.generate $ \k ->
  MkRevDiff (DVGS.index array k) (bpc . boxedVectorBasis k zero)

-- unpackBoxedVector ::
--   (Additive a, KnownNat n) =>
--   BoxedVector n (RevDiff a b c) ->
--   RevDiff a (BoxedVector n b) (BoxedVector n c)
-- unpackBoxedVector array =
--   MkRevDiff'
--     (fmap value array)
--     (boxedVectorSum . (fmap backprop array <*>))

-- | `BoxedVector` argument descriptor builder.
mkBoxedVectorArg ::
  (Additive b, KnownNat n) =>
  RevDiffArg a b c d ->
  RevDiffArg a (BoxedVector n b) (BoxedVector n c) (BoxedVector n d)
mkBoxedVectorArg f = fmap f . boxedVectorArg

-- | `BoxedVector` instance for `AutoDifferentiableArgument` typeclass.
instance
  ( AutoDifferentiableArgument a,
    KnownNat n
  ) =>
  AutoDifferentiableArgument (BoxedVector n a)
  where
  type DerivativeRoot (BoxedVector n a) = DerivativeRoot a
  type DerivativeCoarg (BoxedVector n a) = BoxedVector n (DerivativeCoarg a)
  type DerivativeArg (BoxedVector n a) = BoxedVector n (DerivativeArg a)
  autoArg :: RevDiff (DerivativeRoot a) (BoxedVector n (DerivativeCoarg a)) (BoxedVector n (DerivativeArg a)) -> BoxedVector n a
  autoArg = mkBoxedVectorArg autoArg

-- | Differentiable operator for functions with boxed array argument
-- and any supported by `AutoDifferentiableValue` value type.
-- The output is a boxed array of corresponding partial derivatives (i.e. gradient).
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.DiffExpr (SymbolicFunc)
-- >>> import Numeric.InfBackprop.Utils.SizedVector (BoxedVector, boxedVectorSum)
-- >>> import Debug.SimpleExpr.Utils.Algebra (AlgebraicPower, (^))
--
-- >>> :{
--   x = variable "x"
--   y = variable "y"
--   z = variable "z"
--   r = DVGS.fromTuple (x, y, z) :: BoxedVector 3 SE
--   norm2 :: (AlgebraicPower Integer a, Additive a) => BoxedVector 3 a -> a
--   norm2 v = boxedVectorSum (v^2)
-- :}
--
-- >>> simplify $ norm2 r
-- ((x^2)+(y^2))+(z^2)
--
-- >>> :{
--  norm2' :: BoxedVector 3 SE -> BoxedVector 3 SE
--  norm2' = simplify . boxedVectorArgDerivative norm2
-- :}
--
-- >>> norm2' r
-- Vector [2*x,2*y,2*z]
--
-- >>> :{
--  norm2'' :: BoxedVector 3 SE -> BoxedVector 3 (BoxedVector 3 SE)
--  norm2'' = simplify . boxedVectorArgDerivative (boxedVectorArgDerivative norm2)
-- :}
--
-- >>> norm2'' r
-- Vector [Vector [2,0,0],Vector [0,2,0],Vector [0,0,2]]
boxedVectorArgDerivative ::
  (KnownNat n, AutoDifferentiableValue b, Additive (CT a)) =>
  (BoxedVector n (RevDiff' (BoxedVector n a) a) -> b) ->
  BoxedVector n a ->
  DerivativeValue b
boxedVectorArgDerivative = customArgDerivative boxedVectorArg

-- instance (HasSum (BoxedVector n c) d, KnownNat n) =>
--   HasSum (RevDiff a (BoxedVector n b) (BoxedVector n c)) (RevDiff a b d) where
--     sum (MkRevDiff vec bp) = MkRevDiff' (sum vec) (bp . DVGS.replicate)

-- ** Stream

-- | Stream differentiable value builder
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples.
mkStreamVal :: (a -> b) -> Stream a -> Stream b
mkStreamVal = fmap

-- | Stream value structure for differentiable functions.
--
-- ==== __Examples__
--
-- >>> import GHC.Base ((<>))
-- >>> import Data.Stream (Stream, fromList, take)
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import Debug.DiffExpr (unarySymbolicFunc, SymbolicFunc)
--
-- >>> :{
--   s :: SymbolicFunc a => a -> Stream a
--   s t = fromList [unarySymbolicFunc ("s_" <> show n) t | n <- [0..]]
-- :}
--
-- >>> t = variable "t"
-- >>> take 5 (s t)
-- [s_0(t),s_1(t),s_2(t),s_3(t),s_4(t)]
--
-- >>> :{
--   s' :: SE -> Stream SE
--   s' = simplify . customValDerivative streamVal s
-- :}
--
-- >>> take 5 (s' t)
-- [s_0'(t),s_1'(t),s_2'(t),s_3'(t),s_4'(t)]
streamVal ::
  (Multiplicative b) =>
  Stream (RevDiff a b c) ->
  Stream a
streamVal = mkStreamVal scalarVal

-- | `Stream` instance for `AutoDifferentiableValue` typeclass.
instance
  (AutoDifferentiableValue a) =>
  AutoDifferentiableValue (Stream a)
  where
  type DerivativeValue (Stream a) = Stream (DerivativeValue a)
  autoVal :: Stream a -> Stream (DerivativeValue a)
  autoVal = mkStreamVal autoVal

-- | Derivative operator for a function from any supported argument type to a Stream.
streamValDerivative ::
  ( AutoDifferentiableArgument a,
    Multiplicative c,
    DerivativeCoarg a ~ CT (DerivativeArg a),
    DerivativeRoot a ~ CT (DerivativeArg a)
  ) =>
  (a -> Stream (RevDiff b c d)) ->
  DerivativeArg a ->
  Stream b
streamValDerivative = customValDerivative streamVal

-- | Stream argument descriptor for differentiable functions.
-- Transforms a `RevDiff` instances of a stream into a stream of `RevDiff` instances.
-- This allows applying differentiable operations to each element of the Stream.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import GHC.Base ((<>))
--
-- >>> :{
--   f :: Additive a => Stream a -> a
--   f = NumHask.sum . Data.Stream.take 4 :: Additive a => Data.Stream.Stream a -> a
-- :}
--
-- >>> :{
--   f' :: (Distributive a, CT a ~ a) => Stream a -> FiniteSupportStream a
--   f' = customArgDerivative streamArg f
-- :}
--
-- >>> s = Data.Stream.fromList [variable ("s_" <> show n) | n <- [0 :: Int ..]] :: Data.Stream.Stream SE
-- >>> simplify $ f' s
-- [1,1,1,1,0,0,0,...
streamArg ::
  (Additive b) =>
  RevDiff a (FiniteSupportStream b) (Stream c) ->
  Stream (RevDiff a b c)
streamArg (MkRevDiff x bpc) =
  DS.Cons
    (MkRevDiff x_head bpc_head)
    (streamArg (MkRevDiff x_tail bpc_tail))
  where
    x_head = DS.head x
    x_tail = DS.tail x
    bpc_head = bpc . singleton
    bpc_tail = bpc . cons zero

-- | Stream argument builder.
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:sophisticated-45-argument-45-function-45-how-45-it-45-works)
-- for details and examples for the tuple.
mkStreamArg ::
  (Additive b) =>
  (RevDiff a b c -> d) ->
  RevDiff a (FiniteSupportStream b) (Stream c) ->
  Stream d
mkStreamArg f = fmap f . streamArg

-- | `Stream` instance for `AutoDifferentiableArgument` typeclass.
instance
  (AutoDifferentiableArgument a) =>
  AutoDifferentiableArgument (Stream a)
  where
  type DerivativeRoot (Stream a) = DerivativeRoot a
  type DerivativeCoarg (Stream a) = FiniteSupportStream (DerivativeCoarg a)
  type DerivativeArg (Stream a) = Stream (DerivativeArg a)
  autoArg :: RevDiff (DerivativeRoot a) (FiniteSupportStream (DerivativeCoarg a)) (Stream (DerivativeArg a)) -> Stream a
  autoArg = mkStreamArg autoArg

-- | Differentiable operator for functions with `Stream` argument
-- and any supported by `AutoDifferentiableValue` value type.
-- The output is a boxed array of corresponding partial derivatives (i.e. gradient).
--
-- ==== __Examples__
--
-- >>> import GHC.Base ((<>))
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.DiffExpr (SymbolicFunc)
-- >>> import Data.Stream (Stream, fromList, take)
--
-- >>> s = fromList [variable ("s_" <> show n) | n <- [0 :: Int ..]] :: Stream SE
--
-- >>> take4Sum = NumHask.sum . take 4 :: Additive a => Stream a -> a
-- >>> simplify $ take4Sum s :: SE
-- s_0+(s_1+(s_2+s_3))
--
-- >>> :{
--  take4Sum' :: (Distributive a, CT a ~ a) =>
--    Stream a -> FiniteSupportStream (CT a)
--  take4Sum' = streamArgDerivative take4Sum
-- :}
--
-- >>> simplify $ take4Sum' s
-- [1,1,1,1,0,0,0,...
--
-- >>> :{
--  take4Sum'' :: (Distributive a, CT a ~ a) =>
--    Stream a -> FiniteSupportStream (FiniteSupportStream (CT a))
--  take4Sum'' = streamArgDerivative (streamArgDerivative take4Sum)
-- :}
--
-- >>> simplify $ take4Sum'' s
-- [[0,0,0,...,[0,0,0,...,[0,0,0,...,...
streamArgDerivative ::
  (AutoDifferentiableValue b, Additive (CT a)) =>
  (Stream (RevDiff' (Stream a) a) -> b) ->
  Stream a ->
  DerivativeValue b
streamArgDerivative = customArgDerivative streamArg

-- FiniteSupportStream

-- | Finite support stream differentiable value builder
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples.
-- It is expected that the argument function is linear or at least maps zero to zero.
mkFiniteSupportStreamVal :: (a -> b) -> FiniteSupportStream a -> FiniteSupportStream b
mkFiniteSupportStreamVal = unsafeMap

-- | Finite support stream value structure for differentiable functions.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import Debug.DiffExpr (unarySymbolicFunc, SymbolicFunc)
-- >>> import Data.FiniteSupportStream (unsafeFromList, FiniteSupportStream)
--
-- >>> :{
--  fss :: (Multiplicative a, IntegerPower a) =>
--    a -> FiniteSupportStream a
--  fss t = unsafeFromList [t^3, t^2, t, one]
-- :}
--
-- >>> t = variable "t"
-- >>> fss t
-- [t^3,t^2,t,1,0,0,0,...
--
-- >>> :{
--   fss' :: SE -> FiniteSupportStream SE
--   fss' = simplify . customValDerivative finiteSupportStreamVal fss
-- :}
--
-- >>> (fss' t)
-- [3*(t^2),2*t,1,0,0,0,...
finiteSupportStreamVal ::
  (Multiplicative b) =>
  FiniteSupportStream (RevDiff a b c) ->
  FiniteSupportStream a
finiteSupportStreamVal = mkFiniteSupportStreamVal scalarVal

-- | `FiniteSupportStream` instance for `AutoDifferentiableValue` typeclass.
instance
  (AutoDifferentiableValue a) =>
  AutoDifferentiableValue (FiniteSupportStream a)
  where
  type DerivativeValue (FiniteSupportStream a) = FiniteSupportStream (DerivativeValue a)
  autoVal :: FiniteSupportStream a -> FiniteSupportStream (DerivativeValue a)
  autoVal = mkFiniteSupportStreamVal autoVal

-- | Derivative operator for a function from any supported argument type to
-- a `FiniteSupportStream` instance.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import Debug.DiffExpr (unarySymbolicFunc, SymbolicFunc)
-- >>> import Data.FiniteSupportStream (unsafeFromList, FiniteSupportStream)
--
-- >>> :{
--  fss :: (Multiplicative a, IntegerPower a) =>
--    a -> FiniteSupportStream a
--  fss t = unsafeFromList [t^3, t^2, t, one]
-- :}
--
-- >>> t = variable "t"
-- >>> fss t
-- [t^3,t^2,t,1,0,0,0,...
--
-- >>> :{
--   fss' :: SE -> FiniteSupportStream SE
--   fss' = simplify . finiteSupportStreamValDerivative fss
-- :}
--
-- >>> fss' t
-- [3*(t^2),2*t,1,0,0,0,...
finiteSupportStreamValDerivative ::
  ( AutoDifferentiableArgument a,
    Multiplicative c,
    DerivativeCoarg a ~ CT (DerivativeArg a),
    DerivativeRoot a ~ CT (DerivativeArg a)
  ) =>
  (a -> FiniteSupportStream (RevDiff b c d)) ->
  DerivativeArg a ->
  FiniteSupportStream b
finiteSupportStreamValDerivative = customValDerivative finiteSupportStreamVal

-- | Finite support stream argument descriptor for differentiable functions.
-- Transforms a `RevDiff` instances of a finite support stream into
-- a finite support stream of `RevDiff` instances.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import Data.FiniteSupportStream (unsafeFromList, toVector)
--
-- >>> :{
--   f :: Additive a => FiniteSupportStream a -> a
--   f = NumHask.sum . toVector
-- :}
--
-- >>> f (unsafeFromList [1, 2, 3])
-- 6
--
-- >>> :{
--   f' :: (Distributive a, CT a ~ a) => FiniteSupportStream a -> Stream a
--   f' = customArgDerivative finiteSupportStreamArg f
-- :}
--
-- >>> Data.Stream.take 5 $ f' (unsafeFromList [1, 2, 3])
-- [1,1,1,0,0]
finiteSupportStreamArg ::
  (Additive b) =>
  RevDiff a (Stream b) (FiniteSupportStream c) ->
  FiniteSupportStream (RevDiff a b c)
finiteSupportStreamArg (MkRevDiff (MkFiniteSupportStream arrX) bpc) =
  MkFiniteSupportStream $ DV.imap f arrX
  where
    f i x = MkRevDiff x (bpc . cStream i)
    cStream i cy = go 0
      where
        go n = DS.Cons (if i == n then cy else zero) (go (n + 1))

-- cons
--   (MkRevDiff' x_head bpc_head)
--   (finiteSupportStreamArg (MkRevDiff' x_tail bpc_tail))
-- where
--   x_head = trace "taking head" $ head x
--   x_tail = trace "taking tail" $ tail x
--   bpc_head = trace "taking bpc_head" $ bpc . DS.fromList . (: [])
--   bpc_tail = trace "taking bpc_tail" $ bpc . DS.Cons zero

-- | Finite support stream argument descriptor builder.
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples.
-- It is expected that the argument function is linear or at least maps zero to zero.
mkFiniteSupportStreamArg ::
  (Additive b) =>
  (RevDiff a b c -> d) ->
  RevDiff a (Stream b) (FiniteSupportStream c) ->
  FiniteSupportStream d
mkFiniteSupportStreamArg f = unsafeMap f . finiteSupportStreamArg

-- | `FiniteSupportStream` instance for `AutoDifferentiableArgument` typeclass.
instance
  (AutoDifferentiableArgument a) =>
  AutoDifferentiableArgument (FiniteSupportStream a)
  where
  type DerivativeRoot (FiniteSupportStream a) = DerivativeRoot a
  type DerivativeCoarg (FiniteSupportStream a) = Stream (DerivativeCoarg a)
  type DerivativeArg (FiniteSupportStream a) = FiniteSupportStream (DerivativeArg a)
  autoArg :: RevDiff (DerivativeRoot a) (Stream (DerivativeCoarg a)) (FiniteSupportStream (DerivativeArg a)) -> FiniteSupportStream a
  autoArg = undefined

-- | Differentiable operator for functions that take a `FiniteSupportStream` argument
-- and return any value type supported by `AutoDifferentiableValue`.
-- The output is a stream of corresponding partial derivatives,
-- computing the gradient of the function with respect to each stream element.
-- See also
-- ["Tangent and Cotangent Spaces" tutorial section](Numeric-InfBackprop-Tutorial.html#g:how-45-it-45-works-45-tangent-45-space)
-- for the connection beetwen streams and finite support streams.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.DiffExpr (SymbolicFunc)
-- >>> import Data.Stream (Stream, take)
-- >>> import Data.FiniteSupportStream (FiniteSupportStream, unsafeFromList, toVector)
-- >>> import NumHask (sum)
--
-- Define a finite support stream with support length 4 containing 4 symbolic variables.
--
-- >>> s = unsafeFromList [variable "s_0", variable "s_1", variable "s_2", variable "s_3"] :: FiniteSupportStream SE
-- >>> s
-- [s_0,s_1,s_2,s_3,0,0,0,...
--
-- Now we'll define a function that sums all elements of a finite support stream.
--
-- >>> finiteSupportStreamSum = sum . toVector :: Additive a => FiniteSupportStream a -> a
-- >>> simplify $ finiteSupportStreamSum s :: SE
-- s_0+(s_1+(s_2+s_3))
--
-- We compute the gradient
-- of this function.
--
-- >>> :{
--  finiteSupportStreamSum' :: (Distributive a, CT a ~ a) =>
--    FiniteSupportStream a -> Stream (CT a)
--  finiteSupportStreamSum' = finiteSupportStreamArgDerivative finiteSupportStreamSum
-- :}
--
-- Let's compute the gradient at point @s@. It is an infinite stream and we take first 7 elements:
--
-- >>> take 7 $ simplify $ finiteSupportStreamSum' s
-- [1,1,1,1,0,0,0]
--
-- As expected,
-- the gradient is a stream with 1's in the first four positions (corresponding
-- to our four variables and the fixed support length 4) and 0's elsewhere:
--
-- We can compute the second derivative (Hessian matrix) that is stream of streams
-- in our case.
--
-- >>> :{
--  finiteSupportStreamSum'' :: (Distributive a, CT a ~ a) =>
--    FiniteSupportStream a -> Stream (Stream (CT a))
--  finiteSupportStreamSum'' = finiteSupportStreamArgDerivative (finiteSupportStreamArgDerivative finiteSupportStreamSum)
-- :}
--
-- All second derivatives should all be zero. We take first 7 rows and 4 columns of the inifinite Hessian matrix:
--
-- >>> take 7 $ fmap (take 4) $ simplify $ finiteSupportStreamSum'' s
-- [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
finiteSupportStreamArgDerivative ::
  (AutoDifferentiableValue b, Additive (CT a)) =>
  (FiniteSupportStream (RevDiff' (FiniteSupportStream a) a) -> b) ->
  FiniteSupportStream a ->
  DerivativeValue b
finiteSupportStreamArgDerivative = customArgDerivative finiteSupportStreamArg

-- | Maybe differentiable value builder.
-- Creates a mapping function for Maybe types.
-- See [this tutorial section]
-- (Numeric-InfBackprop-Tutorial.html#g:multivalued-45-function-45-how-45-it-45-works)
-- for details and examples.
mkMaybeVal :: (a -> b) -> Maybe a -> Maybe b
mkMaybeVal = fmap

-- | `Maybe` value structure for differentiable functions.
-- Extracts the derivative with respect to the original function for `Maybe` types.
--
-- ==== __Examples__
--
-- >>> :{
--  class SafeRecip a where
--    safeRecip :: a -> Maybe a
--  instance SafeRecip Float where
--    safeRecip x = if x == 0.0 then Nothing else Just (recip x)
--  instance (SafeRecip b, Subtractive b, Multiplicative b, IntegerPower b) =>
--    SafeRecip (RevDiff a b b) where
--      safeRecip (MkRevDiff v bp) =
--        fmap (\r -> MkRevDiff r (bp . negate . (r^2 *))) (safeRecip v)
-- :}
--
-- >>> safeRecip (2.0 :: Float) :: Maybe Float
-- Just 0.5
-- >>> safeRecip (0.0 :: Float) :: Maybe Float
-- Nothing
--
-- >>> customValDerivative maybeVal safeRecip (2.0 :: Float)
-- Just (-0.25)
-- >>> customValDerivative maybeVal safeRecip (0.0 :: Float)
-- Nothing
maybeVal ::
  (Multiplicative b) =>
  Maybe (RevDiff a b c) ->
  Maybe a
maybeVal = mkMaybeVal scalarVal

-- | `Maybe` instance of `AutoDifferentiableValue`.
instance
  (AutoDifferentiableValue a) =>
  AutoDifferentiableValue (Maybe a)
  where
  type DerivativeValue (Maybe a) = Maybe (DerivativeValue a)
  autoVal :: Maybe a -> Maybe (DerivativeValue a)
  autoVal = mkMaybeVal autoVal

-- | Argument descriptor for differentiable functions with optional argument.
-- Transforms a `RevDiff` instances of an otional type into
-- an optional of `RevDiff` instances.
-- This allows applying differentiable operations to the optiona value.

-- | Argument descriptor for differentiable functions with optional (`Maybe`) values.
--
-- Transforms a `RevDiff` instance containing an optional type into an optional
-- `RevDiff` instance. This transformation enables applying differentiable
-- operations to values that may or may not be present, while preserving
-- gradient flow when values exist.
--
-- When the wrapped value is `Just x`, the function extracts the value and
-- wraps it in a new `RevDiff` instance with appropriately transformed
-- backpropagation. When the value is `Nothing`, the result is `Nothing`,
-- effectively short-circuiting the computation.
--
-- ==== __Examples__
--
-- >>> :{
--  f :: Additive a => Maybe a -> a
--  f (Just x) = x
--  f Nothing = zero
-- :}
--
-- >>> customArgDerivative maybeArg f (Just 3 :: Maybe Float) :: Maybe Float
-- Just 1.0
maybeArg :: RevDiff a (Maybe b) (Maybe c) -> Maybe (RevDiff a b c)
maybeArg (MkRevDiff maybeX bpc) = case maybeX of
  Just x -> Just (MkRevDiff x (bpc . Just))
  Nothing -> Nothing

-- | Maybe argument builder.
-- Applies a function to `Maybe` value obtained from a `RevDiff`.
mkMaybeArg ::
  (RevDiff a b c -> d) -> RevDiff a (Maybe b) (Maybe c) -> Maybe d
mkMaybeArg f = fmap f . maybeArg

-- | `Maybe` instance of `AutoDifferentiableArgument`.
instance
  (AutoDifferentiableArgument a) =>
  AutoDifferentiableArgument (Maybe a)
  where
  type DerivativeRoot (Maybe a) = DerivativeRoot a
  type DerivativeCoarg (Maybe a) = Maybe (DerivativeCoarg a)
  type DerivativeArg (Maybe a) = Maybe (DerivativeArg a)
  autoArg :: RevDiff (DerivativeRoot a) (Maybe (DerivativeCoarg a)) (Maybe (DerivativeArg a)) -> Maybe a
  autoArg = mkMaybeArg autoArg

-- | Differentiable operator for functions that take a `Maybe` (a value or none) argument
-- and return any value type supported by `AutoDifferentiableValue`.
-- The output is `Maybe` of corresponding derivatives over the inner type.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify, SE)
-- >>> import Debug.DiffExpr (SymbolicFunc)
-- >>> import qualified GHC.Num as GHCN
--
-- >>> :{
--  maybeF :: TrigField a => Maybe a -> a
--  maybeF (Just x) = sin x
--  maybeF Nothing = zero
-- :}
--
-- >>> maybeF (Just 0.0 :: Maybe Float)
-- 0.0
--
-- >>> maybeF (Nothing :: Maybe Float)
-- 0.0
--
-- >>> maybeArgDerivative maybeF (Just 0.0 :: Maybe Float)
-- Just 1.0
--
-- >>> maybeArgDerivative maybeF (Nothing :: Maybe Float)
-- Just 0.0
maybeArgDerivative ::
  (AutoDifferentiableValue b) =>
  (Maybe (RevDiff' (Maybe a) a) -> b) ->
  Maybe a ->
  DerivativeValue b
maybeArgDerivative = customArgDerivative maybeArg

-- | Derivative operator for functions with Maybe arguments.
-- This allows computing derivatives of functions that returns Maybe values as output,
-- handling the case when the value is Nothing appropriately.
--
-- ==== __Examples__
--
-- >>> :{
--  class SafeRecip a where
--    safeRecip :: a -> Maybe a
--  instance SafeRecip Float where
--    safeRecip x = if x == 0.0 then Nothing else Just (recip x)
--  instance (SafeRecip b, Subtractive b, Multiplicative b, IntegerPower b) =>
--    SafeRecip (RevDiff a b b) where
--      safeRecip (MkRevDiff v bp) =
--        fmap (\r -> MkRevDiff r (bp . negate . (r^2 *))) (safeRecip v)
-- :}
--
-- >>> safeRecip (2.0 :: Float) :: Maybe Float
-- Just 0.5
-- >>> safeRecip (0.0 :: Float) :: Maybe Float
-- Nothing
--
-- >>> maybeValDerivative safeRecip (2.0 :: Float)
-- Just (-0.25)
-- >>> maybeValDerivative safeRecip (0.0 :: Float)
-- Nothing
maybeValDerivative ::
  ( AutoDifferentiableArgument a,
    Multiplicative c,
    DerivativeCoarg a ~ CT (DerivativeArg a),
    DerivativeRoot a ~ CT (DerivativeArg a)
  ) =>
  (a -> Maybe (RevDiff b c d)) ->
  DerivativeArg a ->
  Maybe b
maybeValDerivative = customValDerivative maybeVal
