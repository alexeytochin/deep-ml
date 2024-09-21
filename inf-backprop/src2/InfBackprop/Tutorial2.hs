{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module    :  InfBackprop.Tutorial
-- Copyright   :  (C) 2024 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Tutorial [inf-backprop](https://hackage.haskell.org/package/inf-backprop) package.
module InfBackprop.Tutorial2
  ( -- * Quick start
    -- $quick_start_simple_derivative

    -- * Derivatives for symbolic expressions
    -- $derivatives_for_symbolic_expressions

    -- * Symbolic expressions visualization
    -- $symbolic_expressions_visualization

    -- * Multivalued function
    -- $multivalued_function

    -- * Gradinet
    -- $gradient

    -- * How it works
    -- ** Backpropagation
    -- $backpropagation
    -- ** Lens
    -- $lens
    -- Differentiable types
    -- $differntibale_types
    -- ** Declaring custom derivatives
    -- $$custom_derivatives
  )
where

import Control.Arrow (Kleisli, (<<<), (>>>))
import Control.Monad.Logger (MonadLogger)
import Debug.SimpleExpr (SimpleExpr, simplify, variable)
import Prelude (Maybe (Just, Nothing), Monad, (.), uncurry, curry, undefined, Float, ($), fmap)

import Debug.DiffExpr (binarySymbolycFunc, unarrySymbolycFunc, BinnarySymbolicFunc, SymbolicFunc)
import InfBackprop.Common6 (gradientTuple_, derivativeX, derivativeY, gradientTuple,
  twoArgGradientTuple, GradientTupleType)
import Temp (Mapper, smartMap)
import Numeric.InfBackprop (derivative, customDerivative, intPow, tupleLensIso, idIso, smallVecLensIso, LensD(LensD))
import qualified Optics as O
import InfBackprop.Tangent (T)
import Data.Basis3 (Basis, End, initBackProp, zeroBackProp)
import InfBackprop.LensD (DFunc)
import NumHask (Additive, Multiplicative, (*), ExpField, Divisive, sqrt, (^), (+), (/), zero, Distributive, Subtractive,
 (-), sin, cos, (*), (|*), (*|), (|/))
import Control.Composition ((.*))
import Optics ((%), Iso')
import Optics.InfBackpropExtra (crossed, fmaped)
import qualified Data.Vector.Fixed.Boxed as DVFB
import Data.Vector.Fixed.Generic (mapG)
import qualified Data.Vector.Fixed as DVF
import qualified Data.Vector.Fixed.Cont as DVFC
import qualified Data.Vector.Fixed.Generic as DVFG
import Data.Vector.Fixed.Cont (Peano)
import InfBackprop.Tangent (T, Tangent)
import NumHask.SmallVectorOrphaneInstances ()

-- $quick_start_simple_derivative
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude (Float, fmap, ($), Int)
-- >>> import NumHask ((*), (+), log, Multiplicative)
-- >>> import Numeric.InfBackprop (derivative, intPow)
--
-- Consider squre function
--
-- \[
--   f(x) := x^2
-- \]
--
-- as a simple example. We imported a polymorphic version of the multiplication
--
-- '(*)'@ :: @'Multiplicative'@ a => a -> a -> a@
--
-- from
-- [numhask](https://hackage.haskell.org/package/numhask)
-- package.
-- In particular, for @a :: Float@ we have
--
-- >>> f x = x * x
-- >>> fmap f [-3, -2, -1, 0, 1, 2, 3] :: [Float]
-- [9.0,4.0,1.0,0.0,1.0,4.0,9.0]
--
-- Than the first derivative
--
-- \[
--   f'(x) = 2 \cdot x
-- \]
--
-- can be found as follows
--
-- >>> f' = derivative f :: Float -> Float
-- >>> fmap f' [-3, -2, -1, 0, 1, 2, 3]
-- [-6.0,-4.0,-2.0,0.0,2.0,4.0,6.0]
-- 
-- It is essential to specify a type annotation like @Float -> Float@ for 'f''.
--
-- The second derivative 
-- 
-- \[
--   f''(x) = 2 
-- \]
-- 
-- is also straightforward 
--
-- >>> f'' = derivative $ derivative f :: Float -> Float
-- >>> fmap f'' [-3, -2, -1, 0, 1, 2, 3]
-- [2.0,2.0,2.0,2.0,2.0,2.0,2.0]
--
-- Of course it also works for function compositions
-- 
-- \[
--   g(x) := \log (x^2 + x^3)
-- \]
--
-- >>> g x = log (intPow (2 :: Int) x + intPow (3 :: Int) x)
-- >>> g' = derivative g :: Float -> Float
-- >>> g 1 :: Float
-- 0.6931472
-- >>> g' 1 :: Float
-- 2.5

-- $derivatives_for_symbolic_expressions
-- 
-- It would be more convinient illustrate how the differentiation works on symbolyc expressions 
-- but not floating point numbers.
-- 
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude (($))
-- >>> import NumHask ((*), sin, cos)
-- >>> import Numeric.InfBackprop (derivative, intPow)
-- >>> import Debug.SimpleExpr (variable, simplify)
--
-- We use
-- [simple-expr](https://hackage.haskell.org/package/simple-expr)
-- package here for symbolyc expression.
-- For example, a symbolic function
--
-- \[
--   f(x) := \sin x^2
-- \]
--
-- can be defined as follows
--
-- >>> x = variable "x"
-- >>> f = \x -> sin (intPow 2 x)
-- >>> f x
-- sin(x*x)
-- 
-- Calculating the symbolyc derivative 
-- 
-- \[
--   f'(x) := 2 x \cos x^2
-- \]
-- 
-- of @f@ is straightforward
-- 
-- >>> f' = derivative f
-- >>> simplify $ f' x 
-- (2*x)*cos(x*x)
--
-- Notice we still use universal definition of 'cos', '(*)' and ather operations from 
-- [numhask](https://hackage.haskell.org/package/numhask)
-- package.

-- $symbolic_expressions_visualization
--
-- The
-- [simple-expr](https://hackage.haskell.org/package/simple-expr)
-- package is equipped with a visulaisation tool that can be used to illustrate the  differentiation workflow.
--
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude (($), (.))
-- >>> import Debug.SimpleExpr (SimpleExpr, variable, simplify, plotExpr, plotDGraphPng)
-- >>> import Debug.SimpleExpr.GraphUtils (exprToGraph)
-- >>> import Data.Graph.VisualizeAlternative (plotDGraph)
-- >>> import Debug.DiffExpr (unarrySymbolycFunc)
-- >>> import Numeric.InfBackprop (derivative)
--
-- As a warm up consider a trivial composition of two functions
--
-- \[
--   g(f(x))
-- \]
--
-- that is defined by
--
-- >>> x = variable "x"
-- >>> f = unarrySymbolycFunc "f"
-- >>> g = unarrySymbolycFunc "g"
-- >>> (g . f) x
-- g(f(x))
--
-- This simple cae can be plotted by
--
-- @ plotExpr $ (g . f) x @
--
-- ![image description](doc/images/composition.png)
--
-- The graph for the first derivative can depicted by
--
-- @ plotExpr $ simplify $ derivative (g . f) x @
--
-- ![image description](doc/images/composition_derivative.png)
--
-- where
-- 'simplify'@ :: @'SimpleExpr'@ -> @'SimpleExpr`
-- is a simple removal unnessasary differentiation process artifacts like @*1@ and @+0@.
--
-- As well as the second derivative is straightforward
--
-- @ plotExpr $ simplify $ derivative $ derivative (g . f) x @
--
-- ![image description](doc/images/composition_second_derivative.png)

-- $multivalued_function
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude (Float, fmap, ($), Int, (.), (<>), show, fst, snd)
-- >>> import NumHask ((*), (+), log, Multiplicative, cos, sin, TrigField, Additive)
-- >>> import Numeric.InfBackprop (derivative, customDerivative, intPow, tupleLensIso, idIso, smallVecLensIso, LensD(LensD))
-- >>> import Debug.SimpleExpr (variable, SimpleExpr)
-- >>> import Prelude.Tools (cross)
-- >>> import Temp (smartMap)
-- >>> import Data.Stream (Stream, take, fromList)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarrySymbolycFunc, BinnarySymbolicFunc, binarySymbolycFunc) 
-- >>> import qualified Data.Vector.Fixed as DVF
-- >>> import qualified Data.Vector.Fixed.Boxed as DVFB
-- >>> import Optics ((%), Iso')
--
-- >>> import InfBackprop.Vector (smallVecToLens, longVecToLens)
-- >>> import InfBackprop.Stream (streamLensIso)
-- >>> import Data.FiniteList (BoundedStream)
-- >>> import qualified Data.Vector.Generic.Sized as DVGS
-- >>> import qualified Data.Vector as DV
-- >>> import Optics.InfBackpropExtra (crossed, fmaped)
--
-- >>> x = variable "x"
-- >>> :{
--    f :: TrigField a => a -> (a, a)
--    f x' = (sin x', cos x')
-- :}
--
-- >>> :{
--  smartSimplify :: Mapper SimpleExpr SimpleExpr a a => a -> a
--  smartSimplify = smartMap simplify
-- :}
--
-- >>> tupleDerivative = customDerivative tupleLensIso idIso
-- >>> f' = (smartSimplify) . tupleDerivative f :: SimpleExpr -> (SimpleExpr, SimpleExpr)
-- >>> f' x
-- (cos(x),-(sin(x)))
--
-- >>> f'' = smartSimplify . ((tupleDerivative . tupleDerivative) f) :: SimpleExpr -> (SimpleExpr, SimpleExpr)
-- >>> f'' x
-- (-(sin(x)),-(cos(x)))
-- 
-- >>> v = (\x' -> DVF.mk3 (unarrySymbolycFunc "v_x" x') (unarrySymbolycFunc "v_y" x') (unarrySymbolycFunc "v_z" x')) :: SymbolicFunc a => a -> DVFB.Vec 3 a
--
-- -- >>> v = (\x' -> DVF.mk3 (x') (x' + 1) (x' + 2) :: SymbolicFunc a => a -> DVFB.Vec 3 a
--
-- >>> v x
-- fromList [v_x(x),v_y(x),v_z(x)]
-- 
-- 
--
--
--
-- >>> f3''' = (\x -> DVGS.fromTuple (x * x, x * x * x)) :: (Multiplicative a) => a -> DVGS.Vector DV.Vector 2 a
-- >>> temp = derivative (longVecToLens . f3''') :: Float -> DVGS.Vector DV.Vector 2 Float
-- 
-- >>> temp 2
-- Vector [4.0,12.0]
--  
-- 
-- >>> f3'' = (\x -> DVF.mk2 (x * x) (x * x * x)) :: (Multiplicative a) => a -> DVFB.Vec 2 a
-- >>> temp = derivative (smallVecToLens . f3'') :: Float -> DVFB.Vec 2 Float
-- 
-- >>> temp 2
-- fromList [4.0,12.0]
--
-- >>> f3' = (\x -> DVF.mk2 (x * x) (x * x * x)) :: (SymbolicFunc a, Multiplicative a) => a -> DVFB.Vec 2 a
-- >>> temp = smartSimplify . derivative (smallVecToLens . f3') :: SimpleExpr -> DVFB.Vec 2 SimpleExpr
-- 
-- >>> temp x
-- fromList [x+x,((x*x)+(x*x))+(x*x)]
--
--`
--
-- >>> smallVecDerivative = customDerivative smallVecLensIso idIso
-- 
-- >>> v' = smartSimplify . smallVecDerivative v :: SimpleExpr -> DVFB.Vec 3 SimpleExpr
-- >>> v' x
-- fromList [v_x'(x),v_y'(x),v_z'(x)]
-- 
-- Streams:
-- >>> streamF = (\x -> fromList [unarrySymbolycFunc ("f_" <> show n) x | n <- [0..]]) :: SymbolicFunc a => a -> Stream a
-- >>> take 5 (streamF x)
-- [f_0(x),f_1(x),f_2(x),f_3(x),f_4(x)]
--
-- >>> streamDerivative = customDerivative streamLensIso idIso
-- >>> streamF' = smartSimplify . streamDerivative streamF :: SimpleExpr -> Stream SimpleExpr
-- >>> take 5 (streamF' x)
-- [f_0'(x),f_1'(x),f_2'(x),f_3'(x),f_4'(x)]
--
-- >>> g = \x -> (v x, streamF x)
-- 
-- >>> gDerivative = customDerivative (tupleLensIso % crossed smallVecLensIso streamLensIso) idIso
-- >>> g' = smartSimplify . gDerivative g :: SimpleExpr -> (DVFB.Vec 3 SimpleExpr, Stream SimpleExpr)
-- 
-- >>> fst $ g' x
-- fromList [v_x'(x),v_y'(x),v_z'(x)]
--
-- >>> take 5 $ snd $ g' x
-- [f_0'(x),f_1'(x),f_2'(x),f_3'(x),f_4'(x)]
--

-- $gradient
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude (Float, fmap, ($), Int, (.), (<>), show, fst, snd)
-- >>> import NumHask ((*), (+), log, Multiplicative, cos, sin, TrigField, Additive)
-- >>> import Numeric.InfBackprop (derivative, customDerivative, intPow, tupleLensIso, idIso, smallVecLensIso, LensD(LensD))
-- >>> import Debug.SimpleExpr (variable, SimpleExpr)
-- >>> import Debug.DiffExpr (binarySymbolycFunc)
-- >>> import Prelude.Tools (cross)
-- >>> import Temp (smartMap)
-- >>> import Data.Stream (Stream, take, fromList)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarrySymbolycFunc, BinnarySymbolicFunc, binarySymbolycFunc)
-- >>> import qualified Data.Vector.Fixed as DVF
-- >>> import qualified Data.Vector.Fixed.Boxed as DVFB
-- >>> import Optics ((%), Iso')
--
--
-- >>> import InfBackprop.LensD (stopDiff)
-- >>> import InfBackprop.Common6 (derivativeX, derivativeY, gradientTuple)
-- 
-- >>> :{
--  smartSimplify :: Mapper SimpleExpr SimpleExpr a a => a -> a
--  smartSimplify = smartMap simplify
-- :}
--
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> h = binarySymbolycFunc "h"
-- >>> h x y
-- h(x,y)
--
-- >>> h' = smartSimplify . twoArgGradientTuple h :: SimpleExpr -> SimpleExpr -> (SimpleExpr, SimpleExpr)
-- >>> h' x y
-- (h'_1(x,y),h'_2(x,y))
--
-- >>> :{
--  f :: SymbolicFunc a => a -> a
--  f = unarrySymbolycFunc "f"
--  g :: SymbolicFunc a => a -> a
--  g = unarrySymbolycFunc "g"
--  fg :: (SymbolicFunc a, Multiplicative a) => (a, a) -> a
--  fg (x, y) = f x * g y
-- :}
--
-- >>> type E = SimpleExpr
--
-- >>> fg (x, y) :: E
-- f(x)*g(y)
--
-- 'gradientTuple'@ :: (Basis (T b), Additive (T a0), Additive (T a1)) => ((DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> DFunc (a0, a1) b)-> (a0, a1) -> End (T b) (T a0, T a1)@
--
-- >>> :{
--  fg' :: (E, E) -> (E, E)
--  fg' = smartSimplify . gradientTuple fg
-- :}
-- 
-- >>> fg' (x, y)
-- (f'(x)*g(y),g'(y)*f(x))
--
-- >>> :{
--  tupleGradientTuple :: (Basis (T b0), Basis (T b1), Additive (T a0), Additive (T a1), Additive (T b0), Additive (T b1)) =>
--    ((DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> (DFunc (a0, a1) b0, DFunc (a0, a1) b1)) ->
--    (a0, a1) ->
--    (End (T b0) (T a0, T a1), End (T b1) (T a0, T a1))
--  tupleGradientTuple = customDerivative tupleLensIso tupleLensIso
-- :}
--
-- >>> :{
--  fg'' :: (E, E) -> ((E, E), (E, E))
--  fg'' = smartSimplify $ tupleGradientTuple $ gradientTuple fg
-- :}
--
-- >>> fg'' (x, y)
-- ((f''(x)*g(y),g'(y)*f'(x)),(f'(x)*g'(y),g''(y)*f(x)))
--
-- >>> :{
--  tupleGradientOfDoubleTuple :: (
--        Additive (T a0), Additive (T a1),
--        Additive (T b00), Additive (T b01), Additive (T b10), Additive (T b11),
--        Basis (T b00), Basis (T b01), Basis (T b10), Basis (T b11)
--      ) =>
--    ((DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> ((DFunc (a0, a1) b00, DFunc (a0, a1) b01), (DFunc (a0, a1) b10, DFunc (a0, a1) b11)))
--    -> (a0, a1)
--    -> ((End (T b00) (T a0, T a1), End (T b01) (T a0, T a1)), (End (T b10) (T a0, T a1), End (T b11) (T a0, T a1)))
--  tupleGradientOfDoubleTuple = customDerivative (tupleLensIso % crossed tupleLensIso tupleLensIso) tupleLensIso
-- :}
--
-- >>> :{
--  fg''' :: (E, E) -> (((E, E), (E, E)), ((E, E), (E, E)))
--  fg''' = smartSimplify $ tupleGradientOfDoubleTuple $ tupleGradientTuple $ gradientTuple fg
-- :}
--
-- >>> fg''' (x, y)
-- (((f'''(x)*g(y),g'(y)*f''(x)),(f''(x)*g'(y),g''(y)*f'(x))),((f''(x)*g'(y),g''(y)*f'(x)),(f'(x)*g''(y),g'''(y)*f(x))))
--
--
-- >>> smartSimplify $ derivative (f * g) x :: E
-- (f'(x)*g(x))+(g'(x)*f(x))
--
--
--
-- \[
--    \left.
--      \frac{d}{dx}
--      \left(
--        x
--          \left(
--            \left.
--              \frac{d}{dy}
--              \left(
--                x + y
--              \right)
--            \right|_{y=1}
--          \right)
--      \right)
--    \right|_{x=1}
--    = 1
-- \]
--
--
-- >>> derivativeY (+) x y
-- 0+1
--
-- >>> fSiskindPearlmutter = derivative (\x -> x * derivativeY (+) x (stopDiff (1 :: Float))) (1 :: Float)
-- >>> fSiskindPearlmutter
-- 1.0
--
--

-- $backpropagation
-- The idea would be clear from the example of three
-- @\mathbb{R} \rightarrow \mathbb{R}@
-- functions composition
--
-- \[
--   g(f(h(x)))
-- \]
-- with a focus on function @f@.
--
-- Its first derivative over @x@ is
--
-- \[
--   g(f(h(x))).
-- \]
--
-- \[
--   h'(x) \cdot f'(h(x)) \cdot g'(f(h(x))).
-- \]
--
-- According to the backpropagation strategy, the order of the calculation should be as follows.
--
-- 1. Find @h(x)@.
--
-- 2. Find @f(h(x))@.
--
-- 3. Find @g(f(h(x)))@.
--
-- 4. Find the top derivative @g'(f(h(x)))@.
--
-- 5. Find the next to the top derivative @f'(h(x))@.
--
-- 6. Multiply @g'(f(h(x)))@ on @f'(h(x))@.
--
-- 7. Find the next derivative @h'(x)@.
--
-- 8. Multiply the output of point 6 on @h'(x)@.
--
-- ![image description](doc/images/backprop.drawio.png)
--
-- The upper path on the diagram from left to right hand sides is known as the forward step of the calculation.
-- The lower path from right to left hand sides is the backward step.
-- It could be reasonable to calculate the product of the derivatives in a diferent order,
-- for example, from left to right (forward propagation).
-- These are beyond the scope of the present library.
--
-- The generalizations for longer composition and vector valued function are straightforward.

-- $lens
-- >>> import InfBackprop.LensD (LensD, (%))
--
-- All calculations form the example abpove related to the function @f@ can be divided into two parts.
-- We have to find @f@ of @h(x)@ first (forward step) and then the derivative @f'@ of the same argument @h(x)@ and
-- multiply it on the derivative @g'(f(h(x)))@ obtained during the similar calculations for @g@ (backward step).
-- Notice that the value of @h(x)@ is reused on the backward step.
--
-- ![image description](doc/images/lens.drawio.png)
--
-- This motivates the follwoing type
--
-- 'LensD' ca a cb b @ :: LensD {unpackLensD ::a -> (b, ca -> cb)}@
--
-- The cathegorical composition low for 'LensD' instances
-- '(%)'@ :: LensD cy y cz z -> LensD cx x cy y -> LensD cx x cz z @
-- is clear from the image above.
-- The definition of the identity lens is straightfoeward
--
-- @
--  identity :: LensD ca a ca a
--  identity = LensD (, id)
-- @
--
-- We can regonize here the law breaking lens pattern.
--
-- Equivalent aproaches are also used in
-- [ad](https://https://hackage.haskell.org/package/ad)
-- and
-- [backprop](https://hackage.haskell.org/package/backprop)
-- packages
--
-- Types @ca@, @a@, @cb@, @b@ are just 'Float' for the case of @Float -> Float@ function.
-- However for more complex case $a$ and @b@ can be deffernet which corresponds to $a -> b$ differentiable function.
-- The types $ca$ an $cb$ are often identical to @a@ and $b$ correspondingly, for example, for 'Float' value vectors.
-- For non-trivail case see ???.
-- In general $a$ is considereds as a maifold and $ca$ is a cotangent bundle over it at the corresponding point.

-- $differntibale_types
-- import NumHask.Algebra.Field (TrigField, cos, sin, cosC, sinC)
--
-- Working with specially defined type 'LensD' and its composition may be practically inconvenient.
-- It is desired to have an oportunity to work with differentiable functions as with ordinary functions
-- that are simply instances of type $a -> b$ for some @a@ and @b@.
--
-- To this end we work with polimorphic versions of functions $a -> b$.
-- For example, the cosine function @cos@ is defined in
-- [numhask](https://hackage.haskell.org/package/numhask)
-- library as a polymorphic function
-- 'cos'@ :: TrigField a => a -> a @
-- for any type @a@ with 'TrigField' instance provided
-- that in particular requres the implenetations of sine and cosine function.
--
-- :{
--  class (...) => TrigField a where
--    cos :: a -> a
--    ...
-- :}
--
-- We implement an instance
-- @LensD Float Float Float Float@
-- for
-- 'TrigField'
-- as follows
--
-- @
-- cosC :: LensD Float Float Float Float
-- cosC = LensD $ \x -> (cos x, \cx -> - sin x * cx)
--
-- instance TrigField (LensD Float Float Float Float) where
--   cos x = cosC % x
-- @
--
-- where '(%)' is the composition of lenses.
--
-- We also define the derivative operator as follows
--
-- @
--  derivative :: (LensD Float Float Float Float -> LensD Float Float Float Float) -> Float -> Float
--  derivative f x = (snd ((unpackLensD (f identity)) x)) 1
-- @
--
-- Thereby,
--
-- @
--  cos (0 :: Float)
--  1 :: Float
-- @
--
-- but
--
-- @
--  (derivative cos) (0 :: Flaot)
--  0 :: Float
-- @
--
-- This however provides only a one-time differentiable version of the cosine function.
-- For the general case we have to define a different instance of 'TrigField' like
--
-- @ instance (TrigField a, ...) => TrigField (LensD t t a a) where ... @
--





-- $custom_derivatives
-- >>> import Prelude (Float)
--
-- As an illustrative example for a differentiable version of 'cos' numerical function can be defined as follows
--
--
--
--
--
--
--
--
--
--
--
-- @
--   cos :: BackpropFunc Float Float
--   cos = MkBackprop call' forward' backward' where
--     call' :: Float -> Float
--     call' = Prelude.cos
--
--     forward' :: BackpropFunc Float (Float, Float)
--     forward' = dup >>> first cos
--
--     backward' :: BackpropFunc (Float, Float) Float
--     backward' = second (sin >>> negate) >>> (*)
--
--   sin :: BackpropFunc Float Float
--   sin = ...
-- @
--
-- Here we use @Prelude@ implementation for ordinary @cos@ function in 'call'.
-- The forward function is differentiable (which is needed for further derivatives) function
-- with two output values.
-- Roughly speaking 'forward' is
-- @x -> (sin x, x)@.
-- The first term of the tuple is just @sin@ and
-- the second terms @x@ in the tuple is the value to be reused on the backward step.
-- The 'backward' is
-- @(dy, x) -> dy * (-cos x)@,
-- where @dy@ is the derivative found on the previous backward step and the second value is @x@ stored by `forward`.
-- We simply multiply with @(*)@ the derivative @dy@ on the derivative of @sin@ that is @-cos@.
--
-- The stored value is not necessary just @x@. It could be anything useful for the backward step, see for example
-- the implementation for @exp@ and the corresponding
-- [example](InfBackprop.Tutorial#differentiation_with_logging)
-- below.


type E = SimpleExpr

h :: BinnarySymbolicFunc a => a -> a -> a
h = binarySymbolycFunc "h"

h' = smartSimplify . twoArgGradientTuple h :: SimpleExpr -> SimpleExpr -> (SimpleExpr, SimpleExpr)

smartSimplify :: Mapper SimpleExpr SimpleExpr a a => a -> a
smartSimplify = smartMap simplify


h2' = smartSimplify . gradientTuple (uncurry h) :: (SimpleExpr, SimpleExpr) -> (SimpleExpr, SimpleExpr)
h3' = curry $ smartSimplify . gradientTuple (uncurry h) :: SimpleExpr -> SimpleExpr -> (SimpleExpr, SimpleExpr)




--h2'' = curry $ tupleGradientTuple $ gradientTuple (uncurry h) :: SimpleExpr -> SimpleExpr -> ((SimpleExpr, SimpleExpr), (SimpleExpr, SimpleExpr))


x = variable "x"
y = variable "y"

f :: SymbolicFunc a => a -> a
f = unarrySymbolycFunc "f"

g :: SymbolicFunc a => a -> a
g = unarrySymbolycFunc "g"

fg :: (SymbolicFunc a, Multiplicative a) => (a, a) -> a
fg (x_, y_) = f x_ * g y_
-- :: forall a. (SymbolicFunc a, Multiplicative a) => (a, a) -> a

_ = fg (x, y)

gradientTuple2 :: GradientTupleType a a a
gradientTuple2 = gradientTuple

fg' :: (SimpleExpr, SimpleExpr) -> (SimpleExpr, SimpleExpr)
-- fg' :: forall a. (a ~ T a) => (a, a) -> (T a, T a)
--fg' :: forall a. (a ~ T a) => (a, a) -> End (T a) (T a, T a)
fg' = gradientTuple fg
--fg' = (gradientTuple :: GradientTupleType a a a) fg


temp10' = fg' (x, y)

--tupleGradientTuple :: forall a00 a01 a10 a11 b.
--  (Basis (T b), Additive (T a00), Additive (T a01), Additive (T a10), Additive (T a11)) =>
--  (
--    (
--      (DFunc ((a00, a01), (a10, a11)) a00, DFunc ((a00, a01), (a10, a11)) a01),
--      (DFunc ((a00, a01), (a10, a11)) a10, DFunc ((a00, a01), (a10, a11)) a11)
--    )
--    -> DFunc ((a00, a01), (a10, a11)) b
--  )
--  -> ((a00, a01), (a10, a11))
--  -> End (T b) ((T a00, T a01), (T a10, T a11))
-- tupleGradientTuple :: GradientTupleType a a (a, a)
tupleGradientTuple :: (Basis (T b0), Basis (T b1), Additive (T a0), Additive (T a1), Additive (T b0), Additive (T b1)) =>
  ((DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> (DFunc (a0, a1) b0, DFunc (a0, a1) b1)) -> 
  (a0, a1) -> 
  (End (T b0) (T a0, T a1), End (T b1) (T a0, T a1))
-- End (T b0, T b1) (T a0, T a1)
--tupleGradientTuple :: GradientTupleType a0 a1 (b0, b1)
tupleGradientTuple = customDerivative tupleLensIso tupleLensIso

-- _ = gradientTuple fg -- :: forall t. Additive (T t) => (DFunc t SimpleExpr, DFunc t SimpleExpr) -> (DFunc t SimpleExpr, DFunc t SimpleExpr)

fg'' :: (E, E) -> ((E, E), (E, E))
-- fg'' :: forall a. (a ~ T a) => (a, a) -> End (T (End (T a) (T a, T a))) (T a, T a)
--fg'' = smartSimplify . tupleGradientTuple $ gradientTuple2 fg
--fg'' = (gradientTuple :: GradientTupleType a a (a, a)) (gradientTuple fg)
-- fg'' = gradientTuple $ gradientTuple fg
-- fg'' = gradientTuple $ (gradientTuple fg :: (DFunc t SimpleExpr, DFunc t SimpleExpr) -> (DFunc t SimpleExpr, DFunc t SimpleExpr))
--fg'' = tupleGradientTuple (gradientTuple fg)
fg'' = smartSimplify $ tupleGradientTuple $ gradientTuple fg
-- fg'' = tupleGradientTuple (
--  gradientTuple fg :: (Additive (T t)) =>
--    (DFunc t SimpleExpr, DFunc t SimpleExpr) ->
--    (DFunc t SimpleExpr, DFunc t SimpleExpr)
--  )
--fg'' :: forall a. (a ~ T a) => (a, a) -> (End (T a) (T a, T a), End (T a) (T a, T a))
-- fg'' = (gradientTuple :: (a ~ T a) => GradientTupleType a a (a, a)) fg'

--tupleGradientOfDoubleTuple :: (Basis (T b0), Basis (T b1), Additive (T a0), Additive (T a1), Additive (T b0), Additive (T b1)) =>
--  ((DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> (DFunc (a0, a1) b0, DFunc (a0, a1) b1)) ->
--  (a0, a1) ->
--  End (T b0, T b1) (T a0, T a1)
tupleGradientOfDoubleTuple :: (
      Additive (T a0), Additive (T a1), 
      Additive (T b00), Additive (T b01), Additive (T b10), Additive (T b11), 
      Basis (T b00), Basis (T b01), Basis (T b10), Basis (T b11)
    ) =>
  ((DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> ((DFunc (a0, a1) b00, DFunc (a0, a1) b01), (DFunc (a0, a1) b10, DFunc (a0, a1) b11))) 
  -> (a0, a1) 
  -> ((End (T b00) (T a0, T a1), End (T b01) (T a0, T a1)), (End (T b10) (T a0, T a1), End (T b11) (T a0, T a1)))
tupleGradientOfDoubleTuple = customDerivative (tupleLensIso % crossed tupleLensIso tupleLensIso) tupleLensIso 

fg''' :: (E, E) -> (((E, E), (E, E)), ((E, E), (E, E)))
--fg''' = tupleGradientOfDoubleTuple (tupleGradientTuple (gradientTuple2 fg))
fg''' = tupleGradientOfDoubleTuple $ tupleGradientTuple $ gradientTuple fg

tupleGradientOfTrippleTuple :: (
      Additive (T a0), Additive (T a1), 
      Additive (T b000), Additive (T b010), Additive (T b100), Additive (T b110), Additive (T b001), Additive (T b011), Additive (T b101), Additive (T b111),
      Basis (T b000), Basis (T b010), Basis (T b100), Basis (T b110), Basis (T b001), Basis (T b011), Basis (T b101), Basis (T b111)
    ) =>
  (
    (DFunc (a0, a1) a0, DFunc (a0, a1) a1) -> 
    (
      ((DFunc (a0, a1) b000, DFunc (a0, a1) b100), (DFunc (a0, a1) b010, DFunc (a0, a1) b110)), 
      ((DFunc (a0, a1) b001, DFunc (a0, a1) b101), (DFunc (a0, a1) b011, DFunc (a0, a1) b111)) 
    )
  )
  -> (a0, a1) 
  -> (
    ((End (T b000) (T a0, T a1), End (T b100) (T a0, T a1)), (End (T b010) (T a0, T a1), End (T b110) (T a0, T a1))),
    ((End (T b001) (T a0, T a1), End (T b101) (T a0, T a1)), (End (T b011) (T a0, T a1), End (T b111) (T a0, T a1)))
  )
tupleGradientOfTrippleTuple = customDerivative (tupleLensIso % crossed tupleLensIso tupleLensIso % crossed (crossed tupleLensIso tupleLensIso) (crossed tupleLensIso tupleLensIso)) tupleLensIso 

fg'''' :: (E, E) -> ((((E, E), (E, E)), ((E, E), (E, E))), (((E, E), (E, E)), ((E, E), (E, E))))
----fg''' = tupleGradientOfDoubleTuple (tupleGradientTuple (gradientTuple2 fg))
fg'''' = tupleGradientOfTrippleTuple (tupleGradientOfDoubleTuple (tupleGradientTuple $ gradientTuple2 fg))



l2Norm :: (ExpField a, DVF.Arity n) => DVFB.Vec n a -> a
l2Norm = sqrt . DVFC.foldl (+) zero . DVFC.cvec . fmap (^2)

conv :: (Distributive a, DVF.Arity n) => DVFB.Vec n a -> DVFB.Vec n a -> a
conv x_ y_ = DVFC.foldl (+) zero $ DVF.zipWith (*) (DVFC.cvec x_) (DVFC.cvec y_) 

newtype L2Normed n a = UnsafeMkL2Normed {unsafeL2NormValues :: DVFB.Vec n a}

getL2NormedValues :: (DVF.Arity n, ExpField a) => L2Normed n a -> DVFB.Vec n a
getL2NormedValues (UnsafeMkL2Normed v) = fmap (/ l2Norm v) v

--data TangentL2Normed n a = MkTangentL2Normed {
--    point :: L2Normed n a,
--    unsafeTangentL2Norm :: DVFB.Vec n a
--  }
newtype TangentL2Normed n a = UnsafeMkTangentL2Normed {
    manifold :: DVFB.Vec n a -> a
  }

tangentL2NormedFromVector :: DVFB.Vec n a -> DVFB.Vec n a -> a
tangentL2NormedFromVector v w =  


--getTangentL2NormedValues :: (ExpField a, DVF.Arity n) =>
--  TangentL2Normed n a -> DVFB.Vec n a
--getTangentL2NormedValues (MkTangentL2Normed d v) = v - conv dValues v *| dValues where
--  dValues = getL2NormedValues d

type instance Tangent (L2Normed n a) = TangentL2Normed n (Tangent a)



instance (DVF.Arity n, Basis a) => Basis (TangentL2Normed n a) where
  type End (TangentL2Normed n a) b = TangentL2Normed n (End a b)
  initBackProp :: (TangentL2Normed n a -> b) -> TangentL2Normed n (End a b)
  initBackProp bp = MkTangentL2Normed $ initBackProp (bp . MkTangentL2Normed)
  zeroBackProp :: TangentL2Normed n a
  zeroBackProp = MkTangentL2Normed zeroBackProp

cFunc :: LensD Float Float (TangentL2Normed 3 Float) (L2Normed 3 Float)
cFunc = LensD $ \t -> let
    s = UnsafeMkL2Normed $ DVF.mk3 (cos t) (sin t) 0
    bcs = \(MkTangentL2Normed _ (DVF.convert -> (cx, cy, _))) -> -sin t * cx + cos t * cy
  in (s, bcs)



newtype L2NormedD2 a = MkL2NormedD2 {unsafeGetL2NormD2 :: DVFB.Vec 2 a}

getL2NormedD2Values :: (ExpField a) => L2NormedD2 a -> DVFB.Vec 2 a
getL2NormedD2Values (MkL2NormedD2 v) = v |/ l2Norm v

newtype TangentL2NormedD2 a = MkTangentL2NormedD2 {unsafeGetTangentL2NormD2 :: a}

type instance Tangent (TangentL2NormedD2 a) = TangentL2NormedD2 (Tangent a)

instance (Basis a) => Basis (TangentL2NormedD2 a) where
  type End (TangentL2NormedD2 a) b = TangentL2NormedD2 (End a b)
  initBackProp :: (TangentL2NormedD2 a -> b) -> TangentL2NormedD2 (End a b)
  initBackProp bp = MkTangentL2NormedD2 $ initBackProp (bp . MkTangentL2NormedD2)
  zeroBackProp :: TangentL2NormedD2 a
  zeroBackProp = MkTangentL2NormedD2 zeroBackProp


--instance (Divisive a, Additive a, ExpField a) =>
--  Differentiable (L2Normed a) (TangentL2Normed a) where
--    startBackprop lna = undefined




--tupleArgDerivative ::
--  (
--    (
--      DFunc (SimpleExpr, SimpleExpr) SimpleExpr,
--      DFunc (SimpleExpr, SimpleExpr) SimpleExpr
--    )
--    -> DFunc (SimpleExpr, SimpleExpr) SimpleExpr
--  )
--  -> (SimpleExpr, SimpleExpr)
--  -> End (T SimpleExpr) (T SimpleExpr, T SimpleExpr)
--tupleArgDerivative :: forall a0 a1 b.
--  (Basis (T b), Additive (T a0), Additive (T a1)) =>
--  (
--    (
--      DFunc (a0, a1) a0,
--      DFunc (a0, a1) a1
--    )
--    -> DFunc (a0, a1) b
--  )
--  -> (a0, a1)
--  -> End (T b) (T a0, T a1)
--tupleArgDerivative = customDerivative
--  idIso -- (idIso :: O.Iso' (DFunc (a0, a1) b) (DFunc (a0, a1) b))
--  tupleLensIso -- (tupleLensIso :: O.Iso' (DFunc (a0, a1) (a0, a1)) (DFunc (a0, a1) a0, DFunc (a0, a1) a1))

--h1 :: (x0, x1) -> Float
--h1 = undefined
--
--h1'_ :: forall x0 x1. (x0, x1) -> End (T Float) (T x0, T x1)
--h1'_ = tupleArgDerivative h1
-- h'_ = tupleArgDerivative (uncurry h) :: (SimpleExpr, SimpleExpr) -> (SimpleExpr, SimpleExpr)
-- h2' = smartSimplify . tupleArgDerivative h :: (SimpleExpr, SimpleExpr) -> (SimpleExpr, SimpleExpr)



--
-- A composition of two functions like
--
-- \[
--   g(x) := \log x^3
-- \]
--
-- must be defined with the categorical composition '(>>>)' (or '(<<<)')
--
-- >>> import InfBackprop (log)
-- >>> import Control.Category ((>>>), (<<<))
-- >>> smoothG = pow 3 >>> log
--
-- For more complicated expressions, for example,
--
-- \[
--   h(x) := x^2 + x^3
-- \]
--
-- we use arrow notations '(***)', 'first' and 'second' as follows
--
-- >>> import InfBackprop ((+), dup)
-- >>> import Control.CatBifunctor ((***))
--
-- >>> smoothH = dup >>> (pow 2 *** pow 3) >>> (+) :: BackpropFunc Float Float
--
-- where
--
-- @
--   dup :: BackpropFunc a (a, a)
-- @
--
-- is differentiable function that simply splits the single implicit argument @x@ into the tuple '(x, x)'.
-- THis is needed path tje implicit @x@ to two independent functions 'pow' @2@ and 'pow' @3@.
-- The last
--
-- @
--   (+) :: BackpropFunc (a, a) a
-- @
--
-- operation transforms the pair of implicit arguments into their sum.
--
--
--
--
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude (Float, fmap)
-- >>> import InfBackprop (BackpropFunc, call, derivative, derivativeN, pow)
-- >>> import Debug.SimpleExpr (variable, number, simplify)
-- >>> import Debug.DiffExpr (SymbolicFunc, unarrySymbolycFunc, BinnarySymbolicFunc, binarySymbolycFunc)
--
-- ...
--
-- >>> x = variable "x"
-- >>> f = unarrySymbolycFunc "f"
-- >>> f x
-- f(x)
