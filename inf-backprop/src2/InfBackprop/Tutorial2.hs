{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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

    -- * Multiple values function
    -- $multiple_values
    
    -- * Gradinet
    -- $gradient

    -- * Gradinet of multiple values function
    -- $$multiple_values_gradient

    -- * How it works
    -- $how_it_works

    -- * Declaring custom derivative
    -- $declaring_custom_derivative
  )
where

import Control.Arrow (Kleisli, (<<<), (>>>))
import Control.Monad.Logger (MonadLogger)
import Debug.SimpleExpr (SimpleExpr, simplify)
import Prelude (Maybe (Just, Nothing), Monad)

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
-- package is equipped with a visulaisation tool that can be used to illustrate differentiation workflow.
--
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude (($), (.))
-- >>> import Debug.SimpleExpr (SimpleExpr, variable, simplify)
-- >>> import Debug.SimpleExpr.GraphUtils (exprToGraph)
-- >>> import Data.Graph.VisualizeAlternative (plotDGraph)
-- >>> import Debug.DiffExpr (unarrySymbolycFunc)
--
-- As a warm up consider a trivial composition of two functions
--
-- \[
--   g(f(x))
-- \]
--
-- is defined as
--
-- >>> x = variable "x"
-- >>> f = unarrySymbolycFunc "f"
-- >>> g = unarrySymbolycFunc "g"
-- >>> (g . f) x
-- g(f(x))
--
-- It can be plotted by
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






-- $quick_start_multiple_values
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude (Float, fmap, ($), Int)
-- >>> import NumHask ((*), (+), log, Multiplicative)
-- >>> import Numeric.InfBackprop (derivative, intPow)
-- >>> 


-- $quick_start_gradient
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude (Float, fmap, ($), Int)
-- >>> import NumHask ((*), (+), log, Multiplicative)
-- >>> import Numeric.InfBackprop (derivative, intPow)
-- >>> 





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
