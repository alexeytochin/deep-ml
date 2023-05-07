{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  Debug.SimpleExpr.Expr
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Simple Expression Basic Tutorial.

module InfBackprop.Tutorial.Basics (
    -- * Quick start
    -- $quick_start

    -- * Derivative for symbolic expressions
    -- $derivative for symbolic expressions

    -- * Symbolic expressions visualization
    -- $symbolic_expressions_visualization

    -- * How it works
    -- $how_it_works

    -- * Declaring custom derivative 
    -- $declaring_custom_derivative

    -- * Differentiation of monadic function
    -- $differentiation_monadic_types

    -- * Differentiation with logging
    -- $differentiation_with_logging
  ) where

import Prelude (Maybe(Just, Nothing))
import Control.Monad.Logger (MonadLogger)
import Control.Arrow (Kleisli, (<<<), (>>>), )
import InfBackprop (pow, call, forward, backward, derivative, (***), first, second, Backprop, BackpropFunc, cos)

{- $quick_start
>>> :set -XNoImplicitPrelude
>>> import Prelude (Float, fmap)
>>> import InfBackprop (BackpropFunc, call, derivative, derivativeN, pow)

We define a differentiable function

\[
  f(x) := x^2
\]

as follows

>>> myFunc = pow 2 :: BackpropFunc Float Float

where 'pow' is a power differentiable function and
@ BackpropFunc' :: * -> * -> * @ 
is a type for infinitely differentiable (smooth) functions.
We can get its values by 'call' method like

>>> f = call myFunc :: Float -> Float
>>> fmap f [-3, -2, -1, 0, 1, 2, 3]
[9.0,4.0,1.0,0.0,1.0,4.0,9.0]

as well as its first derivative by 'derivative' that is

\[
  f'(x) = 2 \cdot x
\]

>>> df = derivative myFunc :: Float -> Float
>>> fmap df [-3, -2, -1, 0, 1, 2, 3]
[-6.0,-4.0,-2.0,0.0,2.0,4.0,6.0]

or the second derivative

\[
  f''(x) = 2
\]

>>> d2f = derivativeN 2 myFunc :: Float -> Float
>>> fmap d2f [-3, -2, -1, 0, 1, 2, 3]
[2.0,2.0,2.0,2.0,2.0,2.0,2.0]

and so on.

A composition like

\[
  f_2(x) := \log x^3
\]

must be defined with the categorical composition '(>>>)' (or '(<<<)')

>>> import InfBackprop (log)
>>> import Control.Category ((>>>), (<<<))
>>> myFunc2 = pow 3 >>> log


For more complicated things like

\[
  f_3(x) := x^2 + x^3
\]

we use arrow notations '(***)', 'first' and 'second' as follows

>>> import InfBackprop ((+), dup)
>>> import Data.CatBifunctor ((***))
>>> myFunc3 = dup >>> (pow 2 *** pow 3) >>> (+) :: BackpropFunc Float Float

where

@
  dup :: BackpropFunc a (a, a)
@

function simple split the single implicit argument @x@ into tuple '(x, x)'
to be processed by two independent functions 'pow' @2@ and 'pow' @3@.
The last

@
  (+) :: BackpropFunc (a, a) a
@

operation transforms the pair of implicit variables into their sum.
-}

{- $symbolic_expressions_visualization
To demonstrate how the differentiation works on expressions we define simple syntactic expressions
in submodule
'Debug.SimpleExpr.Expr'

>>> import Control.Category ((<<<))
>>> import InfBackprop (call, backpropExpr)
>>> import Debug.SimpleExpr.Expr (SimpleExpr, variable, simplify)
>>> import Debug.SimpleExpr.GraphUtils (exprToGraph)
>>> import Data.Graph.VisualizeAlternative (plotDGraph)

together with some visualization tools.
It works as follows. The function

\[
  g(f(x))
\]

is defined as

>>> x = variable "x"
>>> call (backpropExpr "g" <<< backpropExpr "f") x
g(f(x))

It can be plotted by

@ plotDGraph $ exprToGraph $ call (backpropExpr "g" <<< backpropExpr "f") x @

![image description](doc/images/composition.png)

The first derivative graph can be obtained by

@ plotDGraph $ exprToGraph $ simplify $ derivative (backpropExpr "g" <<< backpropExpr "f") x @

![image description](doc/images/composition_derivative.png)

where `simplifyExpr :: SimpleExpr -> SimpleExpr` is a simple removal of `1`s and `0`s.

As well as the second derivative is straightforward

@ plotDGraph $ exprToGraph $ simplify $ derivativeN 2 (backpropExpr "g" <<< backpropExpr "f") x @

![image description](doc/images/composition_second_derivative.png)

-}

{- $derivative for symbolic expressions

>>> import Prelude (($))
>>> import Control.Category ((<<<))
>>> import InfBackprop (BackpropFunc, call, derivative, derivativeN, sin, pow, (**), pow, setSecond, const)
>>> import Debug.SimpleExpr.Expr (SimpleExpr, variable, simplify)

We can define a function

\[
  f(x) := \sin x^2
\]

like

>>> import InfBackprop (sin, pow, (**), setSecond)
>>> f = sin <<< pow 2 :: BackpropFunc SimpleExpr SimpleExpr

and a variable.

>>> x = variable "x"

We can call the function:

>>> call f x
sin(x·x)

We can find the derivative

\[
  \frac{d}{d x} f(x) = \frac{d}{d x} \sin x^2 = 2\, x \cos x^2
\]

as follows

>>> simplify $ derivative f x
cos(x·x)·(2·x)

as well as second and all higher derivatives

>>> simplify $ derivativeN 2 f x
(((2·x)·-(sin(x·x)))·(2·x))+(2·cos(x·x))

-}

{- $how_it_works
Consider a composition of functions
\[
  g(f(h(x))).
\]
Its first derivative over @x@ is
\[
  h'(x) \cdot f'(h(x)) \cdot g'(f(h(x)))
\]
According to the backpropagation strategy, the order of the calculation must be as follows.

1. Find @h(x)@.

2. Find @f(h(x))@.

3. Find @g(f(h(x)))@.

4. Find the top derivative @g'(f(h(x)))@.

5. Find the next derivative @f'(h(x))@.

6. Multiply @g'(f(h(x)))@ on @f'(h(x))@.

7. Find the next derivative @h'(x)@.

8. Multiply the output of point 3. on @h'(x)@.

The generalization for longer composition is straightforward.

All calculations related to the function @f@ can be divided into two parts.
We have to find @f@ of @h(x)@ first (forward step) and then the derivative @f@ of the same argument @h(x)@ and
multiply it on the derivative @g'(f(h(x)))@ obtained during the similar calculations for @g@.
Notice that the value of @h(x)@ is reused on the backward step.
To this end, we define type 'Backprop' (see
[docstring](#backprop)
for 'Backprop' for details).
-}

{- $declaring_custom_derivative
A differentiable version of 'cos' function can be defined as follows

@
{-# LANGUAGE InstanceSigs #-}
import Prelude (Float)
import qualified Prelude
import Control.Category ((>>>), second)
import InfBackprop ((*), negate, dup, BackpropFunc, Backprop(MkBackprop))

cos :: BackpropFunc Float Float
cos = MkBackprop call' forward' backward' where
  call' :: Float -> Float
  call' = Prelude.cos

  forward' :: BackpropFunc Float (Float, Float)
  forward' = dup >>> first cos

  backward' :: BackpropFunc (Float, Float) Float
  backward' = second (sin >>> negate) >>> (*)

sin :: BackpropFunc Float Float
sin = ...
@

where 'forward'
is simply
@ x -> (sin x, x) @
and 'backward' is
@ (dy, x) -> dy * (-cos x) @
and the role of @cache@ is played by
@ x :: Float @,


...

-}

{- $differentiation_monadic_types #differentiation_monadic_types#
Monadic functions @a -> m b@ can also be backpropagated.
For example, consider the real-valued power function that is defined for positive real numbers.
For a negative number, it returns 'Nothing' that is a signal to interrupt the derivative calculation, and
return 'Nothing' according to the 'Maybe' monad behavior.
In this case, we can use that the 'Backprop' type is defined for any category not just for functions @(->)@.
In particular, we can attract 'Backprop (Kleisli Maybe)' instead of 'Backprop (->)' from the previous sections.

>>> import Prelude (Maybe, Maybe(Just, Nothing), ($), Ord, (>), Float)
>>> import InfBackprop (Backprop(MkBackprop), derivative, dup, (*), linear, pureBackprop, first, second)
>>> import Control.Arrow (Kleisli(Kleisli), runKleisli, (>>>))
>>> import qualified NumHask as NH

Term 'pureBackprop :: Monad m => Backprop (->) a b -> Backprop (Kleisli m) a b'
is a tool to lift ordinary backpropagation functions to monadic functions that always "return" 'Just' @...@ .

Define the power function as follows

>>> :{
  powR :: forall a. (Ord a, NH.ExpField a) =>
    a -> Backprop (Kleisli Maybe) a a
  powR p = MkBackprop call' forward' backward'
    where
      call' :: Kleisli Maybe a a
      call' = Kleisli $ \x -> if x > NH.zero
        then Just $ x NH.** p
        else Nothing
      forward' :: Backprop (Kleisli Maybe) a (a, a)
      forward' = pureBackprop dup >>> first (powR p)
      backward' :: Backprop (Kleisli Maybe) (a, a) a
      backward' = second der >>> pureBackprop (*) where
        der = powR (p NH.- NH.one) >>> pureBackprop (linear p)
:}

We can calculate

\[
  \frac{d}{dx} x^{\frac12} = \frac{1}{2 \sqrt{x}}
\]

for @x=4@ and @x=-4@ like

>>> runKleisli (derivative (powR 0.5)) (4 :: Float)
Just 0.25
>>> runKleisli (derivative (powR 0.5)) (-4 :: Float)
Nothing
-}

{- $differentiation_with_logging #differentiation_with_logging#

'Backprop' @cat a b@ type is parametrized by a category @cat@, input @a@ and output @b@.
If @cat@ is @(->)@ it is reduced to 'BackpropFunc' we worked with above.
To add logging to the calculation we shall replace @(->)@ by
'MonadLogger' @m =>@ 'Kleisli' @m@.
We will need the imports below.

>>> import Prelude (Integer, Float, ($), (+), (*))
>>> import Control.Monad.Logger (runStdoutLoggingT, MonadLogger)
>>> import Control.Arrow ((>>>), (***), runKleisli, Kleisli)
>>> import InfBackprop (derivative, loggingBackpropExpr)
>>> import Debug.SimpleExpr.Expr (variable)
>>> import Debug.LoggingBackprop (initUnaryFunc, initBinaryFunc, pureKleisli, exp, sin)



...




Here module 'Debug.loggingBackpropExpr' contains some useful functionality.

@
  initUnaryFunc :: (Show a, Show b, MonadLogger m) => String -> (a -> b) -> Kleisli m a b
@

and binary functions

@
  initBinaryFunc :: (Show a, Show b, Show c, MonadLogger m) => String -> (a -> b -> c) -> Kleisli m (a, b) c
@

Both of these terms map given functions to Kleisli category terms that provide logging during their execution.

We illustrate first how it works in the following example

\[
  f(x) = y \cdot 3 + y \cdot 4, \quad y = x + 2.
\]

This function can be defined as follows

>>> :{
  fLogging :: MonadLogger m => Kleisli m Integer Integer
  fLogging =
    initUnaryFunc "+2" (+2) >>>
    (pureKleisli (\x -> (x, x))) >>>
    (initUnaryFunc "*3" (*3) *** initUnaryFunc "*4" (*4)) >>>
    initBinaryFunc "sum" (+)
:}

We run the calculation with @ x = 5 @ as follows

>>> runStdoutLoggingT $ runKleisli fLogging 5
[Info] Calculating +2 of 5 => 7
[Info] Calculating *3 of 7 => 21
[Info] Calculating *4 of 7 => 28
[Info] Calculating sum of 21 and 28 => 49
49

We are ready now to consider an example with derivatives.
Let us calculate a simple example as follows

\[
  \frac{d}{dx} \mathrm{f} (e^x) = e^x f'(e^x)
\]

>>> runStdoutLoggingT $ runKleisli (derivative (exp >>> loggingBackpropExpr "f")) (variable "x")
[Info] Calculating exp of x => exp(x)
[Info] Calculating f of exp(x) => f(exp(x))
[Info] Calculating f' of exp(x) => f'(exp(x))
[Info] Calculating multiplication of 1 and f'(exp(x)) => 1·f'(exp(x))
[Info] Calculating multiplication of 1·f'(exp(x)) and exp(x) => (1·f'(exp(x)))·exp(x)
(1·f'(exp(x)))·exp(x)

We can set 'f = sin' and 'x=2'

\[
  \left. \frac{d}{dx} \sin (e^x) \right|_{x=2} = e^2 \cos (e^2)
\]

>>> runStdoutLoggingT $ runKleisli (derivative (exp >>> sin)) (2 :: Float)
[Info] Calculating exp of 2.0 => 7.389056
[Info] Calculating sin of 7.389056 => 0.893855
[Info] Calculating cos of 7.389056 => 0.44835615
[Info] Calculating multiplication of 1.0 and 0.44835615 => 0.44835615
[Info] Calculating multiplication of 0.44835615 and 7.389056 => 3.312929
3.312929

The first thing to be mentioned in these logs is that the last forward step
@sin(exp x)@
is still calculated in contrast with the examples from previous section.
It is because of the monadic nature of the calculation chain that must disappear once we switched back to
@(->)@ from 'Kleisli' @m@.

The second thing to be mentioned here is that the exponent
@exp x@
is calculated just once thanks to the cache term that is passed from the `forward` to `backward` method.
-}
