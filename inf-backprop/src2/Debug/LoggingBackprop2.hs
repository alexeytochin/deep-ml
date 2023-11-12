{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  Debug.LoggingBackprop
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Basics for simple expressions equipped with Monadic behaviour.
-- In particular, basic functions with logging for debug and illustration purposes.
-- See [this tutorial section](InfBackprop.Tutorial#differentiation_monadic_types) for details.
module Debug.LoggingBackprop2
  ( -- * Generic logging functions
  )
where

import Control.Arrow (Kleisli (Kleisli))
import Control.CatBifunctor (first, second, (***))
import Control.Category ((.), (>>>))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Text (pack)
import Debug.SimpleExpr.Expr (SimpleExpr, unaryFunc, number, variable, simplify, content)
import InfBackprop.Common6 (
  D(D), const_, lensCtoP, derivative, addC, multC, derivative1, crossC, (%), derivative0,
  (.*.), merge
  )
import IsomorphismClass.Isomorphism (iso)
import NumHask (Additive, Distributive, Divisive, ExpField, Multiplicative, Subtractive, TrigField, fromInteger, zero, (*), MultiplicativeAction, (.*))
import qualified NumHask as NH
import qualified NumHask.Prelude as NHP
import qualified Prelude.InfBackprop
import Prelude (Monad, Show, String, pure, return, show, ($), (<>), fmap, uncurry, curry, undefined)
import qualified Prelude as P
import Debug.SimpleExpr (Expr)


instance MultiplicativeAction SimpleExpr SimpleExpr where
   (.*) = (*) 

--instance MultiplicativeAction (SimpleExpr, SimpleExpr) SimpleExpr where
--   a .* (m1, m2) = (a .* m1, a .* m2) 

instance (MultiplicativeAction m1 SimpleExpr, MultiplicativeAction m2 SimpleExpr) => 
  MultiplicativeAction (m1, m2) SimpleExpr where
    a .* (m1, m2) = (a .* m1, a .* m2) 


--instance (MultiplicativeAction m a) => 
--  MultiplicativeAction (m, m) a where
--    a .* (m1, m2) = (a .* m1, a .* m2) 



class SymbolicFunc a where
  unarrySymbolycFunc :: String -> a -> a

instance SymbolicFunc SimpleExpr where
  unarrySymbolycFunc = unaryFunc

unarrySymbolycFuncC :: (SymbolicFunc a, Multiplicative a) => 
  String -> D a a a a
unarrySymbolycFuncC funcName = 
  D (unarrySymbolycFunc funcName) (\x dy -> unarrySymbolycFunc (funcName <> "'") x * dy )

instance (SymbolicFunc a, Multiplicative a) => 
  SymbolicFunc (D a a a a) where
    unarrySymbolycFunc funcName = lensCtoP (unarrySymbolycFuncC funcName)

pureKleisli :: Monad m => (a -> b) -> Kleisli m a b
pureKleisli f = Kleisli $ pure . f

x = variable "x"
y = variable "y"

f :: SymbolicFunc a => a -> a
f = unarrySymbolycFunc "f"

g :: SymbolicFunc a => a -> a
g = unarrySymbolycFunc "g"

--temp3_0_0 = derivative0 f x :: SimpleExpr
--temp3_0_1 = derivative0 (\a -> f a * const_ (number 2)) x :: SimpleExpr
--temp3_1_0 = derivative f x :: SimpleExpr
--temp3_1_1 = derivative (\a -> f a * const_ (number 2)) x :: SimpleExpr

--temp5 = (derivative (derivative f)) x :: SimpleExpr
--temp5_1 = derivative2 f x :: SimpleExpr


simplify2 :: Expr a => a -> [SimpleExpr]
simplify2 a = fmap simplify (content a) 
  
--  exprToGraph :: Expr d => d -> DGraph String ()
--  exprToGraph d = case content d of
--    [] -> empty -- insertVertex (name e) empty
--    [v] -> simpleExprToGraph v
--    (v : vs) -> simpleExprToGraph v `union` exprToGraph vs -- insertArc newArcV addedV where

fg :: SymbolicFunc a => (a, a) -> (a, a)
fg (a, b) = (f a, g b) 

example_3_0 = derivative0 fg (x, y)
--example_3_1 = derivative1 fg (x, y)

--example_4_0 = derivative0 (f * g) x
--example_4_1 = derivative1 (f * g) x
--example_4_2 = derivative2 (f * g) x

--example_5_0_0 = derivative0 (uncurry (*)) (x, y)
--example_5_1_0 = derivative1 (uncurry (*) . merge) (x, y)
---- example_5_2_0 = derivative2 (uncurry (*) . merge) (x, y)  -- :: ((SimpleExpr, SimpleExpr), (SimpleExpr, SimpleExpr))
--example_5_0 = derivative0 (uncurry2 (*)) (x, y) :: SimpleExpr
--example_5_1 = derivative1 (uncurry2 (*)) (x, y) :: (SimpleExpr, SimpleExpr)
-- example_5_2 = derivative2 (uncurry2 (*)) (x, y) :: ((SimpleExpr, SimpleExpr), (SimpleExpr, SimpleExpr))
-- example_5_1 = derivative1 (uncurry (*)) (x, y)


temp10 :: (forall a. a -> a) -> b
temp10 f = temp11 f -- undefined

temp11 :: (a -> a) -> b
temp11 = undefined -- temp10


--example_6 = derivative1 (lensCtoP multC) (x, y)
--example_6_0 = derivative0 (lensCtoP multC) (x, y) :: SimpleExpr
--example_6_1 = simplify2 $ derivative1 (lensCtoP multC) (x, y)
--example_7 = simplify2 $ derivative1 (
--    lensCtoP (multC % (crossC f g)) 
--  ) (crossC x y) -- :: [SimpleExpr]


-- temp_ = (crossC x y)  -- :: D SimpleExpr SimpleExpr SimpleExpr SimpleExpr

-- Differentiable functions.

-- | Returns symbolically differentiable Simple Expression.
--
-- ==== __Examples of usage__
--
-- >>> import Control.Arrow (runKleisli)
-- >>> import Debug.SimpleExpr (variable, number, simplify)
-- >>> import InfBackprop.Common3 (derivative, derivative2, derivative0)
--
-- >>> x = variable "x"
-- >>> f = unarrySymbolycFunc "f"
-- >>> f x
-- f(x)
--
-- >>> simplify $ (derivative f) x :: SimpleExpr
-- f'(x)
-- 
-- >>> g = unarrySymbolycFunc "g"
-- >>> simplify $ (derivative (f * g)) x :: SimpleExpr
-- (g(x)·f'(x))+(f(x)·g'(x))
-- 
-- >>> simplify $ derivative (\a -> f a * const_ (number 2)) x :: SimpleExpr
-- 2·f'(x)
-- 
-- >>> simplify $ (derivative2 f) x :: SimpleExpr
-- f''(x)
-- 
-- -- >>> simplify $ (derivative0 (f . g) x) :: SimpleExpr
-- f(g(x))
--
-- >>> h = unarrySymbolycFunc "h"
-- >>> simplify $ (derivative (f . g . h) x) :: SimpleExpr
-- h'(x)·(g'(h(x))·f'(g(h(x))))
--
-- >>> simplify $ (derivative (f . g)) x :: SimpleExpr
-- g'(x)·f'(g(x))
-- 
-- >>> simplify $ (derivative2 (f . g)) x :: SimpleExpr
-- (f'(g(x))·g''(x))+(g'(x)·(g'(x)·f''(g(x))))
--
-- >>> derivative1 (lensCtoP multC) (x, y)
-- (y·1,x·1)
-- 
-- >>> simplify2 $ derivative1 (lensCtoP multC) (x, y)
-- [y,x]
temp0 = ()

--backpropExpr :: String -> D SimpleExpr SimpleExpr SimpleExpr SimpleExpr
--backpropExpr funcName = D (unaryFunc funcName) (\(dy, x) -> unaryFunc (funcName <> "'") x * dy)

-- | Tests
--
-- >>>
temp = ()