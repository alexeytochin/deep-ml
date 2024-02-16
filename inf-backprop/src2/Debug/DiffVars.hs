module Debug.DiffVars where

import Debug.DiffExpr (SymbolicFunc, unarrySymbolycFunc, BinnarySymbolicFunc, binarySymbolycFunc) 
import Debug.SimpleExpr (variable, simplify, SimpleExpr, number)
import InfBackprop.Common6 (derivative, gradientTuple, tupleArg, customDerivative, derivativeX, derivativeY, streamToLens)
import Data.Function (($))
import NumHask ((*), (+), Additive, Distributive)
import InfBackprop.LensD (stopDiff, LensD)
import Prelude ((.), show, (<>), id, Float, undefined, Integer)
import Data.Stream (Stream, fromList, iterate, take)
import Data.FiniteList (FiniteList)
--import Control.DeepSeq (force, NFData(..))


--instance NFData SimpleExpr

x = variable "x"
y = variable "y"
z = variable "z"


f :: SymbolicFunc a => a -> a
f = unarrySymbolycFunc "f"

g :: SymbolicFunc a => a -> a
g = unarrySymbolycFunc "g"

h :: SymbolicFunc a => a -> a
h = unarrySymbolycFunc "h"

k :: BinnarySymbolicFunc a => a -> a -> a
k = binarySymbolycFunc "k"

f2 :: forall a. (BinnarySymbolicFunc a, SymbolicFunc a) => (a, a) -> a
f2 (x, y) = k (f x) (g y)
temp = (derivative . tupleArg) f2 (x, y) :: (SimpleExpr, SimpleExpr)

-- | Returns symbolically differentiable Simple Expression.
--
-- ==== __Examples of usage__
--
-- >>> import Control.Arrow (runKleisli)
-- >>> import Debug.SimpleExpr (variable, number, simplify)
-- >>> import NumHask (Additive, Distributive)
-- >>> import InfBackprop.Common6 (derivative, tupleArg)
-- >>> import GHC.Prim (seq)
-- >>> import Prelude.Tools (cross)
-- >>> import Prelude (fmap)
--
-- -- >>> s0 = show (derivative f x :: SimpleExpr)
-- -- >>> s = show ((derivative . tupleArg) f2 (x, y) :: (SimpleExpr, SimpleExpr))
-- -- >>> s
-- -- XXX
--
-- >>> x = variable "x"
-- >>> f = unarrySymbolycFunc "f"
-- >>> f x
-- f(x)
--
-- >>> simplify $ derivative f x :: SimpleExpr
-- f'(x)
-- 
-- >>> g = unarrySymbolycFunc "g"
-- >>> simplify $ derivative (f * g) x :: SimpleExpr
-- (f'(x)*g(x))+(g'(x)*f(x))
-- 
-- >>> simplify $ derivative (\a -> stopDiff (number 2) * f a) x :: SimpleExpr
-- f'(x)*2
-- 
-- >>> simplify $ (derivative $ derivative f) x :: SimpleExpr
-- f''(x)
-- 
-- >>> (f . g) x :: SimpleExpr
-- f(g(x))
--
-- >>> simplify $ derivative (f . g) x :: SimpleExpr
-- g'(x)*f'(g(x))
--
-- >>> simplify $ derivative (f . g . h) x :: SimpleExpr
-- h'(x)*(g'(h(x))*f'(g(h(x))))
--
-- >>> simplify $ (derivative $ derivative (f . g)) x :: SimpleExpr
-- (g''(x)*f'(g(x)))+(g'(x)*(f''(g(x))*g'(x)))
--
-- >>> (cross simplify simplify) $ gradientTuple (*) x y :: (SimpleExpr, SimpleExpr)
-- (y,x)
--
-- >>> simplify $ derivativeX (*) x y
-- y
--
-- >>> derivative (\x -> x * derivativeY (*) x (stopDiff (2 :: Float))) (1 :: Float)
-- 2.0
--
-- >>> derivative (\x -> x * derivativeY (*) x (stopDiff (2 :: Float))) (1 :: Float)
-- 2.0
-- 
-- >>> take 5 $ (iterate (+ 1) (1 :: Integer))
-- [1,2,3,4,5]
-- 
-- >>> stream = customDerivative streamToLens id streamFunc (x :: SimpleExpr) :: Stream SimpleExpr
-- >>> stream2 = fmap simplify stream
-- >>> take 5 stream2
-- [f_0'(x),f_1'(x),f_2'(x),f_3'(x),f_4'(x)]
--


_ = customDerivative streamToLens id streamFunc (x :: SimpleExpr) :: Stream SimpleExpr

--streamF :: SimpleExpr -> Stream SimpleExpr
--streamF x = fromList [unarrySymbolycFunc ("f_" <> show n) x | n <- [0..]]

streamFunc :: SymbolicFunc a => a -> Stream a
streamFunc x = fromList [unarrySymbolycFunc ("f_" <> show n) x | n <- [0..]]


--streamToLens :: Stream (LensD dt t dx x) -> LensD dt t (Stream dx) (Stream x)
--streamToLens = undefined

