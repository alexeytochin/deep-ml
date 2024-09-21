{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}

module Numeric.InfBackprop.TutorialExamples where

import Debug.DiffExpr (SymbolicFunc, unarrySymbolycFunc, BinarySymbolicFunc, binarySymbolycFunc)
import Data.Stream (Stream, take, fromList)
import Debug.SimpleExpr (variable, SimpleExpr, simplify)
import GHC.Show (show)
import GHC.Base ((<>), (.), Int, ($))
import Numeric.InfBackprop (derivative, gradient, tupleDerivative)
import NumHask (Additive, (+), Multiplicative, (*))
import Debug.SimpleExpr (SE)
import Data.Proxy (Proxy(Proxy))
import Data.Tuple (curry, uncurry)
import Numeric.InfBackprop.Diff (twoArgsDerivative, customArgDerivative, tupleArg)

xVar :: SimpleExpr
xVar = variable "x"

streamF :: SymbolicFunc a => a -> Stream a
streamF x_ = fromList [unarrySymbolycFunc ("f_" <> show n) x_ | n <- [0 :: Int ..]]
_ = take 5 (streamF xVar)

streamF' :: SimpleExpr -> Stream SimpleExpr
streamF' = simplify . derivative streamF
_ = take 5 (streamF' xVar)

yVar :: SimpleExpr
yVar = variable "y"

h :: BinarySymbolicFunc a => a -> a -> a
h = binarySymbolycFunc "h"

-- h' = simplify . twoArgGradientTuple h :: SimpleExpr -> SimpleExpr -> (SimpleExpr, SimpleExpr)

f4 :: Additive a => (a, a) -> a
f4 (x, y) = x + y

_ = gradient (Proxy @SE) f4 (xVar, yVar) :: (SE, SE)

_ = gradient (Proxy @SE) (uncurry h) (xVar, yVar) :: (SE, SE)

_ = twoArgsDerivative h xVar yVar :: (SE, SE)

f :: SymbolicFunc a => a -> a
f = unarrySymbolycFunc "f"
g :: SymbolicFunc a => a -> a
g = unarrySymbolycFunc "g"

fg :: (SymbolicFunc a, Multiplicative a) => (a, a) -> a
fg (x, y) = f x * g y

fg'' :: (SE, SE) -> ((SE, SE), (SE, SE))
fg'' = simplify . tupleDerivative (tupleDerivative fg)

fg''' :: (SE, SE) -> (((SE, SE), (SE, SE)), ((SE, SE), (SE, SE)))
fg''' = simplify . tupleDerivative (tupleDerivative (tupleDerivative fg))

-- _ = customArgDerivative tupleArg
