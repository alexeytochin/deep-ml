{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Debug.DiffExpr where

import Data.Type.Equality (type (~))
import Control.Arrow (Kleisli (Kleisli))
--import Control.CatBifunctor (first, second, (***))
import Control.Category ((.))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Text (pack)
import Debug.SimpleExpr.Expr (SimpleExpr, unaryFunc, binaryFunc, number, variable, simplifyExpr, content,
    SimpleExprF(SymbolicFuncF)
  )
--import InfBackprop.Common6 (
--  D(D), const_, lensCtoP, derivative, addC, multC, derivative1, crossC, (%), derivative0,
--  (.*.), merge
--  )
--import IsomorphismClass.Isomorphism (iso)
import NumHask (Additive, Distributive, Divisive, ExpField, Multiplicative, Subtractive, TrigField, fromInteger, zero, 
  (*), MultiplicativeAction, (^^), Integral)
import qualified NumHask as NH
import qualified NumHask.Prelude as NHP
--import qualified Prelude.InfBackprop
import Prelude (Monad, Show, String, pure, return, show, ($), (<>), fmap, uncurry, curry, undefined, Ord)
import qualified Prelude as P
import Debug.SimpleExpr (Expr)
--import InfBackprop.LensD (DFunc, LensD(LensD), tupleToLens, lensCtoP, IntegralPower, integralPow)
--import InfBackprop.Tangent (T)
import Debug.Trace (trace)
import Data.Fix (Fix (Fix, unFix))
import Numeric.InfBackprop.Tangent (CT)
import Numeric.InfBackprop.DFunc2 (DFunc(MkDFunc), tupleToLens)
--import Debug.Hood.Observe (observe)
--import InfBackprop.Tangent (Tangent, T)



twoArgFunc :: String -> SimpleExpr -> SimpleExpr -> SimpleExpr
twoArgFunc name x y = Fix (SymbolicFuncF name [x, y])


class SymbolicFunc a where
  unarrySymbolycFunc :: String -> a -> a
  -- binnarySymbolycFunc :: String -> a -> a -> a

--instance (SimpleExpr a, SimpleExpr b) => SymbolicFunc (a, b) where
--  unarrySymbolycFunc :: String -> (a, b) -> (a, b)
  

instance SymbolicFunc SimpleExpr where
  unarrySymbolycFunc = unaryFunc
--  unarrySymbolycFunc name x = 
--    trace ("\n:> Calculating " <> name <> " of " <> show x) $ 
--      unaryFunc name x

unarrySymbolycFuncC :: (SymbolicFunc a, Multiplicative a, CT a ~ a) =>
  String -> DFunc a a
unarrySymbolycFuncC funcName = 
  MkDFunc $ \x -> (
      unarrySymbolycFunc funcName x, 
      \dy -> unarrySymbolycFunc (funcName <> "'") x * dy
    )

instance (SymbolicFunc a, Multiplicative a, CT a ~ a) => 
  SymbolicFunc (DFunc t a) where
    unarrySymbolycFunc funcName fx = unarrySymbolycFuncC funcName . fx


class BinnarySymbolicFunc a where
  binarySymbolycFunc :: String -> a -> a -> a

instance BinnarySymbolicFunc SimpleExpr where
  binarySymbolycFunc = twoArgFunc
--  binarySymbolycFunc name x y =
--    trace ("\n:> Calculating " <> name <> " of " <> show x <> " and " <> show y) $ 
--      twoArgFunc name x y

binnarySymbolycFuncC :: (BinnarySymbolicFunc a, Multiplicative a, CT a ~ a) => 
  String -> DFunc (a, a) a
binnarySymbolycFuncC funcName = 
  MkDFunc $ \(x, y) -> (
      binarySymbolycFunc funcName x y,
      \dy -> (binarySymbolycFunc (funcName <> "_1") x y * dy, binarySymbolycFunc (funcName <> "_2") x y * dy)
    )

binarySymbolycLens :: (BinnarySymbolicFunc a, Multiplicative a, CT a ~ a) =>
  String -> DFunc t (a, a) -> DFunc t a
binarySymbolycLens funcName (MkDFunc a) = MkDFunc $ \t -> let
    ((x1, x2), dyx) = a t
  in (
      binarySymbolycFunc funcName x1 x2,
      \dy -> dyx (binarySymbolycFunc (funcName <> "'_1") x1 x2 * dy, binarySymbolycFunc (funcName <> "'_2") x1 x2 * dy)
    )

instance (BinnarySymbolicFunc a, Distributive a, Additive (CT t), CT a ~ a) =>
  BinnarySymbolicFunc (DFunc t a) where -- (DFunc a a) where  -- ( (T a) a (T a) a) where
    binarySymbolycFunc funcName fx fy = binnarySymbolycFuncC funcName . tupleToLens (fx, fy)

--    binarySymbolycFunc funcName = lensCtoP (binarySymbolycFuncC funcName)




class TraceSymbolicFunc a where
  traceUnarrySymbolycFunc :: String -> a -> a

instance TraceSymbolicFunc SimpleExpr where
--  traceUnarrySymbolycFunc = unaryFunc
  traceUnarrySymbolycFunc name x = 
    trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " >>>") $ 
      unaryFunc name x

traceUnarrySymbolycFuncC :: (TraceSymbolicFunc a, Multiplicative a, CT a ~ a) =>
  String -> DFunc a a
traceUnarrySymbolycFuncC funcName = 
  MkDFunc $ \x -> (
      traceUnarrySymbolycFunc funcName x, 
      \dy -> traceUnarrySymbolycFunc (funcName <> "'") x * dy
    )

instance (TraceSymbolicFunc a, Multiplicative a, CT a ~ a) => 
  TraceSymbolicFunc (DFunc t a) where
    traceUnarrySymbolycFunc funcName fx = traceUnarrySymbolycFuncC funcName . fx


















