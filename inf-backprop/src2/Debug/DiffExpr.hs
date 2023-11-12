{-# LANGUAGE TypeOperators #-}

module Debug.DiffExpr where

import Control.Arrow (Kleisli (Kleisli))
import Control.CatBifunctor (first, second, (***))
import Control.Category ((.))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Text (pack)
import Debug.SimpleExpr.Expr (SimpleExpr, unaryFunc, binaryFunc, number, variable, simplify, content, 
    SimpleExprF(SymbolicFuncF)
  )
--import InfBackprop.Common6 (
--  D(D), const_, lensCtoP, derivative, addC, multC, derivative1, crossC, (%), derivative0,
--  (.*.), merge
--  )
import IsomorphismClass.Isomorphism (iso)
import NumHask (Additive, Distributive, Divisive, ExpField, Multiplicative, Subtractive, TrigField, fromInteger, zero, 
  (*), MultiplicativeAction, (^^), Integral)
import qualified NumHask as NH
import qualified NumHask.Prelude as NHP
--import qualified Prelude.InfBackprop
import Prelude (Monad, Show, String, pure, return, show, ($), (<>), fmap, uncurry, curry, undefined, Ord)
import qualified Prelude as P
import Debug.SimpleExpr (Expr)
import InfBackprop.LensD (DFunc, LensD(LensD), tupleToLens, lensCtoP, IntegralPower, integralPow)
import InfBackprop.Tangent (T)
import Debug.Trace (trace)
import Data.Fix (Fix (Fix, unFix))
--import Debug.Hood.Observe (observe)
import InfBackprop.Tangent (Tangent, T)



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

unarrySymbolycFuncC :: (SymbolicFunc a, Multiplicative a, T a ~ a) =>
  String -> DFunc a a
unarrySymbolycFuncC funcName = 
  LensD $ \x -> (
      unarrySymbolycFunc funcName x, 
      \dy -> unarrySymbolycFunc (funcName <> "'") x * dy
    )

instance (SymbolicFunc a, Multiplicative a, T a ~ a) => 
  SymbolicFunc (LensD dt t a a) where -- (DFunc a a) where  -- ( (T a) a (T a) a) where
    unarrySymbolycFunc funcName = lensCtoP (unarrySymbolycFuncC funcName)


class BinnarySymbolicFunc a where
  binarySymbolycFunc :: String -> a -> a -> a

instance BinnarySymbolicFunc SimpleExpr where
  binarySymbolycFunc = twoArgFunc
--  binarySymbolycFunc name x y =
--    trace ("\n:> Calculating " <> name <> " of " <> show x <> " and " <> show y) $ 
--      twoArgFunc name x y

binarySymbolycFuncC :: (BinnarySymbolicFunc a, Multiplicative a, T a ~ a) => 
  String -> DFunc (a, a) a
binarySymbolycFuncC funcName = 
  LensD $ \(x, y) -> (
      binarySymbolycFunc funcName x y,
      \dy -> (binarySymbolycFunc (funcName <> "_1") x y * dy, binarySymbolycFunc (funcName <> "_2") x y * dy)
    )

binarySymbolycLens :: (BinnarySymbolicFunc a, Multiplicative da, a ~ da) =>
  String -> LensD dt t (da, da) (a, a) -> LensD dt t da a
binarySymbolycLens funcName (LensD a) = LensD $ \t -> let
    ((x1, x2), dyx) = a t
  in (
      binarySymbolycFunc funcName x1 x2,
      \dy -> dyx (binarySymbolycFunc (funcName <> "'_1") x1 x2 * dy, binarySymbolycFunc (funcName <> "'_2") x1 x2 * dy)
    )

instance (Additive dt, BinnarySymbolicFunc a, Distributive a, T a ~ a) =>
  BinnarySymbolicFunc (LensD dt t a a) where -- (DFunc a a) where  -- ( (T a) a (T a) a) where
    binarySymbolycFunc funcName = curry $ binarySymbolycLens funcName . tupleToLens

--    binarySymbolycFunc funcName = lensCtoP (binarySymbolycFuncC funcName)























