{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}


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
import NumHask (Additive, (+) , Distributive, Divisive, ExpField, Multiplicative, Subtractive, TrigField, fromInteger, zero, 
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
import Numeric.InfBackprop.Diff (Diff(MkDiff))



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

--unarrySymbolycFuncC :: (SymbolicFunc a, Multiplicative a, CT a ~ a) =>
--  String -> Diff t a
--unarrySymbolycFuncC funcName = 
--  MkDFunc $ \x -> (
--      unarrySymbolycFunc funcName x, 
--      \dy -> unarrySymbolycFunc (funcName <> "'") x * dy
--    )

instance (SymbolicFunc a, Multiplicative a, CT a ~ a) => 
  SymbolicFunc (Diff t a) where
    unarrySymbolycFunc :: String -> Diff t a -> Diff t a
  --    unarrySymbolycFunc funcName fx = unarrySymbolycFuncC funcName . fx
    unarrySymbolycFunc funcName (MkDiff x bp h) =
      MkDiff
      (unarrySymbolycFunc funcName x)
      (\(f'_, h_) cy -> bp h_ $ f'_ * cy)
      (unarrySymbolycFunc (funcName <> "'") x, h)


class BinarySymbolicFunc a where
  binarySymbolycFunc :: String -> a -> a -> a

instance BinarySymbolicFunc SimpleExpr where
  binarySymbolycFunc = twoArgFunc

instance (BinarySymbolicFunc a, Distributive a, Additive (CT t), CT a ~ a) =>
  BinarySymbolicFunc (Diff t a) where -- (DFunc a a) where  -- ( (T a) a (T a) a) where
--    binarySymbolycFunc funcName fx fy = binnarySymbolycFuncC funcName . tupleToLens (fx, fy)
    binarySymbolycFunc funcName (MkDiff x bpx hx) (MkDiff y bpy hy) = MkDiff
      (binarySymbolycFunc funcName x y)
      (\(f'1, f'2, hx_, hy_) cz -> bpx hx_ (f'1 * cz) + bpy hy_ (f'2 * cz))
      (binarySymbolycFunc (funcName <> "'_1") x y, binarySymbolycFunc (funcName <> "'_2") x y, hx, hy)


class TraceSymbolicFunc a where
  traceUnarrySymbolycFunc :: String -> a -> a

instance TraceSymbolicFunc SimpleExpr where
  traceUnarrySymbolycFunc name x = 
    trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " >>>") $ 
      unaryFunc name x

instance (TraceSymbolicFunc a, Multiplicative a, CT a ~ a) => 
  TraceSymbolicFunc (Diff t a) where
    traceUnarrySymbolycFunc :: String -> Diff t a -> Diff t a
    traceUnarrySymbolycFunc funcName (MkDiff x bp h) = MkDiff
      (traceUnarrySymbolycFunc funcName x)
      (\(f'_, h_) cy -> bp h_ $ f'_ * cy)
      (traceUnarrySymbolycFunc (funcName <> "'") x, h)


class TraceBinarySymbolicFunc a where
  traceBinarySymbolycFunc :: String -> a -> a -> a

instance TraceBinarySymbolicFunc SimpleExpr where
  traceBinarySymbolycFunc name x y =
    trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " and " <> show y <> " >>>") $
      Fix (SymbolicFuncF name [x, y])

instance (TraceBinarySymbolicFunc a, Distributive a, Additive (CT t), CT a ~ a) =>
  TraceBinarySymbolicFunc (Diff t a) where
    traceBinarySymbolycFunc funcName (MkDiff x bpx hx) (MkDiff y bpy hy) = MkDiff
      (traceBinarySymbolycFunc funcName x y)
      (\(f'1, f'2, hx_, hy_) cz -> bpx hx_ (f'1 * cz) + bpy hy_ (f'2 * cz))
      (traceBinarySymbolycFunc (funcName <> "'_1") x y, traceBinarySymbolycFunc (funcName <> "'_2") x y, hx, hy)















