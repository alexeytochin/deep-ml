{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-export-lists #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Debug.DiffExpr where

import Data.Type.Equality (type (~))
import GHC.Base (Eq)
import Data.Hashable (Hashable)
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
import Numeric.InfBackprop.BackpropDiff (BackpropDiff(MkBackpropDiff))
import Control.ExtendableMap (ExtandableMap, extendMap)
import Debug.Traced (Traced(MkTraced))


twoArgFunc :: String -> SimpleExpr -> SimpleExpr -> SimpleExpr
twoArgFunc name x y = Fix (SymbolicFuncF name [x, y])


class SymbolicFunc a where
  unarrySymbolicFunc :: String -> a -> a
  -- binnarySymbolycFunc :: String -> a -> a -> a

--instance (SimpleExpr a, SimpleExpr b) => SymbolicFunc (a, b) where
--  unarrySymbolycFunc :: String -> (a, b) -> (a, b)
  

instance SymbolicFunc SimpleExpr where
  unarrySymbolicFunc = unaryFunc
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
  SymbolicFunc (BackpropDiff t a) where
    unarrySymbolicFunc :: String -> BackpropDiff t a -> BackpropDiff t a
  --    unarrySymbolycFunc funcName fx = unarrySymbolycFuncC funcName . fx
--    unarrySymbolicFunc funcName (MkBackpropDiff x bp h ) =
--      MkBackpropDiff
--      (unarrySymbolicFunc funcName x)
--      (\(f'_, h_) cy -> bp h_ $ f'_ * cy)
--      (unarrySymbolicFunc (funcName <> "'") x, h)
--      (\h_ cy -> bp h_ $ (unarrySymbolicFunc (funcName <> "'") x) * cy)
--      h
--      (\h_ cy -> bp h_ $ f' * cy)
--      h
--      where
--        f' = unarrySymbolicFunc (funcName <> "'") x
    unarrySymbolicFunc funcName (MkBackpropDiff x bp) =
      MkBackpropDiff
      (unarrySymbolicFunc funcName x)
      (\cy -> bp $ f' * cy)
      where
        f' = unarrySymbolicFunc (funcName <> "'") x

class BinarySymbolicFunc a where
  binarySymbolicFunc :: String -> a -> a -> a

instance BinarySymbolicFunc SimpleExpr where
  binarySymbolicFunc = twoArgFunc

instance (BinarySymbolicFunc a, Distributive a, Additive (CT t), CT a ~ a) =>
  BinarySymbolicFunc (BackpropDiff t a) where -- (DFunc a a) where  -- ( (T a) a (T a) a) where
--    binarySymbolycFunc funcName fx fy = binnarySymbolycFuncC funcName . tupleToLens (fx, fy)
--    binarySymbolicFunc funcName (MkBackpropDiff x bpx hx) (MkBackpropDiff y bpy hy) = MkBackpropDiff
--      (binarySymbolicFunc funcName x y)
--      (\(f'1, f'2, hx_, hy_) cz -> bpx hx_ (f'1 * cz) + bpy hy_ (f'2 * cz))
--      (binarySymbolicFunc (funcName <> "'_1") x y, binarySymbolicFunc (funcName <> "'_2") x y, hx, hy)
    binarySymbolicFunc funcName (MkBackpropDiff x bpx) (MkBackpropDiff y bpy) = MkBackpropDiff
      (binarySymbolicFunc funcName x y)
      (\cz -> bpx (f'1 * cz) + bpy (f'2 * cz)) where
        f'1 = binarySymbolicFunc (funcName <> "'_1") x y
        f'2 = binarySymbolicFunc (funcName <> "'_2") x y


type TracedSimpleExpr = Traced SimpleExpr
type TSE = TracedSimpleExpr


instance (SymbolicFunc a, Show a) =>
  SymbolicFunc (Traced a) where
    unarrySymbolicFunc name (MkTraced x) =
      trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " >>>") $
        MkTraced $ unarrySymbolicFunc name x

instance (BinarySymbolicFunc a, Show a) =>
  BinarySymbolicFunc (Traced a) where
    binarySymbolicFunc name (MkTraced x) (MkTraced y) =
      trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " and " <> show y <> " >>>") $
        MkTraced $ binarySymbolicFunc name x y

instance ExtandableMap a b c d => ExtandableMap a b (Traced c) (Traced d) where
  extendMap f (MkTraced x) = MkTraced $ extendMap f x

--tracedSimplify :: ExtandableMap SimpleExpr SimpleExpr a b => a -> b
--tracedSimplify = extendMap simplifyExpr

class TraceSymbolicFunc a where
  traceUnarrySymbolicFunc :: String -> a -> a

instance TraceSymbolicFunc SimpleExpr where
  traceUnarrySymbolicFunc name x =
    trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " >>>") $ 
      unaryFunc name x

instance (TraceSymbolicFunc a, Multiplicative a, CT a ~ a) => 
  TraceSymbolicFunc (BackpropDiff t a) where
    traceUnarrySymbolicFunc :: String -> BackpropDiff t a -> BackpropDiff t a
--    traceUnarrySymbolicFunc funcName (MkBackpropDiff x bp h) = MkBackpropDiff
--      (traceUnarrySymbolicFunc funcName x)
--      (\(f'_, h_) cy -> bp h_ $ f'_ * cy)
--      (traceUnarrySymbolicFunc (funcName <> "'") x, h)
    traceUnarrySymbolicFunc funcName (MkBackpropDiff x bp) = MkBackpropDiff
      (traceUnarrySymbolicFunc funcName x)
      (\cy -> bp $ f' * cy) where
        f' = traceUnarrySymbolicFunc (funcName <> "'") x


class TraceBinarySymbolicFunc a where
  traceBinarySymbolicFunc :: String -> a -> a -> a

instance TraceBinarySymbolicFunc SimpleExpr where
  traceBinarySymbolicFunc name x y =
    trace (" <<< TRACING: Calculating " <> name <> " of " <> show x <> " and " <> show y <> " >>>") $
      Fix (SymbolicFuncF name [x, y])

instance (TraceBinarySymbolicFunc a, Distributive a, Additive (CT t), CT a ~ a) =>
  TraceBinarySymbolicFunc (BackpropDiff t a) where
--    traceBinarySymbolicFunc funcName (MkBackpropDiff x bpx hx) (MkBackpropDiff y bpy hy) = MkBackpropDiff
--      (traceBinarySymbolicFunc funcName x y)
--      (\(f'1, f'2, hx_, hy_) cz -> bpx hx_ (f'1 * cz) + bpy hy_ (f'2 * cz))
--      (traceBinarySymbolicFunc (funcName <> "'_1") x y, traceBinarySymbolicFunc (funcName <> "'_2") x y, hx, hy)
    traceBinarySymbolicFunc funcName (MkBackpropDiff x bpx) (MkBackpropDiff y bpy) = MkBackpropDiff
      (traceBinarySymbolicFunc funcName x y)
      (\cz -> bpx (f'1 * cz) + bpy (f'2 * cz)) where
        f'1 = traceBinarySymbolicFunc (funcName <> "'_1") x y
        f'2 = traceBinarySymbolicFunc (funcName <> "'_2") x y















