{-# LANGUAGE DeriveTraversable #-}

module Debug.MutableSimplify where

import Debug.SimpleExpr (SimpleExpr, variable)
import Debug.SimpleExpr.Expr (simplifyStep, SimpleExprF(NumberF, VariableF, BinaryFuncF, SymbolicFuncF))
import Prelude (undefined, ($), (++), id, String, Double, (<$>), (<$>), Functor, Monad, mapM, sequenceA, fmap, (>>=), (=<<), Traversable, Foldable)
import NumHask ((+))
import Control.Monad.ST (runST, ST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef, STRef)
import Data.Fix (Fix(Fix), hoistFix')
import Control.Monad (liftM, join)
--import Data.Mutable (thawRef, freezeRef)



x = variable "x"
y = variable "y"

temp :: SimpleExpr
temp = x + (y + 0)

--_ = mapM
--_ = liftM
--_ = sequenceA
--_ = (>>=)

instance Foldable SimpleExprF


instance Traversable SimpleExprF



newtype SimpleExprFRef s a = SimpleExprFRef {get :: STRef s (SimpleExprF a)}

type SimpleExprRef s = Fix (SimpleExprFRef s)

thawSimpleExprF :: SimpleExprF a -> ST s (SimpleExprFRef s a)
thawSimpleExprF e = SimpleExprFRef <$> newSTRef e

freezeSimpleExprF :: SimpleExprFRef s a -> ST s (SimpleExprF a)
freezeSimpleExprF (SimpleExprFRef ref) = readSTRef ref

hoistFix :: Functor f => (forall a. f a -> g a) -> Fix f -> Fix g
hoistFix nt = go where go (Fix fFix) = Fix (nt (fmap go fFix))
                                                     -- f (Fix f)
                                            -- f (Fix g)
                                        -- g (Fix g)

hoistFixM :: (Traversable f, Monad m) => (forall a. f a -> m (g a)) -> Fix f -> m (Fix g)
hoistFixM nt = go where go (Fix fFixf) = fmap Fix (nt =<< mapM go fFixf)
                                                                   -- f (Fix f)
                                                             -- m (f (Fix g))
                                                --  m (g (Fix g))
                                         -- m (Fix (Fix g))

--hoistFix' :: Functor g => (forall a. f a -> g a) -> Fix f -> Fix g
--hoistFix' nt = go where go (Fix fFixf) = Fix (fmap go (nt fFixf))

-- fFixf  nt=>  g Fix f  go=>  g Fix g

hoistFixM' :: (Traversable g, Monad m) => (forall a. f a -> m (g a)) -> Fix f -> m (Fix g)
hoistFixM' nt = go where go (Fix fFixf) = fmap Fix (mapM go =<< nt fFixf)

-- fFixf  nt=>  m g Fix f  go=>  m m g Fix g


-- func (mga) -> m (func g a)

thawSimpleExpr :: SimpleExpr -> ST s (SimpleExprRef s)
thawSimpleExpr = hoistFixM thawSimpleExprF

freezeSimpleExpr :: SimpleExprRef s -> ST s SimpleExpr
freezeSimpleExpr = hoistFixM' freezeSimpleExprF


mSimplifyStep :: SimpleExprRef s -> ST s ()
mSimplifyStep = SimplifyStep
-- SimplifyStep :: (SimpleExpr -> SimpleExpr)

--mSimplify :: SimpleExpr -> SimpleExpr
--mSimplify e = runST $ do
--  ref <- thawSimpleExpr e
--  modifySTRef ref (simplifyStep id)
--  readSTRef ref

