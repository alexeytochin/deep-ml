{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Data.Linear where

import NumHask.Algebra.Multiplicative (Multiplicative, one, (*))
import Prelude (Float, undefined, fst, (.), Integer, (==), iterate, ($), takeWhile, (<), zip, fmap, const, snd)
import NumHask.Algebra.Ring (Distributive)
import NumHask.Algebra.Additive (zero, Additive, (+), (-))
import GHC.Natural (Natural)
import Control.Monad (void)


instance (Multiplicative a, Multiplicative b) =>
  Multiplicative (a, b)  where
    (a, b) * (c, d) = (a * c, b * d)
    one = (one, one)

instance (Additive a, Additive b) =>
  Additive (a, b)  where
    (a, b) + (c, d) = (a + c, b + d)
    zero = (zero, zero)

instance (Additive a, Additive b, Additive c) =>
  Additive (a, b, c)  where
    (a, b, c) + (d, e, f) = (a + d, b + e, c + f)
    zero = (zero, zero, zero)

unitDual :: (Multiplicative a) =>
  (a -> b) -> b
unitDual f = f one

tupleDual :: (Multiplicative a1, Additive a1, Multiplicative a2, Additive a2) =>
  ((a1, a2) -> b) -> (b, b)
tupleDual f = (f (one, zero), f (zero, one))

-- ((a1, a2) -> (b, b, b)) -> ((b, b, b), (b, b, b))

-- (((a00, a01, a02), (a10, a11, a12)) -> b) -> ((a1, a2) -> (b, b, b))

-- (((a00, a01, a02), (a10, a11, a12)) -> b) -> ((b, b, b), (b, b, b))

tripleDual :: (Multiplicative a0, Additive a0, Multiplicative a1, Additive a1, Multiplicative a2, Additive a2) =>
  ((a0, a1, a2) -> b) -> (b, b, b)
tripleDual f = (f (one, zero, zero), f (zero, one, zero), f (zero, zero, one))

tupleTripleDual :: forall a00 a01 a02 a10 a11 a12 b. (
    Multiplicative a00, Additive a00, 
    Multiplicative a01, Additive a01, 
    Multiplicative a02, Additive a02, 
    Multiplicative a10, Additive a10, 
    Multiplicative a11, Additive a11, 
    Multiplicative a12, Additive a12 
  ) =>
    (((a00, a01, a02), (a10, a11, a12)) -> b) -> ((b, b, b), (b, b, b))
tupleTripleDual f = (
    tripleDual (\a0 -> f (a0, (zero, zero, zero))), 
    tripleDual (\a1 -> f ((zero, zero, zero), a1))
  )

--listDual :: (Multiplicative a, Additive a) =>
--  Natural -> ((a', [a]) -> b) -> [b]
--listDual length f = [f [if i == j then one else zero | j <- range] | i <- range] where
--  range = takeWhile (<length) $ iterate (+1) 0
listDual :: (Multiplicative a, Additive a) =>
  ([()], [a] -> b) -> [b]
listDual (l, f) = [unitDual (\x -> f (basisList x i l)) | i <- counter l] 
--  basisList_ i = [if i == j then one else zero | (j, _) <- enumerate l]
  -- enumerate = zip [0..]

listTupleDual :: (Multiplicative a1, Additive a1, Multiplicative a2, Additive a2) =>
  ([()], [(a1, a2)] -> b) -> [(b, b)]
listTupleDual (l, f) = [tupleDual (\x -> f (basisList x i l)) | i <- counter l]
--listTupleDual (l, f) = [(f (basisList (one, zero) i l), f (basisList (zero, one) i l)) | (i, _) <- enumerate] where
--  basisList1 i = [if i == j then (one, zero) else zero | (j, _) <- enumerate]
--  basisList2 i = [if i == j then (zero, one) else zero | (j, _) <- enumerate]
  -- enumerate = enumerateList l

tupleListDual :: forall a1 a2 b. (Multiplicative a1, Additive a1, Multiplicative a2, Additive a2) =>
  (([()], [()]), ([a1], [a2]) -> b) -> ([b], [b])
tupleListDual ((l1', l2'), f) = (
    listDual (l1', \l1 -> f (l1, fmap (const zero) l2')), 
    listDual (l2', \l2 -> f (fmap (const zero) l1', l2))
  )
--  (
--    listDual (fst l, \l1 -> f (l1, fmap (const zero) (snd l))), 
--    listDual (snd l, \l2 -> f (fmap (const zero) (fst l), l2)), 
--  )


enumlist :: [a] -> [()]
enumlist = void

counter :: [a] -> [Natural]
counter l = fmap fst (zip [(0 :: Natural) ..] l)

basisList :: (Additive a) => a -> Natural -> [b] -> [a]
basisList a i l = [if i == j then a else zero | (j, _) <- enumerateList l]

enumerateList :: [a] -> [(Natural, a)]
enumerateList = zip [(0 :: Natural) ..]

--class RealOp a b c where
--  basis :: (a -> b) -> c

--instance Multiplicative r =>
--  Dual r r r where
--    basis f = f one

--instance Dual a Float a where
--    basis f = f one

--type ListOfList a = [[a]]
--
--instance RealOp Float Float Float where
--    basis f = f one
--
--instance (RealOp r r r, Additive r) =>
--  RealOp (r, r) r (r, r) where
--    basis f = (basis (\x -> f (x, zero)), basis (\x -> f (zero, x)))
--
--instance (RealOp r r r, Additive r) =>
--  RealOp (r, r, r) r (r, r, r) where
--    basis f = (basis (\x -> f (x, zero, zero)), basis (\x -> f (zero, x, zero)), basis (\x -> f (zero, zero, x)))
--
--instance (RealOp r r r, Additive r) =>
--  RealOp [r] r [r] where
--    basis f = undefined --  [basis (\x -> f (x, zero)), basis (\x -> f (zero, x)))
--
--instance (RealOp a r b, Additive r) =>
--  RealOp a (r, r) (b, b) where
--    basis f = undefined --  (basis (\x -> f (x, zero)), basis (\x -> f (zero, x)))

--instance RealOp Float [] [] ListOfList where
--    basis ar = f one

--instance Dual (a, b, c) Float (a, b, c) where
--  basis f = f one

--instance (RealOp r a a', RealOp r b b', Additive a, Additive b) =>
--  RealOp r (a, b) (a', b') where
--    basis f = (basis (\x -> f (x, zero)), basis (\x -> f (zero, x)))
--
--instance (RealOp r a a', RealOp r b b', RealOp r c c', Additive a, Additive b, Additive c) =>
--  RealOp r (a, b, c) (a', b', c') where
--    basis f = (basis (\x -> f (x, zero, zero)), basis (\x -> f (zero, x, zero)), basis (\x -> f (zero, zero, x)))
--
--instance (RealOp r a a', Additive a) =>
--  RealOp r [a] [a'] where
--    basis f = [undefined]
--
--third :: (a, b, c) -> c
--third (_, _, c) = c
--
--example1 = basis (fst :: (Float, Float) -> Float) :: (Float, Float)
--example2 = basis (third . fst :: ((Float, Float, Float), (Float, Float, Float)) -> Float) :: ((Float, Float, Float), (Float, Float, Float))

--f3 :: (Float, Float, Float) -> (Float, Float)
--f3 = undefined
--example3 = basis f3 :: ((Float, Float), (Float, Float), (Float, Float)) -- :: ((Float, Float, Float), (Float, Float, Float))


--basisBasis :: (Multiplicative a) =>
--  (a -> r) -> r
--basisBasis f = f one
--
--tupleBasis :: (Distributive a, Distributive b) =>
--  ((a, b) -> r) -> (r, r)
--tupleBasis f = (f (one, zero), f (zero, one))
--
--tuple3Basis :: (Distributive a, Distributive b, Distributive c) =>
--  ((a, b, c) -> r) -> (r, r, r)
--tuple3Basis f = (f (one, zero, zero), f (zero, one, zero), f (zero, zero, one))

--tuple23Basis :: (Distributive a, Distributive b, Distributive c) =>
--  (((a1, b1, c1), (a2, b2, c2)) -> r) -> ((r, r, r), (r, r, r))
--tuple23Basis f = (tuple3Basis (\x -> f (x, zero)), tuple3Basis (\x -> f (zero, x)))


--listBasis :: (Distributive a) =>
--  (Stream a -> r