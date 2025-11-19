-- | Module    :  Debug.SimpleExpr.Utils.Algebra
-- Copyright   :  (C) 2025 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Inegral power type class and instances.
module Debug.SimpleExpr.Utils.Algebra
  ( MultiplicativeAction,
    (*|),
    Convolution,
    (|*|),
    AlgebraicPower,
    IntPower,
    IntegerPower,
    NaturalPower,
    FloatPower,
    DoublePower,
    (^^),
    (^),
    square,
    qube,
    splitIntoN,
    splitInto4,
  )
where

import Combinatorics (binomialSeq)
import Data.Functor (Functor (fmap))
import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import qualified Data.List as DL
import qualified Data.Stream as DS
import qualified Data.Vector as DV
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Generic.Sized as DVGS
import qualified Data.Vector.Unboxed as DVU
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Debug.SimpleExpr (SimpleExpr)
import GHC.Base (Double, Eq ((==)), Float, Maybe, otherwise, ($), (.), (>=))
import GHC.Integer (Integer)
import GHC.Natural (naturalFromInteger)
import qualified GHC.Num as GHCN
import GHC.Real (Integral, Real, fromIntegral, mod, realToFrac, toInteger)
import qualified GHC.Real
import GHC.TypeLits (Natural)
import NumHask
  ( Complex (Complex),
    Field,
    Multiplicative,
    Ring,
    Subtractive,
    negate,
    recip,
    (*),
    (-),
  )
import qualified NumHask as NH

-- | Type class for multiplicative actions.
-- This class defines a method for multiplying a value of type @b@
-- by a value of type @a@ producing a value of type @b@.
--
-- ==== __Examples__
--
-- >>> (2 :: Int) *| (3 :: Float) :: Float
-- 6.0
--
-- >>> (2 :: Int) *| (3 :: Float, 4 :: Double) :: (Float, Double)
-- (6.0,8.0)
--
-- >>> (2 :: Natural) *| [3, 4, 5] :: [Int]
-- [6,8,10]
--
-- >>> (2 :: Natural) *| Data.Vector.fromList [3, 4, 5] :: Data.Vector.Vector Int
-- [6,8,10]
--
-- >>> (2 :: Natural) *| [(3, 4), (5, 6), (7, 8)] :: [(Int, Int)]
-- [(6,8),(10,12),(14,16)]
--
-- >>> (2 :: Natural) *| Data.Vector.fromList [[3, 4], [], [5]] :: Data.Vector.Vector [Int]
-- [[6,8],[],[10]]
class MultiplicativeAction a b where
  -- | Left multiplicative action operator that preserve
  -- the type on the right hand side.
  (*|) :: a -> b -> b

-- | `Int` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Int
  where
  (*|) c = (fromIntegral c *)

-- | `Int8` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Int8
  where
  (*|) c = (fromIntegral c *)

-- | `Int16` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Int16
  where
  (*|) c = (fromIntegral c *)

-- | `Int32` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Int32
  where
  (*|) c = (fromIntegral c *)

-- | `Int64` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Int64
  where
  (*|) c = (fromIntegral c *)

-- | `Word` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Word
  where
  (*|) c = (fromIntegral c *)

-- | `Word8` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Word8
  where
  (*|) c = (fromIntegral c *)

-- | `Word16` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Word16
  where
  (*|) c = (fromIntegral c *)

-- | `Word32` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Word32
  where
  (*|) c = (fromIntegral c *)

-- | `Word64` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Word64
  where
  (*|) c = (fromIntegral c *)

-- | `Integer` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Integer
  where
  (*|) c = (fromIntegral c *)

-- | `Natural` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a Natural
  where
  (*|) c = (fromIntegral c *)

-- | `Float` instance of `MultiplicativeAction`@ a@.
instance
  (Real a) =>
  MultiplicativeAction a Float
  where
  (*|) c = (realToFrac c *)

-- | `Double` instance of `MultiplicativeAction`@ a@.
instance
  (Real a) =>
  MultiplicativeAction a Double
  where
  (*|) c = (realToFrac c *)

-- | `SimpleExpr` instance of `MultiplicativeAction`@ a@.
instance
  (Integral a) =>
  MultiplicativeAction a SimpleExpr
  where
  (*|) c = (*) (NH.fromInteger $ toInteger c)

-- | Tuple instance of `MultiplicativeAction`@ a@.
instance
  (MultiplicativeAction a b0, MultiplicativeAction a b1) =>
  MultiplicativeAction a (b0, b1)
  where
  (*|) c (x0, x1) = (c *| x0, c *| x1)

-- | Triple instance of `MultiplicativeAction`@ a@.
instance
  ( MultiplicativeAction a b0,
    MultiplicativeAction a b1,
    MultiplicativeAction a b2
  ) =>
  MultiplicativeAction a (b0, b1, b2)
  where
  (*|) c (x0, x1, x2) = (c *| x0, c *| x1, c *| x2)

-- | `Data.Functor.Identity` instance of `MultiplicativeAction`@ a@.
instance
  (MultiplicativeAction a b) =>
  MultiplicativeAction a (Identity b)
  where
  (*|) = fmap . (*|)

-- | List `[]` instance of `MultiplicativeAction`@ a@.
instance
  (MultiplicativeAction a b) =>
  MultiplicativeAction a [b]
  where
  (*|) = fmap . (*|)

-- | Boxed vector `Data.Vector.Vector` instance of `MultiplicativeAction`@ a@.
instance
  (MultiplicativeAction a b) =>
  MultiplicativeAction a (DV.Vector b)
  where
  (*|) = DV.map . (*|)

-- | Unboxed vector `Data.Vector.Unboxed.Vector` instance
-- of `MultiplicativeAction`@ a@.
instance
  (MultiplicativeAction a b) =>
  MultiplicativeAction a (DVGS.Vector DV.Vector n b)
  where
  (*|) = DVGS.map . (*|)

-- | `Data.Stream.Stream` instance of `MultiplicativeAction`@ a@.
instance
  (MultiplicativeAction a b) =>
  MultiplicativeAction a (DS.Stream b)
  where
  (*|) = DS.map . (*|)

-- | Type class for convolution operations that support nested structures.
--
-- ==== __Examples__
--
-- >>> [1,1,1] |*| [1,2,0] :: Int
-- 3
--
-- >>> ([1,1,1], Data.Vector.fromList [1, 2]) |*| ([1,2,0], Data.Vector.fromList [0, 2]) :: Int
-- 7
class Convolution a b c where
  -- | The convolution operator that combines values of type @a@ and @b@.
  (|*|) :: a -> b -> c

-- | `Int` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Int Int a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Int8` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Int8 Int8 a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Int16` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Int16 Int16 a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Int32` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Int32 Int32 a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Int64` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Int64 Int64 a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Word` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Word Word a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Word8` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Word8 Word8 a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Word16` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Word16 Word16 a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Word32` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Word32 Word32 a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Word64` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Word64 Word64 a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Integer` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Integer Integer a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Natural` instance of `Convolution`.
instance
  (GHCN.Num a) =>
  Convolution Natural Natural a
  where
  x |*| y = fromIntegral x GHCN.* fromIntegral y

-- | `Float` instance of `Convolution`.
instance
  (GHC.Real.Fractional a) =>
  Convolution Float Float a
  where
  x |*| y = realToFrac x GHCN.* realToFrac y

-- | `Double` instance of `Convolution`.
instance
  (GHC.Real.Fractional a) =>
  Convolution Double Double a
  where
  x |*| y = realToFrac x GHCN.* realToFrac y

-- | Tuple instance of `Convolution`.
instance
  (Convolution a0 b0 c, Convolution a1 b1 c, NH.Additive c) =>
  Convolution (a0, a1) (b0, b1) c
  where
  (x0, x1) |*| (y0, y1) = x0 |*| y0 NH.+ x1 |*| y1

-- | Triple instance of `Convolution`.
instance
  (Convolution a0 b0 c, Convolution a1 b1 c, Convolution a2 b2 c, NH.Additive c) =>
  Convolution (a0, a1, a2) (b0, b1, b2) c
  where
  (x0, x1, x2) |*| (y0, y1, y2) = x0 |*| y0 NH.+ x1 |*| y1 NH.+ x2 |*| y2

-- | `Data.Functor.Identity` instance of `Convolution`.
instance
  (Convolution a b c) =>
  Convolution (Identity a) (Identity b) c
  where
  (Identity x) |*| (Identity y) = x |*| y

-- | List `[]` instance of `Convolution`.
-- Shorter list is efficently treated as padded with zeros.
instance
  (Convolution a b c, NH.Additive c) =>
  Convolution [a] [b] c
  where
  lx |*| ly = DL.foldl' (NH.+) NH.zero $ DL.zipWith (|*|) lx ly

-- | Boxed vector `Data.Vector.Vector` instance of `Convolution`.
-- Smaller vector is efficently treated as padded with zeros.
instance
  (Convolution a b c, NH.Additive c) =>
  Convolution (DV.Vector a) (DV.Vector b) c
  where
  vx |*| vy = DV.foldl' (NH.+) NH.zero $ DV.zipWith (|*|) vx vy

-- | Unboxed vector `Data.Vector.Unboxed.Vector` instance of `Convolution`.
-- Smaller vector is efficently treated as padded with zeros.
instance
  (Convolution a b c, NH.Additive c) =>
  Convolution (DVGS.Vector DV.Vector n a) (DVGS.Vector DV.Vector n b) c
  where
  vx |*| vy = DVGS.foldl' (NH.+) NH.zero $ DVGS.zipWith (|*|) vx vy

-- instance
--   MultiplicativeAction a b c =>
--   MultiplicativeAction (FiniteSupportStream a) (DS.Stream b) c
--   where
--   vx |*| vy = undefined --   foldl' zero (+) $ DVGS.zipwith (|*|) lx ly
--   -- (*|) = DS.map . (*|)

-- instance
--   MultiplicativeAction a b c =>
--   MultiplicativeAction (DS.Stream a) (FiniteSupportStream b) c
--   where
--   vx |*| vy = undefined --   foldl' zero (+) $ DVGS.zipwith (|*|) lx ly

-- instance
--   MultiplicativeAction a b c =>
--   MultiplicativeAction (FiniteSupportStream a) (FiniteSupportStream b) c
--   where
--   vx |*| vy = undefined --   foldl' zero (+) $ DVGS.zipwith (|*|) lx ly

-- | Type class for power operations.
-- This class defines a method for raising a value of type @a@ to a power
-- of type @b@.
-- It is usefull to deistinguish, for example,
-- the integral power defined as a repetative multiplication
-- `(^^)` or `(^)` from the general power operation.
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, SE, simplify)
-- >>> import GHC.Base (($))
-- >>> import GHC.Real (Rational)
-- >>> import qualified NumHask
--
-- >>> x = variable "x"
--
-- >>> (^^ 2) x
-- x^2
--
-- >>> (NumHask.^^ 2) (x)
-- x*x
--
-- >>> (^^ 2) (3 :: Float)
-- 9.0
--
-- >>> (2.0 :: Double) ^^ (3.5 :: Float)
-- 11.313708498984761
--
-- For lists, vectors and other containers, the power operation is applied element-wise:
-- >>> (^^ 2) [0, 1, 2, 3] :: [Int]
-- [0,1,4,9]
class AlgebraicPower a b where
  (^^) :: b -> a -> b

-- | `Int` type alias for `AlgebraicPower`.
type IntPower a = AlgebraicPower Int a

-- | `Integer` type alias for `AlgebraicPower`.
type IntegerPower a = AlgebraicPower Integer a

-- | `Natural` type alias for `AlgebraicPower`.
type NaturalPower a = AlgebraicPower Natural a

-- | `Float` type alias for `AlgebraicPower`.
type FloatPower a = AlgebraicPower Float a

-- | `Double` type alias for `AlgebraicPower`.
type DoublePower a = AlgebraicPower Double a

-- | Infix synonym for `(^^)`.
(^) :: (AlgebraicPower Integer a) => a -> Integer -> a
(^) = (^^)

-- | Square a value.
square :: (AlgebraicPower Integer a) => a -> a
square x = x ^ 2

-- | Qube a value.
qube :: (AlgebraicPower Integer a) => a -> a
qube x = x ^ 2

-- | `Int` instance of `AlgebraicPower` typeclass.
instance
  (Integral a) =>
  AlgebraicPower a Int
  where
  x ^^ n = x GHC.Real.^ n

-- | `Int8` instance of `AlgebraicPower` typeclass.
instance
  (Integral a) =>
  AlgebraicPower a Int8
  where
  x ^^ n = x GHC.Real.^ n

-- | `Integer` instance of `AlgebraicPower` typeclass.
instance
  (Integral a) =>
  AlgebraicPower a Integer
  where
  x ^^ n = x GHC.Real.^ n

-- | `Float` instace of `AlgebraicPower` typeclass.
instance
  (Real a) =>
  AlgebraicPower a Float
  where
  x ^^ n = x NH.** realToFrac n

-- | `Double` instance of `AlgebraicPower` typeclass
-- for raising `Double` values to `Real` exponents.
instance
  (Real a) =>
  AlgebraicPower a Double
  where
  x ^^ n = x NH.** realToFrac n

-- | `AlgebraicPower` instance for raising `SimpleExpr` values to `Integer` exponents.
instance AlgebraicPower Integer SimpleExpr where
  x ^^ n = x NH.** NH.fromInteger n

-- | `Data.Functor.Identity` instance of `AlgebraicPower` typeclass.
instance
  (AlgebraicPower b a) =>
  AlgebraicPower b (Identity a)
  where
  (Identity x0) ^^ n = Identity $ x0 ^^ n

-- | `Maybe` instance of `AlgebraicPower` typeclass.
instance
  (AlgebraicPower b a) =>
  AlgebraicPower b (Maybe a)
  where
  x ^^ n = fmap (^^ n) x

-- | Generate the list of terms of the binomial expansion of
-- \( (x + y)^n \).
--
-- ==== __Examples__
--
-- >>> import Debug.SimpleExpr (variable, simplify)
--
-- >>> simplify $ biniminalList 3 (variable "x") (variable "y")
-- [y^3,3*(x*(y^2)),3*((x^2)*y),x^3]
biniminalList ::
  ( Multiplicative a,
    AlgebraicPower b a,
    MultiplicativeAction b a,
    Subtractive b,
    Integral b
  ) =>
  b ->
  a ->
  a ->
  [a]
biniminalList n x y =
  DL.zipWith
    (*|)
    (binomialSeq n)
    ([x ^^ i * y ^^ (n - i) | i <- [0 ..]])

-- | Interleave two lists.
--
-- ==== __Examples__
--
-- >>> interleave [1,3,5] [2,4,6]
-- [1,2,3,4,5,6]
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x : xs) (y : ys) = x : y : interleave xs ys

-- | Take every n-th element from a list starting from a given index.
--
-- ==== __Examples__
--
-- >>> takeEveryNth 3 0 [0..11]
-- [0,3,6,9]
takeEveryNth :: Int -> Int -> [a1] -> [a1]
takeEveryNth n start ys = [y | (y, j) <- DL.zip ys [0 ..], j `mod` n == start]

-- | Split a list into n lists by taking every n-th element.
--
-- ==== __Examples__
--
-- >>> splitIntoN 3 [0..11]
-- [[0,3,6,9],[1,4,7,10],[2,5,8,11]]
splitIntoN :: Int -> [a] -> [[a]]
splitIntoN n xs = [takeEveryNth n i xs | i <- [0 .. n - 1]]

-- | Split a list into n lists by taking every n-th element.
--
-- ==== __Examples__
--
-- >>> splitInto4 [0..11]
-- ([0,4,8],[1,5,9],[2,6,10],[3,7,11])
splitInto4 :: [a] -> ([a], [a], [a], [a])
splitInto4 xs = (takeEveryNth 4 0 xs, takeEveryNth 4 1 xs, takeEveryNth 4 2 xs, takeEveryNth 4 3 xs)

-- | Complex number `NumHask.Complex` instance of `AlgebraicPower`
-- for raising to `Natural` exponents.
instance
  (Ring a, AlgebraicPower Natural a, MultiplicativeAction Natural a) =>
  AlgebraicPower Natural (Complex a)
  where
  (Complex (x, y)) ^^ n = Complex (r, i)
    where
      r = NH.sum $ interleave split0 (fmap negate split2)
      i = NH.sum $ interleave split1 (fmap negate split3)
      (split0, split1, split2, split3) = splitInto4 (biniminalList n x y)

-- | Complex number `NumHask.Complex` instance of `AlgebraicPower`
-- for raising to `Integer` exponents.
instance
  (Field a, AlgebraicPower Natural a, MultiplicativeAction Natural a) =>
  AlgebraicPower Integer (Complex a)
  where
  x ^^ n
    | n >= 0 = x ^^ naturalFromInteger n
    | otherwise = recip $ x ^^ naturalFromInteger (-n)

-- | `AlgebraicPower` instance for raising tuples to powers.
instance
  (AlgebraicPower b a0, AlgebraicPower b a1) =>
  AlgebraicPower b (a0, a1)
  where
  (x0, x1) ^^ n = (x0 ^^ n, x1 ^^ n)

-- | `AlgebraicPower` instance for raising triples to powers.
instance
  (AlgebraicPower b a0, AlgebraicPower b a1, AlgebraicPower b a2) =>
  AlgebraicPower b (a0, a1, a2)
  where
  (x0, x1, x2) ^^ n = (x0 ^^ n, x1 ^^ n, x2 ^^ n)

-- | `AlgebraicPower` instance for raising lists to powers.
instance
  (AlgebraicPower b a) =>
  AlgebraicPower b [a]
  where
  x ^^ n = fmap (^^ n) x

-- | `AlgebraicPower` instance for raising boxed vectors to powers.
instance
  (AlgebraicPower b a) =>
  AlgebraicPower b (DV.Vector a)
  where
  x ^^ n = DV.map (^^ n) x

-- | `AlgebraicPower` instance for raising unboxed vectors to powers.
instance
  (AlgebraicPower b a, DVU.Unbox a) =>
  AlgebraicPower b (DVU.Vector a)
  where
  x ^^ n = DVU.map (^^ n) x

-- | `AlgebraicPower` instance for raising sized vectors to powers.
instance
  (AlgebraicPower b a, DVG.Vector v a) =>
  AlgebraicPower b (DVGS.Vector v n a)
  where
  x ^^ n = DVGS.map (^^ n) x

-- | `AlgebraicPower` instance for raising streams to powers.
instance
  (AlgebraicPower b a) =>
  AlgebraicPower b (DS.Stream a)
  where
  x ^^ n = DS.map (^^ n) x
