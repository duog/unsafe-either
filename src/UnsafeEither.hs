{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl -ddump-stg #-}
module UnsafeEither where

import qualified Criterion.Main as Criterion
import Unsafe.Coerce (unsafeCoerce)
import Data.List (unfoldr)
import Data.Traversable
import GHC.Base (noinline)

data SafeEither a b
  = SafeLeft a
  | SafeRight b

instance Functor (SafeEither a) where
  fmap f = \case
    SafeLeft x -> SafeLeft x
    SafeRight x -> SafeRight $ f x
  {-# NOINLINE fmap #-}

data UnsafeEither a b
  = UnsafeLeft a
  | UnsafeRight b

instance Functor (UnsafeEither a) where
  fmap f = \case
    x@UnsafeLeft {} -> unsafeCoerce x
    UnsafeRight x -> UnsafeRight (f x)
  {-# NOINLINE fmap #-}

safeMap :: (a -> b) -> UnsafeEither e a -> UnsafeEither e b
safeMap f = \case
  UnsafeLeft x -> UnsafeLeft x
  UnsafeRight x -> UnsafeRight (f x)

makeList :: (Int -> f a) -> (Int -> f a) -> Int -> [f a]
makeList odd even n = unfoldr go 0 where
  go = \case
    i | i >= n -> Nothing
      | i `mod` 2 == 1 -> Just (odd i, i + 1)
      | otherwise -> Just (even i, i + 1)
{-# NOINLINE makeList #-}

fmapList :: Functor f => [f Int] -> [f Int]
fmapList = fmap (fmap (+ 1))
{-# NOINLINE fmapList #-}

sumList :: Functor f => (f Int -> Int) -> [f Int] -> Int
sumList to_int = sum . fmap to_int
{-# NOINLINE sumList #-}

safeToInt :: SafeEither Int Int -> Int
safeToInt = \case
  SafeLeft x -> x
  SafeRight x -> x
{-# NOINLINE safeToInt #-}

unsafeToInt :: UnsafeEither Int Int -> Int
unsafeToInt = \case
  UnsafeLeft x -> x
  UnsafeRight x -> x
{-# NOINLINE unsafeToInt #-}

safeCase1 :: Int -> Int
safeCase1 n = let
  !l0 = makeList SafeLeft SafeRight n
  !l1 = fmapList l0
  in sumList safeToInt l1
{-# NOINLINE safeCase1 #-}

unsafeCase1 :: Int -> Int
unsafeCase1 n = let
  !l0 = makeList UnsafeLeft UnsafeRight n
  !l1 = fmapList l0
  in sumList unsafeToInt l1
{-# NOINLINE unsafeCase1 #-}

main :: IO ()
main = Criterion.defaultMain
  [ Criterion.bgroup "b1" $ do
      n <- [100,10000,1000000]
      let
        safe = Criterion.bench ("safe-" <> show n) $ Criterion.whnf safeCase1 n
        unsafe = Criterion.bench ("unsafe-" <> show n) $ Criterion.whnf unsafeCase1 n
      [safe, unsafe]
  ]
