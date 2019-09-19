{-# language PartialTypeSignatures #-}
-- We definitely don't want to infer the types of the test functions
-- based on how they're *used*; if their implementations and (partial)
-- signatures aren't sufficient, then we don't want them to have
-- Typeable instances.
{-# language NoMonomorphismRestriction #-}
{-# language GADTs #-}
{-# language ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main
  ( main
  -- Exported to avoid unused warnings
  , trans
  , refl
  , sym
  ) where
import Data.Monoid (Sum (..), All (..))
import CoercibleUtils.Newtype
import Type.Reflection

-- We don't want defaults making it look like things are working
-- if they're not!
default ()

-- Copied from the constraints package
data Dict c where
  Dict :: c => Dict c

trans :: (Similar a b, Similar b c) => proxy b -> Dict (Similar a c)
trans _ = Dict

-- We don't get unqualified reflexivity, but we get something really
-- close: if a type is similar to *some* other type, then it's similar
-- to itself.
refl :: Similar a b => proxy b -> Dict (Similar a a)
refl _ = Dict

sym :: Similar a b => Dict (Similar b a)
sym = Dict

pack_test0 :: _ -> All
pack_test0 = pack

pack_test1a :: _ -> Sum Int
pack_test1a = pack

pack_test1b :: Int -> Sum _
pack_test1b = pack

unpack_test0 :: All -> _
unpack_test0 = unpack

unpack_test1a :: Sum Int -> _
unpack_test1a = unpack

unpack_test1b :: Sum _ -> Int
unpack_test1b = unpack

-- Fix n
ala_test0a
 :: (_ -> All) -> (_ -> Bool -> _) -> Bool -> _
ala_test0a = ala

-- Fix n'
ala_test0b
 :: (_ -> _) -> (_ -> Bool -> All) -> (Bool -> _)
ala_test0b = ala

-- Fix o and o', partially fix n
ala_test1a :: (Integer -> Sum _) -> (_ -> [Integer] -> _) -> [Integer] -> Word
ala_test1a = ala
    --  => (o `to` n) -> ((o -> n) -> b -> n') -> (b -> o')

-- Fix o', partially fix n, and partially fix n'
ala_test1b :: (_ -> Sum _) -> ((_ -> _ Integer) -> [Integer] -> _) -> [Integer] -> Word
ala_test1b = ala

-- Fix o and n'; infer o' and n
ala_test1c :: (Integer -> _) -> ((_ -> _) -> [Integer] -> Sum Word) -> [Integer] -> _
ala_test1c = ala

-- Fix o' and n; infer o and n'
ala_test1d :: (_ -> Sum Integer) -> (_ -> [Integer] -> _) -> [Integer] -> Word
ala_test1d = ala

under_test0a :: (_ -> All) -> _ -> _ -> _
under_test0a = under

under_test0b :: (_ -> _) -> (_ -> All) -> _ -> _
under_test0b = under

over_test0a :: (_ -> All) -> _ -> _ -> _
over_test0a = over

over_test0b :: (_ -> _) -> _ -> _ -> All
over_test0b = over

main :: IO ()
main = do
  print $ (pack True :: All)
  print $ unpack (All True)
  print $ ala Sum foldMap [1..10 :: Int]
  print $ under Sum (fmap (+1)) (3 :: Int)
  print $ over Sum (+3) (Sum (3 :: Int))

  -- Make sure the types are monomorphic. This is guaranteed by the
  -- fact that the Typeable instances are resolved. Also make sure these
  -- functions have the expected types!
  print $ typeOf pack_test0
  print $ typeOf pack_test1a
  print $ typeOf pack_test1b
  print $ typeOf unpack_test0
  print $ typeOf unpack_test1a
  print $ typeOf unpack_test1b

  print $ typeOf ala_test0a
  print $ typeOf ala_test0b
  print $ typeOf ala_test1a
  print $ typeOf ala_test1b
  print $ typeOf ala_test1c
  print $ typeOf ala_test1d

  print $ typeOf under_test0a
  print $ typeOf under_test0b

  print $ typeOf over_test0a
  print $ typeOf over_test0b
