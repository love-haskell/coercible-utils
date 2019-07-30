{-# language PartialTypeSignatures #-}
-- We definitely don't want to infer the types of the test functions
-- based on how they're *used*; if their implementations and (partial)
-- signatures aren't sufficient, then we don't want them to have
-- Typeable instances.
{-# language NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main) where
import Data.Monoid (Sum (..), All (..))
import Control.Newtype.Generic
import Type.Reflection

-- We don't want defaults making it look like things are working
-- if they're not!
default ()

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

main :: IO ()
main = do
  print $ pack_test0 True
  print $ pack_test1a 1
  print $ pack_test1b 2
  print $ unpack_test0 (All True)
  print $ unpack_test1a (Sum 10)
  print $ unpack_test1b (Sum 11)
  print $ ala_test1a Sum (\f -> foldMap (fmap fromInteger . f)) [1..10]
  print $ ala_test1b Sum (\f -> foldMap (fmap fromInteger . f)) [1..10]
  print $ ala_test1c Sum (\f -> foldMap (fmap fromInteger . f)) [1..10]
  print $ ala_test1d Sum (\f -> foldMap (fmap fromInteger . f)) [1..10]
  print $ ala_test0a All id True
  print $ ala_test0b All id True

  -- Make sure the types are monomorphic. This is guaranteed by the
  -- fact that the Typeable instances are resolved.
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
