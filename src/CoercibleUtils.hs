module CoercibleUtils where

import Data.Coerce

-- | Coercive left-composition.
infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
{-# INLINE (#.) #-}

-- | Coercive right-composition.
infixr 9 .#
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
{-# INLINE (.#) #-}

op :: Coercible a b
   => (a -> b)
   -> b
   -> a
op = coerce
{-# INLINE op #-}

ala :: (Coercible a b, Coercible a' b')
    => (a -> b)
    -> ((a -> b) -> c -> b')
    -> c
    -> a'
ala pa hof = ala' pa hof id
{-# INLINE ala #-}

ala' :: (Coercible a b, Coercible a' b')
     => (a -> b)
     -> ((d -> b) -> c -> b')
     -> (d -> a)
     -> c
     -> a'
ala' _ hof f = coerce #. hof (coerce f)
{-# INLINE ala' #-}

under :: (Coercible a b, Coercible a' b')
      => (a -> b)
      -> (b -> b')
      -> a
      -> a'
under _ f = coerce f
{-# INLINE under #-}

over :: (Coercible a b, Coercible a' b')
     => (a -> b)
     -> (a -> a')
     -> b
     -> b'
over _ f = coerce f
{-# INLINE over #-}

under2 :: (Coercible a b, Coercible a' b')
       => (a -> b)
       -> (b -> b -> b')
       -> a
       -> a
       -> a'
under2 _ f = coerce f
{-# INLINE under2 #-}

over2 :: (Coercible a b, Coercible a' b')
      => (a -> b)
      -> (a -> a -> a')
      -> b
      -> b
      -> b'
over2 _ f = coerce f
{-# INLINE over2 #-}

underF :: (Coercible a b, Coercible a' b', Functor f, Functor g)
       => (a -> b)
       -> (f b -> g b')
       -> f a
       -> g a'
underF _ f = fmap coerce . f . fmap coerce
{-# INLINE underF #-}

overF :: (Coercible a b, Coercible a' b', Functor f, Functor g)
      => (a -> b)
      -> (f a -> g a')
      -> f b
      -> g b'
overF _ f = fmap coerce . f . fmap coerce
{-# INLINE overF #-}

