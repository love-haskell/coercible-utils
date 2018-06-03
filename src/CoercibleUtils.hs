module CoercibleUtils where

import Data.Coerce

op :: Coercible a b
   => (a -> b)
   -> b
   -> a
op = coerce

ala :: (Coercible a b, Coercible a' b')
    => (a -> b)
    -> ((a -> b) -> c -> b')
    -> c
    -> a'
ala pa hof = ala' pa hof id

ala' :: (Coercible a b, Coercible a' b')
     => (a -> b)
     -> ((d -> b) -> c -> b')
     -> (d -> a)
     -> c
     -> a'
ala' _ hof f = coerce . hof (coerce f)

under :: (Coercible a b, Coercible a' b')
      => (a -> b)
      -> (b -> b')
      -> a
      -> a'
under _ f = coerce f

over :: (Coercible a b, Coercible a' b')
     => (a -> b)
     -> (a -> a')
     -> b
     -> b'
over _ f = coerce f

under2 :: (Coercible a b, Coercible a' b')
       => (a -> b)
       -> (b -> b -> b')
       -> a
       -> a
       -> a'
under2 _ f = coerce f

over2 :: (Coercible a b, Coercible a' b')
      => (a -> b)
      -> (a -> a -> a')
      -> b
      -> b
      -> b'
over2 _ f = coerce f

underF :: (Coercible a b, Coercible a' b', Functor f, Functor g)
       => (a -> b)
       -> (f b -> g b')
       -> f a
       -> g a'
underF _ f = fmap coerce . f . fmap coerce

overF :: (Coercible a b, Coercible a' b', Functor f, Functor g)
      => (a -> b)
      -> (f a -> g a')
      -> f b
      -> g b'
overF _ f = fmap coerce . f . fmap coerce
