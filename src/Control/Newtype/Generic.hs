{-# language FlexibleContexts           #-}
{-# language FlexibleInstances          #-}
{-# language MultiParamTypeClasses      #-}
{-# language ScopedTypeVariables        #-}
{-# language TypeFamilies               #-}
{-# language TypeOperators              #-}
{-# language DataKinds                  #-}
{-# language UndecidableInstances       #-}
{-# language ConstraintKinds            #-}
{-# language PolyKinds                  #-}

{- |
A version of the 'Newtype' typeclass and related functions.
Primarily pulled from Conor McBride's Epigram work. Some examples:

>>> ala Sum foldMap [1,2,3,4]
10

>>> ala Endo foldMap [(+1), (+2), (subtract 1), (*2)] 3
8

>>> under2 Min (<>) 2 1
1

>>> over All not (All False)
All {getAll = True)

The version of the 'Newtype' class exported by this module has an instance for
*all* and *only* newtypes with 'Generic' instances whose generated
'Coercible' instances are visible. Users need not, and probably should not, write
their own instances.

Like McBride's version, and unlike the one in @newtype-generics@,
this version has two parameters: one for the newtype and one for the
underlying type. This generally makes 'Newtype' constraints more compact
and easier to read.

Unlike the versions in "CoercibleUtils", one of the "packers" must
operate on only one newtype layer at a time; this improves inference at
the expense of a little flexibility.

Note: Each function in this module takes an argument representing a
newtype constructor (or similar). This is used only for its type. To make that clear,
the type of that argument is allowed to be extremely polymorphic: @o \`to\` n@
rather than @o -> n@.

@since TODO
-}
module Control.Newtype.Generic
  ( Newtype
  , IsNewtype
  , HasUnderlying
  , O
  , Similar
  , pack
  , unpack
  , op
  , ala
  , ala'
  , under
  , over
  , under2
  , over2
  , underF
  , overF
  ) where

import GHC.Generics
import Data.Coerce
import GHC.TypeLits (TypeError, ErrorMessage (..))
import CoercibleUtils (op, (#.), (.#))
import Data.Kind (Constraint)

-- | Get the underlying type of a newtype.
--
-- @
-- data N = N Int deriving Generic
-- -- O N = Int
-- @
type O x = GO x (Rep x)

type family GO x rep where
  GO _x (D1 ('MetaData _n _m _p 'True) (C1 _c (S1 _s (K1 _i a)))) = a
  GO x _rep = TypeError
    ('Text "There is no " ':<>: 'ShowType Newtype ':<>: 'Text " instance for"
      ':$$: 'Text "    " ':<>: 'ShowType x
      ':$$: 'Text "because it is not a newtype.")

-- | @Newtype n o@ means that @n@ is a newtype wrapper around
-- @o@. @n@ must be an instance of 'Generic'. Furthermore, the
-- @'Coercible' n o@ instance must be visible; this typically
-- means the newtype constructor is visible, but the instance
-- could also have been brought into view by pattern matching
-- on a 'Data.Type.Coercion.Coercion'.
class Coercible n o => Newtype (n :: k) (o :: k)

-- The Generic n constraint gives a much better type error if
-- n is not an instance of Generic. Without that, there's
-- just a mysterious message involving GO and Rep. With it, the
-- lousy error message still shows up, but at least there's
-- also a good one.
instance (Generic n, Coercible n o, O n ~ o) => Newtype n o

-- | A single-parameter version of 'Newtype', similar to the
-- @Newtype@ class in @newtype-generics@.
class Newtype n (O n) => IsNewtype n
instance Newtype n (O n) => IsNewtype n

-- | A version of 'Newtype' with the parameters flipped, for
-- partial application.
class Newtype n o => HasUnderlying o n
instance Newtype n o => HasUnderlying o n

-- | Two types are @Similar@ if they are built from
-- the same type constructor and the same kind arguments.
--
-- @Sum Int@ and @Sum Bool@ are @Similar@.
--
-- @Sum Int@ and @Product Int@ are not @Similar@
-- because they are built from different type constructors.
--
-- @Const Int Char@ and @Const Int Maybe@ are not @Similar@
-- because they have different kind arguments.
class Similar (n :: k) (n' :: k)
instance (Sim n n', Sim n' n) => Similar n n'

type family GetArg (x :: j) :: k where
  GetArg (_ a) = a

type family GetFun (x :: j) :: k where
  GetFun (f _) = f

type family Sim (n :: k) (n' :: k) :: Constraint where
  Sim (f (_ :: j)) n' = (Sim f (GetFun n'), n' ~ GetFun n' (GetArg n' :: j))
  Sim f g = f ~ g

-- | Wrap a value with a newtype constructor.
pack :: Newtype n o => o -> n
pack = coerce

-- | Unwrap a newtype constructor from a value.
unpack :: Newtype n o => n -> o
unpack = coerce


-- | The workhorse of the package. Given a "packer" and a \"higher order function\" (/hof/),
-- it handles the packing and unpacking, and just sends you back a regular old
-- function, with the type varying based on the /hof/ you passed.
--
-- The reason for the signature of the /hof/ is due to 'ala' not caring about structure.
-- To illustrate why this is important, consider this alternative implementation of 'under2':
--
-- > under2 :: (Newtype n o, Newtype n' o')
-- >        => (o -> n) -> (n -> n -> n') -> (o -> o -> o')
-- > under2' pa f o0 o1 = ala pa (\p -> uncurry f . bimap p p) (o0, o1)
--
-- Being handed the "packer", the /hof/ may apply it in any structure of its choosing –
-- in this case a tuple.
--
-- >>> ala Sum foldMap [1,2,3,4]
-- 10
ala :: (Coercible n o, Newtype n' o', Similar n n')
    => (o `to` n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof = ala' pa hof id

-- | This is the original function seen in Conor McBride's work.
-- The way it differs from the 'ala' function in this package,
-- is that it provides an extra hook into the \"packer\" passed to the hof.
-- However, this normally ends up being @id@, so 'ala' wraps this function and
-- passes @id@ as the final parameter by default.
-- If you want the convenience of being able to hook right into the hof,
-- you may use this function.
--
-- >>> ala' Sum foldMap length ["hello", "world"]
-- 10
--
-- >>> ala' First foldMap (readMaybe @Int) ["x", "42", "1"]
-- Just 42
ala' :: (Coercible n o, Newtype n' o', Similar n n')
     => (o `to` n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
--ala' _ hof f = unpack . hof (coerce . f)
ala' _ = coerce

-- | A very simple operation involving running the function \'under\' the newtype.
--
-- >>> under Product (stimes 3) 3
-- 27
under :: (Coercible n o, Newtype n' o', Similar n n')
      => (o `to` n) -> (n -> n') -> (o -> o')
under _ f = unpack #. f .# coerce

-- | The opposite of 'under'. I.e., take a function which works on the
-- underlying types, and switch it to a function that works on the newtypes.
--
-- >>> over All not (All False)
-- All {getAll = True}
over :: (Coercible n o,  Newtype n' o', Similar n n')
     => (o `to` n) -> (o -> o') -> (n -> n')
over _ f = pack #. f .# coerce

-- | Lower a binary function to operate on the underlying values.
--
-- >>> under2 Any (<>) True False
-- True
under2 :: (Coercible n o, Newtype n' o', Similar n n')
       => (o `to` n) -> (n -> n -> n') -> (o -> o -> o')
--under2 _ f o0 o1 = unpack $ f (coerce o0) (coerce o1)
under2 _ = coerce

-- | The opposite of 'under2'.
over2 :: (Coercible n o, Newtype n' o', Similar n n')
       => (o `to` n) -> (o -> o -> o') -> (n -> n -> n')
--over2 _ f n0 n1 = pack $ f (coerce n0) (coerce n1)
over2 _ = coerce

-- | 'under' lifted into a functor.
underF :: (Coercible (f o) (f n), Coercible (g n') (g o'), Newtype n' o', Similar n n')
       => (o `to` n) -> (f n -> g n') -> (f o -> g o')
underF _ f = coerce #. f .# coerce

-- | 'over' lifted into a functor.
overF :: (Coercible (f n) (f o), Coercible (g o') (g n'), Newtype n' o', Similar n n')
      => (o `to` n) -> (f o -> g o') -> (f n -> g n')
overF _ f = coerce #. f .# coerce
