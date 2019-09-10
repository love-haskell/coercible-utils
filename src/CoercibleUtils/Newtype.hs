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
A version of the 'Newtype' typeclass and related functions.  The API is
primarily pulled from Conor McBride's Epigram work. The general idea is that we
can manipulate types in terms of newtypes around them or newtypes in terms of
their underlying types. Some examples:

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

Like McBride's version, and unlike the one in @newtype-generics@, this version
has two parameters: one for the newtype and one for the underlying type. This
is mostly a matter of taste.

__Note__: Most functions in this module take an argument representing a newtype
constructor. This is used only for its type. To make that clear, the type of
that argument is allowed to be extremely polymorphic: @o \`to\` n@ rather than
@o -> n@. Unfortunately, GHCi displays this as @to o n@, which is ugly but
equivalent.

General approach: When the type variables @n@ and @o@ appear, @n@ is required
to be a newtype wrapper around @o@. Similarly, when the type variables @n'@ and
@o'@ appear as well, @n'@ is required to be a newtype wrapper around @o'@.
Furthermore, in this case, @n@ and @n'@ are required to be the /same newtype/,
with possibly different type arguments. See 'Similar' for detailed
documentation.

@since TODO
-}
module CoercibleUtils.Newtype
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
import CoercibleUtils.Compose ((#.), (.#))
import Data.Kind (Constraint)

-- | Get the underlying type of a newtype.
--
-- @
-- data N = N Int deriving Generic
-- -- O N = Int
-- @
type O x = GO (Rep x)

-- | Get the underlying type of a newtype from its generic
-- representation.
type family GO rep where
  GO (D1 _d (C1 _c (S1 _s (K1 _i a)))) = a

-- | Given types @x@ and @o@, produce a constraint requiring that @x@ be a
-- newtype whose underlying type is @o@.
type NewtypeF x o = GNewtypeF x (Rep x) o

-- | Putting a newtype check and 'TypeError' call in 'GO' and then requiring @o
-- ~ O n@ doesn't get us a custom type error when we want one. Calculating the
-- constraint with an error case, however, seems to do the trick.
type family GNewtypeF x rep o :: Constraint where
  GNewtypeF _x (D1 ('MetaData _n _m _p 'True) (C1 _c (S1 _s (K1 _i a)))) o = a ~ o
  GNewtypeF x _rep _o = TypeError
    ('Text "There is no " ':<>: 'ShowType Newtype ':<>: 'Text " instance for"
      ':$$: 'Text "    " ':<>: 'ShowType x
      ':$$: 'Text "because it is not a newtype.")

-- | @Newtype n o@ means that @n@ is a newtype wrapper around @o@. @n@ must be
-- an instance of 'Generic'. Furthermore, the @'Coercible' n o@ instance must
-- be visible; this typically means the newtype constructor is visible, but the
-- instance could also have been brought into view by pattern matching on a
-- 'Data.Type.Coercion.Coercion'.
class Coercible n o => Newtype (n :: k) (o :: k)

-- The Generic n constraint gives a much better type error if n is not an
-- instance of Generic. Without that, there's just a mysterious message
-- involving GO and Rep. With it, the lousy error message still shows up, but
-- at least there's also a good one.
instance (Generic n, NewtypeF n o, Coercible n o) => Newtype n o

-- | A single-parameter version of 'Newtype', similar to the @Newtype@ class in
-- @newtype-generics@.
--
-- @Newtype n o@ is equivalent to @(IsNewtype n, o ~ O n)@.
class Newtype n (O n) => IsNewtype n
instance Newtype n (O n) => IsNewtype n

-- | A version of 'Newtype' with the parameters flipped, for partial
-- application.
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
-- See [Note: Similar implementation]

instance (Similar' n n', Similar' n' n) => Similar n n'

-- | Given a type of the form (f b), try to extract b, assuming that b is of
-- kind kb. Note that kb itself is an implicit argument to GetArg; the
-- surrounding context of the call must determine kb.
type family GetArg (x :: kx) :: kb where
  GetArg (_ b) = b

-- | Given a type of the form (f b), try to extract f, assuming that f is of
-- kind kf. Note that kf itself is an implicit argument to GetFun; the
-- surrounding context of the call must determine kf.
type family GetFun (x :: kx) :: kf where
  GetFun (f _) = f

-- | Given types @n@ and @n'@, produce a constraint requiring that @n'@ be
-- built from the same newtype constructor as @n@. See [Note: Similar
-- implementation].
type family Similar' (n :: k) (n' :: k) :: Constraint where
  Similar' (f (_a :: ka)) n' =
    ( Similar' f (GetFun n')
    , n' ~ GetFun n' (GetArg n' :: ka))
  Similar' f g = f ~ g
-- We capture the kind of the argument as ka, and pass it to GetArg.
-- The kind of the first call to GetFun is fixed by the kind of f,
-- while the kind of the second is fixed by the kind of n' and the
-- imposed kind, ka, of GetArg.

{-
[Note: Similar implementation]

The Similar class is what really separates this implementation of this API from
all previous ones of which I am aware. Since it's not the simplest thing, I'll
give an extended description of the implementation here.

We really want n to give us information about n'. It's also nice (for typed
holes and error messages, particularly) if n' gives us information about n.
Since we may know nothing, a priori, about one of those, we have to be a bit
careful not to force a match on one when the other is the one with information.
The horrible way to do that (even worse :t results!) is with INCOHERENT
typeclass instances. Ignoring that awfulness, the thing to do is to require n'
to have a structure calculated from n and the other way around, "in parallel".
Similar' n n' matches on n, producing a constraint. Suppose we have n ~ F a b.
Then we get

   Similar' (F a b) n'
   (Similar' (F a) (GetFun n'), n' ~ GetFun n' (GetArg n'))

We have now constrained things only a little: n' is required to have the form p
q for some p and q, where F a and p have the same kind and b and q have the
same kind. Substituting in p and q, we continue:

   (Similar' (F a) p, n' ~ p q)
   ((Similar' F (GetFun p), p ~ GetFun p (GetArg p)), n' ~ p q)

Let's set r ~ GetFun p and s ~ GetArg p. Then

   ((Similar' F r, p ~ r s), n' ~ p q)

We've now reached the base case! Yay! So the last step is

   ((F ~ r, p ~ r s), n' ~ p q)

which then simplifies to

   n' ~ F s q

Undoing the substitutions we made,

   n' ~ F (GetArg (GetFun n')) (GetArg n')

The arguments in that constraint are completely useless (as far as I know), but
we need to put it all together like that to get what we're after: an equality
constraint on n' itself that lets GHC know it's F x y for some x and y to be
determined. Very often, those are then fixed by the type o' and the Newtype n'
o' constraint, which at first seems backwards. Say we consider Sum. We have

   type Rep (Sum a) = M1 _ _ (M1 _ _ (M1 _ _ (K1 _ a)))

This can reduce as soon as GHC sees we have Sum whatever! So then the
NewtypeF-calculated constraint can constrain the type argument to whatever's
required to wrap the new contents. Magic!
-}

-- | Wrap a value with a newtype constructor.
pack :: Newtype n o => o -> n
pack = coerce

-- | Unwrap a newtype constructor from a value.
unpack :: Newtype n o => n -> o
unpack = coerce

-- | Reverse the type of a "packer".
--
-- >>> op All (All True)
-- True
-- >>> op (Identity . Sum) (Identity (Sum 3))
-- 3
op :: Coercible a b
   => (a `to` b)
   -> b
   -> a
op _ = coerce
{-# INLINE op #-}

-- | The workhorse of the package. Given a "packer" and a \"higher order function\" (/hof/),
-- it handles the packing and unpacking, and just sends you back a regular old
-- function, with the type varying based on the /hof/ you passed.
--
-- The reason for the signature of the /hof/ is due to 'ala' not caring about structure.
-- To illustrate why this is important, consider this alternative implementation of 'under2':
--
-- > under2 :: (Newtype n o, Newtype n' o')
-- >        => (o -> n) -> (n -> n -> n') -> (o -> o -> o')
-- > under2 pa f o0 o1 = ala pa (\p -> uncurry f . bimap p p) (o0, o1)
--
-- Being handed the "packer", the /hof/ may apply it in any structure of its choosing –
-- in this case a tuple.
--
-- >>> ala Sum foldMap [1,2,3,4]
-- 10
ala :: (Newtype n o, Newtype n' o', Similar n n')
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
ala' :: (Newtype n o, Newtype n' o', Similar n n')
     => (o `to` n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
--ala' _ hof f = unpack . hof (pack . f)
ala' _ = coerce

-- | A very simple operation involving running the function \'under\' the newtype.
--
-- >>> under Product (stimes 3) 3
-- 27
under :: (Newtype n o, Newtype n' o', Similar n n')
      => (o `to` n) -> (n -> n') -> (o -> o')
under _ f = unpack #. f .# coerce

-- | The opposite of 'under'. I.e., take a function which works on the
-- underlying types, and switch it to a function that works on the newtypes.
--
-- >>> over All not (All False)
-- All {getAll = True}
over :: (Newtype n o,  Newtype n' o', Similar n n')
     => (o `to` n) -> (o -> o') -> (n -> n')
over _ f = pack #. f .# coerce

-- | Lower a binary function to operate on the underlying values.
--
-- >>> under2 Any (<>) True False
-- True
under2 :: (Newtype n o, Newtype n' o', Similar n n')
       => (o `to` n) -> (n -> n -> n') -> (o -> o -> o')
--under2 _ f o0 o1 = unpack $ f (pack o0) (pack o1)
under2 _ = coerce

-- | The opposite of 'under2'.
over2 :: (Newtype n o, Newtype n' o', Similar n n')
       => (o `to` n) -> (o -> o -> o') -> (n -> n -> n')
--over2 _ f n0 n1 = pack $ f (unpack n0) (unpack n1)
over2 _ = coerce

-- | 'under' lifted into functors
underF :: ( Newtype n o, Coercible (f o) (f n)
          , Coercible (g n') (g o'), Newtype n' o', Similar n n')
       => (o `to` n) -> (f n -> g n') -> (f o -> g o')
underF _ f = coerce #. f .# coerce

-- | 'over' lifted into functors
overF :: ( Newtype n o, Coercible (f n) (f o), Coercible (g o') (g n')
         , Newtype n' o', Similar n n')
      => (o `to` n) -> (f o -> g o') -> (f n -> g n')
overF _ f = coerce #. f .# coerce
