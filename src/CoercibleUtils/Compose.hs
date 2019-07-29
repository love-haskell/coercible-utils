{-# language TypeOperators #-}

{-|
Coercive function composition.

__Note__: The functions in this module take an argument that solely
directs the /type/ of the coercion. The value of this argument is /ignored/.
In each case, this argument has a type that looks like @a \`to\` b@. As the name
of the @to@ type variable suggests, this will typically be a function from
@a@ to @b@. But leaving the type variable completely polymorphic and
unconstrained lets the type signature communicate the fact that the argument
is not used.
-}
module CoercibleUtils.Compose
  ( (#.)
  , (.#)
  ) where

import Data.Coerce (Coercible, coerce)

-- | Coercive left-composition.
--
-- >>> (All #. not) True
-- All {getAll = False}
--
-- The semantics with respect to bottoms are:
--
-- @
-- p '#.' ⊥ ≡ ⊥
-- p '#.' f ≡ p '.' f
-- @
infixr 9 #.
(#.) :: Coercible b c => (b `to` c) -> (a -> b) -> a -> c
(#.) _ = coerce
{-# INLINE (#.) #-}

-- | Coercive right-composition.
--
-- >>> (stimes 2 .# Product) 3
-- Product {getProduct = 9}
--
-- The semantics with respect to bottoms are:
--
-- @
-- ⊥ '.#' p ≡ ⊥
-- f '.#' p ≡ p '.' f
-- @
infixr 9 .#
(.#) :: Coercible a b => (b -> c) -> (a `to` b) -> a -> c
(.#) f _ = coerce f
{-# INLINE (.#) #-}
