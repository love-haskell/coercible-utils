{-# language TypeOperators #-}

{- |
This module reexports the entire content of the `coercible-utils` package.

See the 'CoercibleUtils.Newtype' module for the newtype combinators.

__Note__: Most functions in this package take an argument that solely
directs the /type/ of the coercion. The value of this argument is /ignored/.
In each case, this argument has a type that looks like @a \`to\` b@. As the name
of the @to@ type variable suggests, this will typically be a function from
@a@ to @b@. But leaving the type variable completely polymorphic and
unconstrained lets the type signature communicate the fact that the argument
is not used.
-}
module CoercibleUtils
  ( module CoercibleUtils.Newtype
  , (#.), (.#)
  ) where

import CoercibleUtils.Compose ((#.), (.#))
import CoercibleUtils.Newtype
