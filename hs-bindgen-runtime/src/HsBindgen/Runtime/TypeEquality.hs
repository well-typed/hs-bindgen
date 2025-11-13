{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}

module HsBindgen.Runtime.TypeEquality (
    -- $type-equality
    TyEq
  ) where

import Data.Kind

{- $type-equality

On GHC versions <= 9.2, type equality @(~)@ is a magic built-in syntax, while on
later GHC versions it is a proper type operator that has to be imported from
'Prelude' or some other module from the @base@ package. The 'TyEq' class bridges
the gap between the GHC versions: it is always a class regardless of the GHC
version, and it has no special usage and import constraints depending on the GHC
version. @hs-bindgen@ generates Haskell bindings using 'TyEq' so that the
generated bindings are (syntactically) the same regardless of the GHC version
that was used to run @hs-bindgen@.
-}

-- | Type equality @~@ as a class
type TyEq :: k -> k -> Constraint
class a ~ b => TyEq a b
instance a ~ b => TyEq a b
