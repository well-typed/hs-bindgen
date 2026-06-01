-- | Reusable type-level error messages for the 'ToHighLevel' type, so diagnostics
-- read consistently. These are 'ErrorMessage' type synonyms; consume them with
-- 'GHC.TypeLits.TypeError' in an instance context.
--
module HsBindgen.Runtime.HighLevel.ToHighLevel.Errors (
    NoDefault
  , TooManyOutputs
  , UnitOutput
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), Symbol)

-- | No default marshaller exists for a Haskell type. @position@ is @"input"@,
-- @"output"@ or @"result"@.
--
type NoDefault :: Symbol -> Type -> ErrorMessage
type NoDefault position hs =
        'Text "No default " ':<>: 'Text position ':<>: 'Text " marshaller for type:"
  ':$$: 'Text "    " ':<>: 'ShowType hs
  ':$$: 'Text "Pass an explicit marshaller, or define a Default instance for it."

-- | A spec keeps more outputs than fit in a flat tuple (the ceiling is
-- six). Raised instead of silently nesting the result tuple.
type TooManyOutputs :: ErrorMessage
type TooManyOutputs =
        'Text "This spec keeps more than six outputs."
  ':$$: 'Text "A flat result tuple tops out at six components."
  ':$$: 'Text "Return a record from a hand-written wrapper instead."

-- | An output whose peeked value is @()@ cannot be kept: its @()@ is
-- indistinguishable from the empty result accumulator, so keeping it would
-- silently drop a tuple component (collapsing the result arity). Raised instead
-- of dropping it. A write-only out-parameter should use @scratch@.
type UnitOutput :: ErrorMessage
type UnitOutput =
        'Text "An output whose value is () cannot be kept in the result tuple."
  ':$$: 'Text "Its () collides with the empty-result accumulator, so keeping it"
  ':$$: 'Text "would silently drop a component. Use 'scratch' for a write-only"
  ':$$: 'Text "out-parameter, or peek a non-() value."
