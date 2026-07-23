{-# OPTIONS_HADDOCK hide #-}

-- | Reusable type-level error messages for the marshaller types, so diagnostics
-- read consistently. These are 'ErrorMessage' type synonyms; consume them with
-- 'GHC.TypeLits.TypeError' in an instance context.
--
module HsBindgen.HighLevel.Internal.Errors (
    NoDefault
  , AutoMismatch
  , TooManyResults
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), Symbol)

-- | No default marshaller exists for a type. @position@ is @"input"@, @"output"@ or
-- @"result"@; @hs@ is the type with no default (the Haskell type for @"input"@ and
-- @"output"@, the C return type for @"result"@).
--
type NoDefault :: Symbol -> Type -> ErrorMessage
type NoDefault position hs =
        'Text "No default " ':<>: 'Text position ':<>: 'Text " marshaller for type:"
  ':$$: 'Text "    " ':<>: 'ShowType hs
  ':$$: 'Text "Pass an explicit marshaller for this position, or define a Default instance for it."

-- | @auto@ ran out of high-level arguments while the C function still expects
-- some, so it cannot line the two up. @hi@ is the remaining high-level type (a
-- result, like @IO Int@) and @lo@ the remaining C type (still a function). The
-- usual causes are a C argument the wrapper does not expose (an out-parameter or
-- scratch buffer) or an argument missing from the signature.
--
type AutoMismatch :: Type -> Type -> ErrorMessage
type AutoMismatch hi lo =
        'Text "auto cannot line the high-level type up with the C function here:"
  ':$$: 'Text "    high-level:  " ':<>: 'ShowType hi
  ':$$: 'Text "    C remainder: " ':<>: 'ShowType lo
  ':$$: 'Text "auto fills inputs and the result only. A C argument the wrapper does not"
  ':$$: 'Text "take (an out-parameter or scratch buffer) needs an explicit 'output' or"
  ':$$: 'Text "'scratch'; a missing high-level argument needs adding to the signature."

-- | A wrapper builds more result components than the flat tuple holds. The result
-- is a flat tuple of at most eight: the kept return value plus up to seven outputs.
--
type TooManyResults :: ErrorMessage
type TooManyResults =
        'Text "This wrapper builds more than eight result components."
  ':$$: 'Text "The high-level result is a flat tuple of at most eight: the kept"
  ':$$: 'Text "return value plus up to seven outputs. Combine outputs into a struct"
  ':$$: 'Text "or a newtype-wrapped tuple, or drop a write-only one with 'scratch'."
