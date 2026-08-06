-- | The type-level error messages the library reports, in one place.
--
-- These are 'ErrorMessage' synonyms, consumed with 'GHC.TypeLits.TypeError' in an
-- instance context. Read them here to see the full text of a message you hit, or
-- reuse one when adding a default instance of your own.
--
module HsBindgen.HighLevel.Internal.Errors (
    NoDefault
  , AutoMismatch
  , AutoResultMismatch
  , AutoOutputsMismatch
  , unreachable
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), Symbol)

-- | No default marshaller exists for a type. @combinator@ is @"input"@, @"output"@ or
-- @"result"@; @hs@ is the type with no default (the Haskell type for @"input"@ and
-- @"output"@, the C return type for @"result"@).
--
type NoDefault :: Symbol -> Type -> ErrorMessage
type NoDefault combinator hs =
        'Text "No default " ':<>: 'Text combinator ':<>: 'Text " marshaller for type:"
  ':$$: 'Text "    " ':<>: 'ShowType hs
  ':$$: 'Text "Pass an explicit marshaller to this combinator, or define a Default instance for it."

-- | @auto@ ran out of high-level arguments while the C function still expects
-- some, so it cannot line the two up. @hi@ is the remaining high-level type (a
-- result, like @IO Int@) and @lo@ the remaining C type (still a function). The
-- usual causes are a C argument the high-level type does not expose (an out-parameter
-- or scratch buffer) or an argument missing from the signature.
--
type AutoMismatch :: Type -> Type -> ErrorMessage
type AutoMismatch hi lo =
        'Text "auto cannot line the high-level type up with the C function here:"
  ':$$: 'Text "    high-level:  " ':<>: 'ShowType hi
  ':$$: 'Text "    C remainder: " ':<>: 'ShowType lo
  ':$$: 'Text "auto fills inputs and the result only. A C argument the high-level type does"
  ':$$: 'Text "not take (an out-parameter or scratch buffer) needs an explicit 'output'"
  ':$$: 'Text "or 'scratch'; a missing high-level argument needs adding to the signature."

-- | @auto@ reached the closing combinator but cannot build the result its signature
-- asks for out of what the spec collected. @os@ is the list of out-parameter Haskell
-- types (most recent first), @c@ the C return type, and @hs@ the result the signature
-- asks for.
--
type AutoResultMismatch :: [Type] -> Type -> Type -> ErrorMessage
type AutoResultMismatch os c hs =
        'Text "auto cannot assemble this result:"
  ':$$: 'Text "    outputs (most recent first): " ':<>: 'ShowType os
  ':$$: 'Text "    C return type:               " ':<>: 'ShowType c
  ':$$: 'Text "    high-level result type:      " ':<>: 'ShowType hs
  ':$$: 'Text "auto builds a tuple of every output in spec order, then the converted C"
  ':$$: 'Text "return unless the C function returns void, and it covers up to five"
  ':$$: 'Text "outputs. Give the signature that result type, or write the closer by hand"
  ':$$: 'Text "with 'resultPure' / 'resultIO', which accept any shape."

-- | The out-parameters a spec collected do not assemble into the result the high-level
-- type asks for. Reported where the C return value contributes nothing to the result:
-- a @void@ call under @auto@, or a status a closer consumed
-- ('HsBindgen.HighLevel.Auto.checkedResult',
-- 'HsBindgen.HighLevel.Auto.maybeResult',
-- 'HsBindgen.HighLevel.Auto.eitherResult'). @os@ is the list of out-parameter Haskell types
-- (most recent first) and @hs@ the result the signature asks for.
--
type AutoOutputsMismatch :: [Type] -> Type -> ErrorMessage
type AutoOutputsMismatch os hs =
        'Text "auto cannot assemble this result from the out-parameters alone:"
  ':$$: 'Text "    outputs (most recent first): " ':<>: 'ShowType os
  ':$$: 'Text "    high-level result type:      " ':<>: 'ShowType hs
  ':$$: 'Text "Nothing of the C return value survives into the result here, either"
  ':$$: 'Text "because the C function returns void or because a closer consumed its"
  ':$$: 'Text "status, so the result is the outputs alone: no outputs is (), one output"
  ':$$: 'Text "is that output, and several are a tuple of them in spec order. Up to five"
  ':$$: 'Text "are covered. Give the signature that result type, or write the closer by"
  ':$$: 'Text "hand with 'resultPure' / 'resultIO', which accept any shape."

-- | The body of an instance that exists only to carry a 'GHC.TypeLits.TypeError'.
-- Selecting such an instance is what raises the error, so the body is never evaluated.
unreachable :: a
unreachable = errorWithoutStackTrace "HsBindgen.HighLevel: unreachable"
