{-# LANGUAGE GADTs #-}

-- | The spec type itself, and the out-parameter values a spec collects.
--
-- Most bindings never import this module: t'ToHighLevel' and 'toHighLevel' are
-- re-exported from "HsBindgen.HighLevel", and the rest only shows up in a type error.
-- It is here so that those types are documented where they are defined, and so that a
-- combinator of your own can name them.
module HsBindgen.HighLevel.Internal.Spec (
    -- * The spec
    ToHighLevel (..)
  , toHighLevel
    -- * The collected out-parameter values
  , Outputs (..)
  , Assembler
  , ApplyOutputs (..)
  ) where

import Data.Kind (Type)

{-------------------------------------------------------------------------------
  The collected out-parameter values
-------------------------------------------------------------------------------}

-- | The values that the @output@s in a spec read back, most recent first.
--
-- Only @output@ builds one and only a closer takes one apart, so the reversed order
-- never escapes: 'Assembler' and 'applyOutputs' both undo it, and the assembler you
-- write takes its arguments in spec order.
type Outputs :: [Type] -> Type
data Outputs os where
  NoOutputs :: Outputs '[]
  (:*)      :: a -> Outputs os -> Outputs (a : os)
infixr 5 :*

-- | The type of the function a closer wants: one argument per output, in __spec
-- order__, and then @r@.
--
-- It is a fold over @os@ that hangs the outputs in front of @r@. @os@ holds them most
-- recent first, and the fold peels the most recent one first, which parks it nearest
-- @r@; the oldest is peeled last and so ends up leftmost. The reversal undoes itself:
--
-- > Assembler '[]         r  =                  r
-- > Assembler '[b]        r  =  b ->            r
-- > Assembler '[c, b]     r  =  b -> c ->       r
-- > Assembler '[d, c, b]  r  =  b -> c -> d ->  r
--
-- So for the spec
--
-- > output boolOut $ output textOut $ resultPure f
--
-- the collected @os@ is @\'[Text, Bool]@ and @f@ has to be a
-- @Bool -> Text -> CInt -> hs@: the two outputs in the order they appear, then the C
-- return value, then whatever result you want.
--
-- Note what @r@ absorbs: the whole @c -> hs@ tail, rather than the wrapper's result
-- type alone. That is why 'HsBindgen.HighLevel.resultPure' can pass
-- @'Assembler' os (c -> hs)@ and get an assembler that ends in the C return value.
--
-- One practical consequence: the family reduces as soon as the /number/ of outputs is
-- known, whatever their types are. A closer's argument type is therefore concrete even
-- while an @output@ above it is still a hole, which is what makes
-- @resultPure _@ report a usable type.
type Assembler :: [Type] -> Type -> Type
type family Assembler os r where
  Assembler '[]      r = r
  Assembler (a : os) r = Assembler os (a -> r)

-- | Apply an assembler to the collected values, in spec order. A closer's last step,
-- and the reason 'HsBindgen.HighLevel.resultPure' and 'HsBindgen.HighLevel.resultIO'
-- carry an @ApplyOutputs os@ constraint. Any spec built from the combinators
-- satisfies it.
class ApplyOutputs (os :: [Type]) where
  applyOutputs :: Outputs os -> Assembler os r -> r

instance ApplyOutputs '[] where
  applyOutputs NoOutputs f = f
  {-# INLINE applyOutputs #-}

instance ApplyOutputs os => ApplyOutputs (a : os) where
  applyOutputs (x :* xs) f = applyOutputs xs f x
  {-# INLINE applyOutputs #-}

{-------------------------------------------------------------------------------
  The spec
-------------------------------------------------------------------------------}

-- | A recipe for turning the low-level callable @lo@ into the high-level wrapper
-- @hi@, collecting the out-parameter types @os@ on the way.
--
-- The three indices are described in \"Reading the signatures\" in
-- "HsBindgen.HighLevel"; in short, @lo@ is what is left of the C function's type,
-- @hi@ is what is left of the wrapper's, and @os@ is the out-parameter Haskell types
-- collected /above/ this point, most recent first.
--
-- @os@ starts empty at 'toHighLevel' and each @output@ adds to it going down, so it is
-- known from the spec's own text: by the time the closer is reached it has the
-- complete list and can demand an @'Assembler' os@ for it. Nothing has to be inferred
-- backwards out of the closer, which is what keeps the design workable.
--
-- The value threaded alongside is the deferred read-back: an @'IO' ('Outputs' os)@
-- that each @output@ extends with its own peek and that a closer runs once the C call
-- has returned.
--
-- Build one with @input@ \/ @output@ \/ @scratch@ and a closer; see
-- "HsBindgen.HighLevel".
type ToHighLevel :: [Type] -> Type -> Type -> Type
newtype ToHighLevel os lo hi = ToHighLevel (IO (Outputs os) -> lo -> hi)

-- | Run a finished spec against a low-level callable (the raw @foreign import@) to
-- get the high-level wrapper. Every binding ends here.
--
-- This is also where the two ends are tied down: @lo@ unifies with the C function's
-- actual type and @hi@ with the wrapper's type signature, and everything in the spec
-- between them follows from those two. A spec starts with nothing collected, hence
-- the @\'[]@.
toHighLevel :: ToHighLevel '[] lo hi -> lo -> hi
toHighLevel (ToHighLevel f) = f (pure NoOutputs)
{-# INLINE toHighLevel #-}
