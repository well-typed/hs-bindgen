{-# OPTIONS_HADDOCK hide #-}

-- | The 'ToHighLevel' spec type. Internal so the sibling
-- "HsBindgen.HighLevel.Unlifted" can build specs with the constructor while
-- "HsBindgen.HighLevel" re-exports the type abstractly.
module HsBindgen.HighLevel.Internal.Spec (
    ToHighLevel (..)
  , toHighLevel
  ) where

-- | A spec for lifting a low-level callable @lo@ into a high-level wrapper @hi@.
-- Read it as a recipe that turns @lo@ into @hi@: it is literally that @lo -> hi@
-- function, which 'toHighLevel' applies. Build it with @input@ \/ @output@ \/
-- @scratch@ and a closer (see "HsBindgen.HighLevel").
--
newtype ToHighLevel lo hi = ToHighLevel (lo -> hi)

-- | Run a finished spec against a low-level callable (the raw @foreign import@),
-- producing the high-level wrapper. Every binding ends here: build the spec with
-- the combinators in "HsBindgen.HighLevel", then apply it. That module's
-- header shows the full pattern.
toHighLevel :: ToHighLevel lo hi -> lo -> hi
toHighLevel (ToHighLevel f) = f
{-# INLINE toHighLevel #-}
