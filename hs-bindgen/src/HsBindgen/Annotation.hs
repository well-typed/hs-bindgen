-- | Syntax tree annotations
--
-- The type argument @l@ pervasive in @haskell-src-exts@ allows for arbitrary
-- annotations; we will instantiate it to 'Ann', defined in this module. This
-- type is not considered part of the public API of @hs-bindgen@.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/74>
-- If we want to include LINE pragmas, we will need to include line information
-- (referring to the C header) in these annotations.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Annotation (Ann)
-- > import HsBindgen.Annotation qualified as Ann
module HsBindgen.Annotation (
    Ann(..)
  ) where

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Syntax tree annotation
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/23>
-- We should use this explain tool decisions (when generating high-level API).
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/74>
-- We should reference the relevant part of the C header here (including line
-- numbers).
data Ann = Ann {
    }

instance Semigroup Ann where
  _ <> _ = mempty

instance Monoid Ann where
  mempty = Ann