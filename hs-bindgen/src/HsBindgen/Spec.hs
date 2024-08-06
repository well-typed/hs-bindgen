-- | Specification of what @hs-bindgen@ should do
--
-- Intended for qualified import.
--
-- > import HsBindgen.Spec (Spec(..))
-- > import HsBindgen.Spec qualified as Unresolved
--
-- The @Unresolved@ qualifier distinguishes this module from
-- "HsBindgen.Spec.Resolved".
module HsBindgen.Spec (
    Spec(..)
    -- * Resolution
  , resolve
  ) where

import HsBindgen.Spec.Resolved qualified as Resolved

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Specification of what @hs-bindgen@ should do
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/10>
-- This needs to include parameters for cross compilation.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/71>
-- This needs to have fields with paths, preprocessor defines, etc.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
-- Support multiple C headers.
data Spec = Spec {
      -- | Path to the C header
      specCHeader :: FilePath

      -- | Name of the generated Haskell module
    , specHsModuleName :: String
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Resolution
-------------------------------------------------------------------------------}

-- | Resolve the spec
--
-- The resulting 'Resolved.Spec' contains the same information as the input
-- 'Spec', but the resolution will involve reading files from disk, interacting
-- with the C toolchain, etc. The goal is that after resolution the translation
-- from the resolved 'Resolved.Spec' to a Haskell module is a pure function.
--
-- The 'Resolved.Spec' is an opaque type in the public API.
resolve :: Spec -> IO Resolved.Spec
resolve spec = do
    cHeader <- readFile $ specCHeader spec
    return Resolved.Spec{
        specCHeader      = cHeader
      , specHsModuleName = specHsModuleName spec
      }