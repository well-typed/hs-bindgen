-- | Translation IR
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Translation
module HsBindgen.IR.Translation (
    -- * Types
    DeclIdPair(..)
  , ScopedNamePair(..)
  ) where

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | A t'C.DeclId' paired with a Haskell name
data DeclIdPair = DeclIdPair {
      cName  :: C.DeclId
    , hsName :: Hs.SomeName
    }
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | A t'C.ScopedName' paired with a Haskell name
data ScopedNamePair = ScopedNamePair {
      cName  :: C.ScopedName
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1927>
      -- ScopedNamePair only ever refers to type constructors and variable
      -- names.
    , hsName :: Hs.SomeName
    }
  deriving stock (Eq, Generic, Ord, Show)
