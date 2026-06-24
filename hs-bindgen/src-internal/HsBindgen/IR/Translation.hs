-- | Translation IR
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Translation
module HsBindgen.IR.Translation (
    -- * DeclIdPair
    DeclIdPair(..)
  , extDeclIdPair
    -- * ScopedNamePair
  , ScopedNamePair(..)
  , TranslatedTypes(..)
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  DeclIdPair
-------------------------------------------------------------------------------}

-- | A t'C.DeclId' paired with a Haskell name
data DeclIdPair = DeclIdPair {
      cName  :: C.DeclId
    , hsName :: Hs.SomeName
    }
  deriving stock (Eq, Ord, Show)

-- | Get the 'DeclIdPair' for a 'ResolvedExtBinding'
extDeclIdPair :: BindingSpec.ResolvedExtBinding -> DeclIdPair
extDeclIdPair ext = DeclIdPair{
      cName  = ext.cName
    , hsName = Hs.demoteNs ext.hsName.name
    }

{-------------------------------------------------------------------------------
  ScopedNamePair
-------------------------------------------------------------------------------}

-- | A t'C.ScopedName' paired with a Haskell name
data ScopedNamePair = ScopedNamePair {
      cName  :: C.ScopedName
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1927>
      -- ScopedNamePair only ever refers to type constructors and variable
      -- names.
    , hsName :: Hs.SomeName
    }
  deriving stock (Eq, Generic, Ord, Show)

--------------------------------------------------------------------------------

-- | A t'C.Type' associated with possible Haskell type translations
data TranslatedTypes (p :: Pass) = TranslatedTypes {
      c :: C.Type p
--    , hs :: Hs.Type    -- TODO
    }
  deriving stock (Eq, Generic, Show)

instance (
      CoercePassId p p'
    , CoercePassMacroId p p'
    , CoercePassMacroUnderlying p p'
    , CoercePassAnn "TypeFunArg" p p'
    , ScopedName p ~ ScopedName p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass TranslatedTypes p p' where
  coercePass translatedTypes = TranslatedTypes{
      c = coercePass translatedTypes.c
    }
