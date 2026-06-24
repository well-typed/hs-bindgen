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
  , TranslatedAnonRef(..)
  , translatedAnonRefType
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

{-------------------------------------------------------------------------------
  TranslatedTypes
-------------------------------------------------------------------------------}

-- | A t'C.Type' associated with possible Haskell type translations
data TranslatedTypes (p :: Pass) = TranslatedTypes {
      c :: C.Type p
--    , hs :: Hs.Type    -- TODO
    }
  deriving stock (Eq, Generic, Show)

instance (
      CoercePass C.Type p p'
    ) => CoercePass TranslatedTypes p p' where
  coercePass translatedTypes = TranslatedTypes{
      c = coercePass translatedTypes.c
    }

-- | A t'C.AnonRef' associated with possible Haskell type translations
data TranslatedAnonRef (p :: Pass) = TranslatedAnonRef {
      c :: C.AnonRef p
--    , hs :: Hs.Type    -- TODO
    }
  deriving stock (Eq, Generic, Show)

instance (
      CoercePass C.AnonRef p p'
    ) => CoercePass TranslatedAnonRef p p' where
  coercePass translatedAnonRef = TranslatedAnonRef {
      c = coercePass translatedAnonRef.c
    }

translatedAnonRefType :: TranslatedAnonRef p -> TranslatedTypes p
translatedAnonRefType translatedAnonRef = TranslatedTypes {
      c = C.anonRefType translatedAnonRef.c
    }
