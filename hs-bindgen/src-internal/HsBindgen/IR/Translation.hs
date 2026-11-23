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
    -- * Types
  , Translated(..)
  , TranslatedTypes
  , TranslatedTypeFunArg
  , TranslatedAnonRef
  , translatedAnonRefType
  , translateCPrimType
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass
import HsBindgen.Language.C qualified as C
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
  Types
-------------------------------------------------------------------------------}

-- | A C use-site type associated with its Haskell type translation
data Translated (f :: Pass -> Star) (p :: Pass) = Translated {
      -- | A t'C.Type' ('TranslatedTypes'), a t'C.TypeFunArg'
      -- ('TranslatedTypeFunArg') or a t'C.AnonRef' ('TranslatedAnonRef')
      --
      -- A t'C.TypeFunArg' keeps its annotation, on which the translation of the
      -- argument depends: an argument adjusted from an array type translates
      -- to an array element pointer.
      c  :: f p
    , hs :: Hs.Type
    }
  deriving stock (Generic)

deriving stock instance Eq   (f p) => Eq   (Translated f p)
deriving stock instance Show (f p) => Show (Translated f p)

instance CoercePass f p p' => CoercePass (Translated f) p p' where
  coercePass translated = Translated{
      c  = coercePass translated.c
    , hs = translated.hs
    }

-- | A t'C.Type' associated with its Haskell type translation
type TranslatedTypes = Translated C.Type

-- | A t'C.TypeFunArg' associated with its Haskell type translation
type TranslatedTypeFunArg = Translated C.TypeFunArg

-- | A t'C.AnonRef' associated with its Haskell type translation
type TranslatedAnonRef = Translated C.AnonRef

translatedAnonRefType :: TranslatedAnonRef p -> TranslatedTypes p
translatedAnonRefType translatedAnonRef = Translated{
      c  = C.anonRefType translatedAnonRef.c
    , hs = translatedAnonRef.hs
    }

-- | Translate a primitive type
translateCPrimType :: C.PrimType -> Hs.PrimType
translateCPrimType = \case
    C.PrimBool         -> Hs.PrimCBool
    C.PrimIntegral i s -> integralType i s
    C.PrimFloating f   -> floatingType f
    C.PrimChar sign    -> charType sign
  where
    integralType :: C.PrimIntType -> C.PrimSign -> Hs.PrimType
    integralType C.PrimInt      C.Signed   = Hs.PrimCInt
    integralType C.PrimInt      C.Unsigned = Hs.PrimCUInt
    integralType C.PrimShort    C.Signed   = Hs.PrimCShort
    integralType C.PrimShort    C.Unsigned = Hs.PrimCUShort
    integralType C.PrimLong     C.Signed   = Hs.PrimCLong
    integralType C.PrimLong     C.Unsigned = Hs.PrimCULong
    integralType C.PrimLongLong C.Signed   = Hs.PrimCLLong
    integralType C.PrimLongLong C.Unsigned = Hs.PrimCULLong

    floatingType :: C.PrimFloatType -> Hs.PrimType
    floatingType C.PrimFloat  = Hs.PrimCFloat
    floatingType C.PrimDouble = Hs.PrimCDouble

    charType :: C.PrimSignChar -> Hs.PrimType
    charType (C.PrimSignImplicit _inferred)  = Hs.PrimCChar
    charType (C.PrimSignExplicit C.Signed)   = Hs.PrimCSChar
    charType (C.PrimSignExplicit C.Unsigned) = Hs.PrimCUChar
