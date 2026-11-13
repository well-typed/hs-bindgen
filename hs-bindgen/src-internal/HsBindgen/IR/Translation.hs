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
  , TranslatedTypes(..)
  , TranslatedTypeFunArg(..)
  , TranslatedAnonRef(..)
  , translatedAnonRefType
  , translateCPrimType
    -- * Function types
  , FunType(..)
  , ClassifiedFunType(..)
  , UnclassifiedFunType(..)
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

-- | A t'C.Type' associated with possible Haskell type translations
data TranslatedTypes (p :: Pass) = TranslatedTypes {
      c       :: C.Type p
    , hs      :: Hs.Type
      -- | The outermost function type in 'c'
      --
      -- See 'FunType'.
    , funType :: Maybe (FunType p)
    }
  deriving stock (Generic)

deriving stock instance IsPass p => Eq   (TranslatedTypes p)
deriving stock instance IsPass p => Show (TranslatedTypes p)

instance (
      CoercePass C.Type  p p'
    , CoercePass FunType p p'
    ) => CoercePass TranslatedTypes p p' where
  coercePass translatedTypes = TranslatedTypes{
      c       = coercePass translatedTypes.c
    , hs      = translatedTypes.hs
    , funType = coercePass <$> translatedTypes.funType
    }

-- | A t'C.TypeFunArg' associated with possible Haskell type translations
data TranslatedTypeFunArg (p :: Pass) = TranslatedTypeFunArg {
      c       :: C.TypeFunArg p
    , hs      :: Hs.Type
      -- | The outermost function type in 'c'
      --
      -- See 'FunType'.
    , funType :: Maybe (FunType p)
    }
  deriving stock (Generic)

deriving stock instance IsPass p => Eq   (TranslatedTypeFunArg p)
deriving stock instance IsPass p => Show (TranslatedTypeFunArg p)

instance (
      CoercePass C.TypeFunArg p p'
    , CoercePass FunType      p p'
    ) => CoercePass TranslatedTypeFunArg p p' where
  coercePass translatedTypes = TranslatedTypeFunArg{
      c       = coercePass translatedTypes.c
    , hs      = translatedTypes.hs
    , funType = coercePass <$> translatedTypes.funType
    }

-- | A t'C.AnonRef' associated with possible Haskell type translations
data TranslatedAnonRef (p :: Pass) = TranslatedAnonRef {
      c :: C.AnonRef p
    , hs :: Hs.Type
    }
  deriving stock (Eq, Generic, Show)

instance (
      CoercePass C.AnonRef p p'
    ) => CoercePass TranslatedAnonRef p p' where
  coercePass translatedAnonRef = TranslatedAnonRef {
      c = coercePass translatedAnonRef.c
    , hs = translatedAnonRef.hs
    }

translatedAnonRefType :: TranslatedAnonRef p -> TranslatedTypes p
translatedAnonRefType translatedAnonRef = TranslatedTypes {
      c       = C.anonRefType translatedAnonRef.c
    , hs      = translatedAnonRef.hs
      -- An anonymous declaration is a struct, union or enum
    , funType = Nothing
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

{-------------------------------------------------------------------------------
  Function types
-------------------------------------------------------------------------------}

-- | A function type nested in a use-site type
--
-- The outermost function type of a use-site type is the one reached through
-- pointers, arrays, blocks and qualifiers, not through typedefs or other
-- references. There is at most one: of the type constructors, only function
-- types have more than one child. The function types nested deeper sit in its
-- arguments or result, whose translated types carry their own 'FunType'.
--
-- @hs-bindgen@ generates @ToFunPtr@ and @FromFunPtr@ instances for a function
-- type iff it is classified and no position is serialised: function pointers
-- have no C wrapper.
data FunType (p :: Pass) =
    FunTypeClassified   (ClassifiedFunType p)
  | FunTypeUnclassified (UnclassifiedFunType p)
  deriving stock (Generic)

deriving stock instance IsPass p => Eq   (FunType p)
deriving stock instance IsPass p => Show (FunType p)

instance (
      CoercePass C.FunctionArg         p p'
    , CoercePass C.FunctionRes         p p'
    , CoercePass TranslatedTypes      p p'
    , CoercePass TranslatedTypeFunArg p p'
    ) => CoercePass FunType p p' where
  coercePass = \case
      FunTypeClassified   x -> FunTypeClassified   (coercePass x)
      FunTypeUnclassified x -> FunTypeUnclassified (coercePass x)

-- | A function type whose positions all have a passing method
--
-- The annotations of the arguments and the result are the passing methods,
-- as for a function declaration. The name of an argument is always 'Nothing'.
data ClassifiedFunType (p :: Pass) = ClassifiedFunType {
      args :: [C.FunctionArg p]
    , res  :: C.FunctionRes p
    }
  deriving stock (Generic)

deriving stock instance IsPass p => Eq   (ClassifiedFunType p)
deriving stock instance IsPass p => Show (ClassifiedFunType p)

instance (
      CoercePass C.FunctionArg p p'
    , CoercePass C.FunctionRes p p'
    ) => CoercePass ClassifiedFunType p p' where
  coercePass x = ClassifiedFunType{
      args = map coercePass x.args
    , res  = coercePass x.res
    }

-- | A function type with a position we cannot pass
--
-- The @TranslateTypes@ pass reports the position errors.
data UnclassifiedFunType (p :: Pass) = UnclassifiedFunType {
      args :: [TranslatedTypeFunArg p]
    , res  :: TranslatedTypes p
    }
  deriving stock (Generic)

deriving stock instance IsPass p => Eq   (UnclassifiedFunType p)
deriving stock instance IsPass p => Show (UnclassifiedFunType p)

instance (
      CoercePass TranslatedTypes      p p'
    , CoercePass TranslatedTypeFunArg p p'
    ) => CoercePass UnclassifiedFunType p p' where
  coercePass x = UnclassifiedFunType{
      args = map coercePass x.args
    , res  = coercePass x.res
    }
