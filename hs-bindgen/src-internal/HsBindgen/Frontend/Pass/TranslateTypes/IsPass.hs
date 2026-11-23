module HsBindgen.Frontend.Pass.TranslateTypes.IsPass (
    -- * Definition
    TranslateTypes
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.TranslatedMacro
import HsBindgen.IR.AddressStub (AddressStub)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass
import HsBindgen.IR.PassingMethod (PassingMethod, ResPassingMethod)
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type TranslateTypes :: Pass
data TranslateTypes a

type family AnnTranslateTypes ix where
  AnnTranslateTypes "Decl"                = PrescriptiveDeclSpec
  AnnTranslateTypes "Enum"                = NewtypeNames
  AnnTranslateTypes "IndirectField"       = IndirectFieldNames TranslateTypes
  AnnTranslateTypes "Flam"                = FlamNames
  AnnTranslateTypes "Function"            = AddressStub
  AnnTranslateTypes "FunctionArg"         = PassingMethod TranslateTypes
  AnnTranslateTypes "FunctionRes"         = ResPassingMethod TranslateTypes
  AnnTranslateTypes "Global"              = AddressStub
  AnnTranslateTypes "Struct"              = StructNames
  AnnTranslateTypes "Typedef"             = NewtypeNames
  AnnTranslateTypes "TypedefFun"          = Hs.Type
  AnnTranslateTypes "TypedefFunIndirect"  = AuxNames
  AnnTranslateTypes "TranslatedMacroType" = NewtypeNames
  AnnTranslateTypes "Union"               = NewtypeNames
  AnnTranslateTypes _                     = NoAnn

instance IsPass TranslateTypes

instance PassId TranslateTypes where
  type Id TranslateTypes = DeclIdPair

  idNameKind     _ namePair = namePair.cName.name.kind
  idSourceName   _ namePair = C.declIdSourceName namePair.cName
  idLocationInfo _ namePair = C.declIdLocationInfo namePair.cName

instance PassScopedName TranslateTypes where
  type ScopedName TranslateTypes = ScopedNamePair

instance PassTypes TranslateTypes where
  type Types TranslateTypes = TranslatedTypes TranslateTypes
  type TypeFunArg TranslateTypes = TranslatedTypeFunArg TranslateTypes
  type AnonRef TranslateTypes = TranslatedAnonRef TranslateTypes

  cType _ translatedTypes = translatedTypes.c
  cTypeFunArg _ translatedTypeFunArg = translatedTypeFunArg.c
  anonRefTypes _ = translatedAnonRefType

instance PassMacro TranslateTypes where
  type MacroId         TranslateTypes = Id TranslateTypes
  type MacroBody       TranslateTypes = TranslatedMacro TranslateTypes
  type MacroUnderlying TranslateTypes = C.Type TranslateTypes

  macroIdId _ = id

instance PassExtBinding TranslateTypes where
  type ExtBinding TranslateTypes = BindingSpec.ResolvedExtBinding

  extBindingId _ extBinding = extDeclIdPair extBinding

instance PassCommentDecl TranslateTypes where
  type CommentDecl TranslateTypes = Maybe (C.Comment TranslateTypes)

instance PassAnn TranslateTypes where
  type Ann ix TranslateTypes = AnnTranslateTypes ix

instance PassMsg TranslateTypes

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId      MangleNames TranslateTypes
instance CoercePassMacroId MangleNames TranslateTypes

instance CoercePassMacroUnderlying MangleNames TranslateTypes where
  coercePassMacroUnderlying _ = coercePass

instance CoercePassAnn "IndirectField" MangleNames TranslateTypes where
  coercePassAnn _ = \case
      IndirectFieldNames name -> IndirectFieldNames name

instance CoercePassAnn "TypeFunArg" MangleNames TranslateTypes

instance CoercePassCommentDecl MangleNames TranslateTypes where
  coercePassCommentDecl _ = fmap coercePass
