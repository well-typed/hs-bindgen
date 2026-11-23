module HsBindgen.Frontend.Pass.FunPtrInstances.IsPass (
    FunPtrInstances
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.FunPtrInstances.IsPass.Msg
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.TranslatedMacro (TranslatedMacro)
import HsBindgen.IR.AddressStub (AddressStub)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass
import HsBindgen.IR.PassingMethod (PassingMethod, ResPassingMethod)
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type FunPtrInstances :: Pass
data FunPtrInstances a

type family AnnFunPtrInstances ix where
  AnnFunPtrInstances "Decl"                = PrescriptiveDeclSpec
  AnnFunPtrInstances "Enum"                = NewtypeNames
  AnnFunPtrInstances "Flam"                = FlamNames
  AnnFunPtrInstances "Function"            = AddressStub
  AnnFunPtrInstances "FunctionArg"         = PassingMethod FunPtrInstances
  AnnFunPtrInstances "FunctionRes"         = ResPassingMethod FunPtrInstances
  AnnFunPtrInstances "Global"              = AddressStub
  AnnFunPtrInstances "IndirectField"       = IndirectFieldNames FunPtrInstances
  AnnFunPtrInstances "Struct"              = StructNames
  AnnFunPtrInstances "Typedef"             = NewtypeNames
  AnnFunPtrInstances "TypedefFun"          = Hs.Type
  AnnFunPtrInstances "TypedefFunIndirect"  = AuxNames
  AnnFunPtrInstances "TranslatedMacroType" = NewtypeNames
  AnnFunPtrInstances "Union"               = NewtypeNames
  AnnFunPtrInstances _                     = NoAnn

instance IsPass FunPtrInstances

instance PassId FunPtrInstances where
  type Id FunPtrInstances = DeclIdPair

  idNameKind     _ namePair = namePair.cName.name.kind
  idSourceName   _ namePair = C.declIdSourceName namePair.cName
  idLocationInfo _ namePair = C.declIdLocationInfo namePair.cName

instance PassScopedName FunPtrInstances where
  type ScopedName FunPtrInstances = ScopedNamePair

instance PassTypes FunPtrInstances where
  type Types      FunPtrInstances = TranslatedTypes      FunPtrInstances
  type TypeFunArg FunPtrInstances = TranslatedTypeFunArg FunPtrInstances
  type AnonRef    FunPtrInstances = TranslatedAnonRef    FunPtrInstances

  cType _ translatedTypes = translatedTypes.c
  cTypeFunArg _ translatedTypeFunArg = translatedTypeFunArg.c
  anonRefTypes _ = translatedAnonRefType

instance PassMacro FunPtrInstances where
  type MacroId         FunPtrInstances = Id FunPtrInstances
  type MacroBody       FunPtrInstances = TranslatedMacro FunPtrInstances
  type MacroUnderlying FunPtrInstances = C.Type FunPtrInstances

  macroIdId _ = id

instance PassExtBinding FunPtrInstances where
  type ExtBinding FunPtrInstances = BindingSpec.ResolvedExtBinding

  extBindingId _ extBinding = extDeclIdPair extBinding

instance PassCommentDecl FunPtrInstances where
  type CommentDecl FunPtrInstances = Maybe (C.Comment FunPtrInstances)

instance PassAnn FunPtrInstances where
  type Ann ix FunPtrInstances = AnnFunPtrInstances ix

instance PassMsg FunPtrInstances where
  type Msg FunPtrInstances = C.WithLocationInfo FunPtrInstancesMsg

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassAnonRef Select FunPtrInstances where
  coercePassAnonRef _ = coercePass

instance CoercePassId Select FunPtrInstances

instance CoercePassTypes Select FunPtrInstances where
  coercePassTypes _ = coercePass

instance CoercePassTypeFunArg Select FunPtrInstances where
  coercePassTypeFunArg _ = coercePass

instance CoercePassMacroId Select FunPtrInstances

instance CoercePassMacroUnderlying Select FunPtrInstances where
  coercePassMacroUnderlying _ = coercePass

instance CoercePassMacroBody Select FunPtrInstances where
  coercePassMacroBody _ = coercePassParam

instance CoercePassAnn "FunctionArg" Select FunPtrInstances where
  coercePassAnn _ = coercePass

instance CoercePassAnn "FunctionRes" Select FunPtrInstances where
  coercePassAnn _ = coercePass

instance CoercePassAnn "IndirectField" Select FunPtrInstances where
  coercePassAnn _ = coercePass

instance CoercePassAnn "TypeFunArg" Select FunPtrInstances

instance CoercePassCommentDecl Select FunPtrInstances where
  coercePassCommentDecl _ = fmap coercePass
