module HsBindgen.Frontend.Pass.TranslateMacros.IsPass (
    -- * Definition
    TranslateMacros
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TranslateMacros.IsPass.Msg (TranslateMacrosMsg)
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassingMethod (PassingMethod,
                                                                    ResPassingMethod)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type TranslateMacros :: Pass
data TranslateMacros a

type family AnnTranslateMacros ix where
  AnnTranslateMacros "Decl"                 = PrescriptiveDeclSpec
  AnnTranslateMacros "Enum"                 = NewtypeNames
  AnnTranslateMacros "IndirectField"        = IndirectFieldNames TranslateMacros
  AnnTranslateMacros "Flam"                 = FlamNames
  AnnTranslateMacros "FunctionArg"          = PassingMethod TranslateMacros
  AnnTranslateMacros "FunctionRes"          = ResPassingMethod TranslateMacros
  AnnTranslateMacros "Struct"               = StructNames
  AnnTranslateMacros "Typedef"              = TypedefNames
  AnnTranslateMacros "TypecheckedMacroType" = NewtypeNames
  AnnTranslateMacros "TypeFunArg"           = AdjustedFrom TranslateMacros
  AnnTranslateMacros "Union"                = NewtypeNames
  AnnTranslateMacros _                      = NoAnn

instance IsPass TranslateMacros

instance PassId TranslateMacros where
  type Id TranslateMacros = DeclIdPair

  idNameKind     _ namePair = namePair.cName.name.kind
  idSourceName   _ namePair = C.declIdSourceName namePair.cName
  idLocationInfo _ namePair = C.declIdLocationInfo namePair.cName

instance PassScopedName TranslateMacros where
  type ScopedName TranslateMacros = ScopedNamePair

instance PassTypes TranslateMacros where
  type Types TranslateMacros = TranslatedTypes TranslateMacros
  type TypeFunArg TranslateMacros = TranslatedTypeFunArg TranslateMacros
  type AnonRef TranslateMacros = TranslatedAnonRef TranslateMacros

  cType _ translatedTypes = translatedTypes.c
  cTypeFunArg _ translatedTypeFunArg = translatedTypeFunArg.c
  anonRefTypes _ = translatedAnonRefType

instance PassMacro TranslateMacros where
  type MacroId         TranslateMacros = Id TranslateMacros
  type MacroBody       TranslateMacros = TypecheckedMacro TranslateMacros
  type MacroUnderlying TranslateMacros = C.Type TranslateMacros

  macroIdId _ = id

instance PassExtBinding TranslateMacros where
  type ExtBinding TranslateMacros = BindingSpec.ResolvedExtBinding

  extBindingId _ extBinding = extDeclIdPair extBinding

instance PassCommentDecl TranslateMacros where
  type CommentDecl TranslateMacros = Maybe (C.Comment TranslateMacros)

instance PassAnn TranslateMacros where
  type Ann ix TranslateMacros = AnnTranslateMacros ix

instance PassMsg TranslateMacros where
  type Msg TranslateMacros = TranslateMacrosMsg

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassAnonRef TranslateTypes TranslateMacros where
  coercePassAnonRef _ = coercePass

instance CoercePassAnn "FunctionArg" TranslateTypes TranslateMacros where
  coercePassAnn _ = coercePass

instance CoercePassAnn "FunctionRes" TranslateTypes TranslateMacros where
  coercePassAnn _ = coercePass

instance CoercePassAnn "IndirectField" TranslateTypes TranslateMacros where
  coercePassAnn _ = coercePass

instance CoercePassAnn "TypeFunArg" TranslateTypes TranslateMacros where
  coercePassAnn _ = coercePass

instance CoercePassCommentDecl TranslateTypes TranslateMacros where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassId      TranslateTypes TranslateMacros

instance CoercePassMacroId TranslateTypes TranslateMacros

instance CoercePassMacroUnderlying TranslateTypes TranslateMacros where
  coercePassMacroUnderlying _ = coercePass

instance CoercePassMacroBody TranslateTypes TranslateMacros where
  coercePassMacroBody _ = coercePassParam

instance CoercePassTypes TranslateTypes TranslateMacros where
  coercePassTypes _ = coercePass

instance CoercePassTypeFunArg TranslateTypes TranslateMacros where
  coercePassTypeFunArg _ = coercePass
