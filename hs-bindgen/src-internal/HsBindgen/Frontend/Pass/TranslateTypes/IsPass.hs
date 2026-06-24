module HsBindgen.Frontend.Pass.TranslateTypes.IsPass (
    -- * Definition
    TranslateTypes
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type TranslateTypes :: Pass
data TranslateTypes a

type family AnnTranslateTypes ix where
  AnnTranslateTypes "Decl"                 = PrescriptiveDeclSpec
  AnnTranslateTypes "Struct"               = StructNames
  AnnTranslateTypes "Flam"                 = FlamNames
  AnnTranslateTypes "Union"                = NewtypeNames
  AnnTranslateTypes "Enum"                 = NewtypeNames
  AnnTranslateTypes "Typedef"              = TypedefNames
  AnnTranslateTypes "TypecheckedMacroType" = NewtypeNames
  AnnTranslateTypes "TypeFunArg"           = AdjustedFrom TranslateTypes
  AnnTranslateTypes _                      = NoAnn

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

  cType _ translatedTypes = translatedTypes.c

instance PassMacro TranslateTypes where
  type MacroId         TranslateTypes = Id TranslateTypes
  type MacroBody       TranslateTypes = TypecheckedMacro TranslateTypes
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

instance CoercePassId      AdjustTypes TranslateTypes
instance CoercePassMacroId AdjustTypes TranslateTypes

instance CoercePassMacroUnderlying AdjustTypes TranslateTypes where
  coercePassMacroUnderlying _ = coercePass

instance CoercePassMacroBody AdjustTypes TranslateTypes where
  coercePassMacroBody _ = coercePassParam

instance CoercePassAnn "TypeFunArg" AdjustTypes TranslateTypes where
  coercePassAnn _ = \case
    AdjustedFromArray    ty -> AdjustedFromArray    (coercePass ty)
    AdjustedFromFunction ty -> AdjustedFromFunction (coercePass ty)
    NotAdjusted             -> NotAdjusted

instance CoercePassCommentDecl AdjustTypes TranslateTypes where
  coercePassCommentDecl _ = fmap coercePass
