module HsBindgen.Frontend.Pass.AdjustTypes.IsPass (
    AdjustTypes
  , AdjustedFrom (..)
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type AdjustTypes :: Pass
data AdjustTypes a

type family AnnAdjustTypes ix where
  AnnAdjustTypes "Decl"                 = PrescriptiveDeclSpec
  AnnAdjustTypes "Struct"               = StructNames
  AnnAdjustTypes "Union"                = NewtypeNames
  AnnAdjustTypes "UnionField"           = UnionFieldNames
  AnnAdjustTypes "Enum"                 = NewtypeNames
  AnnAdjustTypes "Typedef"              = TypedefNames
  AnnAdjustTypes "TypecheckedMacroType" = NewtypeNames
  AnnAdjustTypes "TypeFunArg"           = AdjustedFrom AdjustTypes
  AnnAdjustTypes _                      = NoAnn

instance IsPass AdjustTypes

instance PassId AdjustTypes where
  type Id AdjustTypes = C.DeclIdPair

  idNameKind     _ namePair = namePair.cName.name.kind
  idSourceName   _ namePair = C.declIdSourceName namePair.cName
  idLocationInfo _ namePair = C.declIdLocationInfo namePair.cName

instance PassScopedName AdjustTypes where
  type ScopedName AdjustTypes = C.ScopedNamePair

instance PassMacro AdjustTypes where
  type MacroId         AdjustTypes = Id AdjustTypes
  type MacroBody       AdjustTypes = TypecheckedMacro AdjustTypes
  type MacroUnderlying AdjustTypes = C.Type AdjustTypes

  macroIdId _ = id

instance PassExtBinding AdjustTypes where
  type ExtBinding AdjustTypes = BindingSpec.ResolvedExtBinding

  extBindingId _ extBinding = BindingSpec.extDeclIdPair extBinding

instance PassCommentDecl AdjustTypes where
  type CommentDecl AdjustTypes = Maybe (C.Comment AdjustTypes)

instance PassAnn AdjustTypes where
  type Ann ix AdjustTypes = AnnAdjustTypes ix

instance PassMsg AdjustTypes

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

data AdjustedFrom p =
    AdjustedFromArray (C.Type p)
  | AdjustedFromFunction (C.Type p)
  | NotAdjusted
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance C.CoercePassId      MangleNames AdjustTypes
instance C.CoercePassMacroId MangleNames AdjustTypes

instance C.CoercePassCommentDecl MangleNames AdjustTypes where
  coercePassCommentDecl _ = fmap C.coercePass
