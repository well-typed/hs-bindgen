module HsBindgen.Frontend.Pass.AdjustTypes.IsPass (
    AdjustTypes
  , AdjustedFrom (..)
  ) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type AdjustTypes :: Pass
data AdjustTypes a

type family AnnAdjustTypes ix where
  AnnAdjustTypes "TranslationUnit"  = DeclMeta
  AnnAdjustTypes "Decl"             = PrescriptiveDeclSpec
  AnnAdjustTypes "Struct"           = StructNames
  AnnAdjustTypes "Union"            = NewtypeNames
  AnnAdjustTypes "UnionField"       = UnionFieldNames
  AnnAdjustTypes "Enum"             = NewtypeNames
  AnnAdjustTypes "Typedef"          = TypedefNames
  AnnAdjustTypes "CheckedMacroType" = NewtypeNames
  AnnAdjustTypes "TypeFunArg"       = AdjustedFrom AdjustTypes
  AnnAdjustTypes _                  = NoAnn

instance IsPass AdjustTypes where
  type Id          AdjustTypes = DeclIdPair
  type ScopedName  AdjustTypes = ScopedNamePair
  type MacroBody   AdjustTypes = CheckedMacro AdjustTypes
  type ExtBinding  AdjustTypes = ResolvedExtBinding
  type Ann ix      AdjustTypes = AnnAdjustTypes ix
  type Msg         AdjustTypes = NoMsg Level
  type MacroId     AdjustTypes = Id AdjustTypes
  type CommentDecl AdjustTypes = Maybe (C.Comment AdjustTypes)

  idNameKind     _ namePair = namePair.cName.name.kind
  idSourceName   _ namePair = declIdSourceName namePair.cName
  idLocationInfo _ namePair = declIdLocationInfo namePair.cName
  extBindingId _ extBinding = extDeclIdPair extBinding
  macroIdId _ = id

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

instance CoercePassId      MangleNames AdjustTypes
instance CoercePassMacroId MangleNames AdjustTypes

instance CoercePassCommentDecl MangleNames AdjustTypes where
  coercePassCommentDecl _ = fmap coercePass
