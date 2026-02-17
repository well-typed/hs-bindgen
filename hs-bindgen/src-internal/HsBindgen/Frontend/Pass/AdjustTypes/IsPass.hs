module HsBindgen.Frontend.Pass.AdjustTypes.IsPass (
    AdjustTypes
  , AdjustedFrom (..)
  ) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Language.C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type AdjustTypes :: Pass
data AdjustTypes a

type family AnnAdjustTypes ix where
  AnnAdjustTypes "TranslationUnit"  = DeclMeta
  AnnAdjustTypes "Decl"             = PrescriptiveDeclSpec
  AnnAdjustTypes "Struct"           = RecordNames
  AnnAdjustTypes "Union"            = NewtypeNames
  AnnAdjustTypes "Enum"             = NewtypeNames
  AnnAdjustTypes "Typedef"          = NewtypeNames
  AnnAdjustTypes "CheckedMacroType" = NewtypeNames
  AnnAdjustTypes "TypeFunArg"       = AdjustedFrom AdjustTypes
  AnnAdjustTypes _                  = NoAnn

instance IsPass AdjustTypes where
  type Id         AdjustTypes = DeclIdPair
  type ScopedName AdjustTypes = ScopedNamePair
  type MacroBody  AdjustTypes = CheckedMacro AdjustTypes
  type ExtBinding AdjustTypes = ResolvedExtBinding
  type Ann ix     AdjustTypes = AnnAdjustTypes ix
  type Msg        AdjustTypes = WithLocationInfo AdjustTypesMsg
  type MacroId    AdjustTypes = Id AdjustTypes

  idNameKind     _ namePair   = namePair.cName.name.kind
  idSourceName   _ namePair   = declIdSourceName namePair.cName
  idLocationInfo _ namePair   = declIdLocationInfo namePair.cName
  extBindingId _ extBinding = extDeclIdPair extBinding
  macroIdId _ = id

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

data AdjustedFrom p =
    AdjustedFromArray (C.Type AdjustTypes)
  | AdjustedFromFunction (C.Type AdjustTypes)
  | NotAdjusted
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data AdjustTypesMsg
  deriving stock Show

instance PrettyForTrace AdjustTypesMsg where
  prettyForTrace = \case

instance IsTrace Level AdjustTypesMsg where
  getDefaultLogLevel = \case
  getSource  = const HsBindgen
  getTraceId = \case

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId Select AdjustTypes
instance CoercePassMacroId Select AdjustTypes
