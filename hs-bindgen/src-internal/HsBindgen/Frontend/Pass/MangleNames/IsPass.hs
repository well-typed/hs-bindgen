module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    MangleNames
    -- * Trace messages
  , MangleNamesMsg(..)
  ) where

import HsBindgen.Frontend.AST.External (DeclSpec, NamePair, NewtypeNames,
                                        RecordNames)
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (ResolvedExtBinding)
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mangle names pass
type MangleNames :: Pass
data MangleNames a deriving anyclass (ValidPass)

type family AnnMangleNames ix where
  AnnMangleNames "TranslationUnit"  = SelectDeclMeta
  AnnMangleNames "Decl"             = DeclSpec
  AnnMangleNames "Struct"           = RecordNames
  AnnMangleNames "Union"            = NewtypeNames
  AnnMangleNames "Enum"             = NewtypeNames
  AnnMangleNames "Typedef"          = NewtypeNames
  AnnMangleNames "CheckedMacroType" = NewtypeNames
  AnnMangleNames _                  = NoAnn

instance IsPass MangleNames where
  type Id           MangleNames = (NamePair, C.NameOrigin)
  type FieldName    MangleNames = NamePair
  type ArgumentName MangleNames = Maybe NamePair
  type TypedefRef   MangleNames = RenamedTypedefRef MangleNames
  type MacroBody    MangleNames = CheckedMacro MangleNames
  type ExtBinding   MangleNames = ResolvedExtBinding
  type Ann ix       MangleNames = AnnMangleNames ix
  type Msg          MangleNames = MangleNamesMsg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data MangleNamesMsg =
    MangleNamesCouldNotMangle Text
  | MangleNamesMissingDeclaration C.QualName
  | MangleNamesMissingIdentifier Text
  deriving stock (Show)

instance PrettyForTrace MangleNamesMsg where
  prettyForTrace = \case
      MangleNamesCouldNotMangle name ->
        "Could not mangle C name: " >< textToCtxDoc name
      MangleNamesMissingDeclaration cQualName -> hcat [
          "Missing declaration: '"
        , prettyForTrace cQualName
        , "'; did you select the declaration?"
        ]
      MangleNamesMissingIdentifier name ->
        "Could not mangle C name identifier: " >< textToCtxDoc name

instance IsTrace Level MangleNamesMsg where
  getDefaultLogLevel (MangleNamesMissingIdentifier _) = Warning
  getDefaultLogLevel _                                = Error

  getSource          = const HsBindgen
  getTraceId         = const "mangle-names"
