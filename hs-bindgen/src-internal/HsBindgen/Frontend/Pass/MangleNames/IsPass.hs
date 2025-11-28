module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    MangleNames
    -- * Trace messages
  , MangleNamesMsg(..)
  ) where

import Text.SimplePrettyPrint

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mangle names pass
type MangleNames :: Pass
data MangleNames a deriving anyclass (C.ValidPass)

type family AnnMangleNames ix where
  AnnMangleNames "TranslationUnit"  = DeclMeta
  AnnMangleNames "Decl"             =
    (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  AnnMangleNames "Struct"           = C.RecordNames
  AnnMangleNames "Union"            = C.NewtypeNames
  AnnMangleNames "Enum"             = C.NewtypeNames
  AnnMangleNames "Typedef"          = C.NewtypeNames
  AnnMangleNames "CheckedMacroType" = C.NewtypeNames
  AnnMangleNames _                  = NoAnn

instance IsPass MangleNames where
  type Id           MangleNames = C.DeclId MangleNames
  type FieldName    MangleNames = C.NamePair
  type ArgumentName MangleNames = Maybe C.NamePair
  type HaskellId    MangleNames = Hs.Identifier
  type TypedefRef   MangleNames = RenamedTypedefRef MangleNames
  type MacroBody    MangleNames = C.CheckedMacro MangleNames
  type ExtBinding   MangleNames = ResolvedExtBinding
  type Ann ix       MangleNames = AnnMangleNames ix
  type Msg          MangleNames = MangleNamesMsg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data MangleNamesMsg =
    MangleNamesCouldNotMangle Text
  | MangleNamesMissingIdentifier Text
  deriving stock (Show)

instance PrettyForTrace MangleNamesMsg where
  prettyForTrace = \case
      MangleNamesCouldNotMangle name ->
        "Could not mangle C name: " >< textToCtxDoc name
      MangleNamesMissingIdentifier name ->
        "Could not mangle C name identifier: " >< textToCtxDoc name

instance IsTrace Level MangleNamesMsg where
  getDefaultLogLevel = \case
    MangleNamesMissingIdentifier _ -> Warning
    _other                         -> Error
  getSource  = const HsBindgen
  getTraceId = const "mangle-names"
