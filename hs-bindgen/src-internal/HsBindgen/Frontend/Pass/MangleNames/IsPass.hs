module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    MangleNames
    -- * Trace messages
  , MangleNamesMsg(..)
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
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
  type Id           MangleNames = C.DeclIdPair
  type FieldName    MangleNames = C.NamePair
  type ArgumentName MangleNames = Maybe C.NamePair
  type HaskellId    MangleNames = Hs.Identifier
  type MacroBody    MangleNames = C.CheckedMacro MangleNames
  type ExtBinding   MangleNames = ResolvedExtBinding
  type Ann ix       MangleNames = AnnMangleNames ix
  type Msg          MangleNames = MangleNamesMsg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data MangleNamesMsg =
    MangleNamesSquashed (C.DeclInfo Select)
  | MangleNamesRenamed (C.DeclInfo Select) Hs.Identifier
  | MangleNamesCouldNotMangle Text
  | MangleNamesMissingIdentifier Text
  deriving stock (Show)

instance PrettyForTrace MangleNamesMsg where
  prettyForTrace = \case
      MangleNamesSquashed info -> PP.hsep [
          "Squashed typedef"
        , prettyForTrace info
        ]
      MangleNamesRenamed info newName -> PP.hsep [
          "Renamed"
        , prettyForTrace info
        , "to"
        , PP.string (Text.unpack newName.text)
        ]
      MangleNamesCouldNotMangle name -> PP.hsep [
          "Could not mangle C name: "
        , PP.textToCtxDoc name
        ]
      MangleNamesMissingIdentifier name -> PP.hsep [
          "Could not mangle C name identifier: "
        , PP.textToCtxDoc name
        ]

instance IsTrace Level MangleNamesMsg where
  getDefaultLogLevel = \case
      MangleNamesSquashed{}          -> Info
      MangleNamesRenamed{}           -> Info
      MangleNamesCouldNotMangle{}    -> Error
      MangleNamesMissingIdentifier{} -> Warning

  getSource  = const HsBindgen
  getTraceId = const "mangle-names"

