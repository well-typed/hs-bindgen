module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    MangleNames
    -- * Additional names
  , RecordNames(..)
  , NewtypeNames(..)
    -- * Trace messages
  , MangleNamesMsg(..)
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mangle names pass
type MangleNames :: Pass
data MangleNames a

type family AnnMangleNames ix where
  AnnMangleNames "TranslationUnit"  = DeclMeta
  AnnMangleNames "Decl"             =
    (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  AnnMangleNames "Struct"           = RecordNames
  AnnMangleNames "Union"            = NewtypeNames
  AnnMangleNames "Enum"             = NewtypeNames
  AnnMangleNames "Typedef"          = NewtypeNames
  AnnMangleNames "CheckedMacroType" = NewtypeNames
  AnnMangleNames _                  = NoAnn

instance IsPass MangleNames where
  type Id         MangleNames = C.DeclIdPair
  type ScopedName MangleNames = C.ScopedNamePair
  type MacroBody  MangleNames = C.CheckedMacro MangleNames
  type ExtBinding MangleNames = ResolvedExtBinding
  type Ann ix     MangleNames = AnnMangleNames ix
  type Msg        MangleNames = MangleNamesMsg

  idNameKind   _ namePair = namePair.cName.name.kind
  idSourceName _ namePair = C.declIdSourceName namePair.cName

{-------------------------------------------------------------------------------
  Additional names required for Haskell code gen
-------------------------------------------------------------------------------}

-- | Names for a Haskell record type
--
-- This is used in addition to a 'NamePair'.
data RecordNames = RecordNames {
      recordConstr :: Hs.Name Hs.NsConstr
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Names for a Haskell newtype
--
-- This is used in addition to a 'NamePair'.
data NewtypeNames = NewtypeNames {
      newtypeConstr :: Hs.Name Hs.NsConstr
    , newtypeField  :: Hs.Name Hs.NsVar
    }
  deriving stock (Show, Eq, Ord, Generic)

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

