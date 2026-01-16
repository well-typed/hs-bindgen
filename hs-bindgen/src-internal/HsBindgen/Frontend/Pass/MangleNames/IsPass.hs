module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    MangleNames
    -- * Additional names
  , RecordNames(..)
  , NewtypeNames(..)
    -- * Trace messages
  , MangleNamesMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Frontend.Analysis.Typedefs
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
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
  AnnMangleNames "Decl"             = PrescriptiveDeclSpec
  AnnMangleNames "Struct"           = RecordNames
  AnnMangleNames "Union"            = NewtypeNames
  AnnMangleNames "Enum"             = NewtypeNames
  AnnMangleNames "Typedef"          = NewtypeNames
  AnnMangleNames "CheckedMacroType" = NewtypeNames
  AnnMangleNames _                  = NoAnn

instance IsPass MangleNames where
  type Id         MangleNames = DeclIdPair
  type ScopedName MangleNames = ScopedNamePair
  type MacroBody  MangleNames = CheckedMacro MangleNames
  type ExtBinding MangleNames = ResolvedExtBinding
  type Ann ix     MangleNames = AnnMangleNames ix
  type Msg        MangleNames = WithLocationInfo MangleNamesMsg
  type MacroId    MangleNames = Id MangleNames

  idNameKind     _ namePair   = namePair.cName.name.kind
  idSourceName   _ namePair   = declIdSourceName namePair.cName
  idLocationInfo _ namePair   = declIdLocationInfo namePair.cName
  extBindingId   _ extBinding = extDeclIdPair extBinding
  macroIdId _ = id

{-------------------------------------------------------------------------------
  Additional names required for Haskell code gen
-------------------------------------------------------------------------------}

-- | Names for a Haskell record type
--
-- This is used in addition to a 'NamePair'.
data RecordNames = RecordNames {
      constr :: Hs.Name Hs.NsConstr
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Names for a Haskell newtype
--
-- This is used in addition to a 'NamePair'.
data NewtypeNames = NewtypeNames {
      constr :: Hs.Name Hs.NsConstr
    , field  :: Hs.Name Hs.NsVar
    }
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data MangleNamesMsg =
    MangleNamesRenamed Hs.Identifier
  | MangleNamesSquashed Squash
  deriving stock (Show)

instance PrettyForTrace MangleNamesMsg where
  prettyForTrace = \case
      MangleNamesRenamed newName -> PP.hsep [
          "Renamed to"
        , PP.text newName.text
        ]
      MangleNamesSquashed s -> PP.hsep [
          "Squashed typedef to"
        , prettyForTrace s.targetId
        ]

instance IsTrace Level MangleNamesMsg where
  getDefaultLogLevel = \case
      MangleNamesRenamed{}           -> Info
      MangleNamesSquashed{}          -> Notice

  getSource  = const HsBindgen
  getTraceId = const "mangle-names"
