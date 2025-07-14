-- | C name types
--
-- Not intended for direct import; import via @HsBindgen.Language.C@.
module HsBindgen.Language.C.Name (
    -- * Types
    AnonId(..)
  , Name(..)
  , NameKind(..)
  , QualName(..)
  , qualNameText
  , parseQualName
  , NameOrigin(..)
  ) where

import Data.Text qualified as Text

import Clang.HighLevel (ShowFile (..))
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (SingleLoc)
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))
import Text.SimplePrettyPrint (showToCtxDoc, textToCtxDoc, (><))

{-------------------------------------------------------------------------------
  AnonId
-------------------------------------------------------------------------------}

-- | Identity of an anonymous declaration
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord, Generic)

-- | We mimick the syntax used by clang itself for anonymous declarations
instance PrettyForTrace AnonId where
  prettyForTrace (AnonId loc) = fromString $ concat [
        "(unnamed at "
      , HighLevel.prettySingleLoc ShowFile loc
      , ")"
      ]

{-------------------------------------------------------------------------------
  Name
-------------------------------------------------------------------------------}

-- | C name
--
-- This type represents a C name, with no prefix.
--
-- Example: @foo@
newtype Name = Name {
      getName :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)

instance PrettyForTrace Name where
  prettyForTrace (Name name) = "'" >< textToCtxDoc name >< "'"

{-------------------------------------------------------------------------------
  NameKind
-------------------------------------------------------------------------------}

-- | C name kind
--
-- This type distinguishes ordinary names and tagged names.  It is needed when
-- the kind is not determined by a context.
data NameKind =
    -- | Ordinary kind
    --
    -- An ordinary name is written without a prefix.
    NameKindOrdinary

    -- | Struct kind
    --
    -- A struct name is written with a @struct@ prefix.
  | NameKindStruct

    -- | Union kind
    --
    -- A union name is written with a @union@ prefix.
  | NameKindUnion

    -- | Enum kind
    --
    -- An enum name is written with an @enum@ prefix.
  | NameKindEnum
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance PrettyForTrace NameKind where
  prettyForTrace = showToCtxDoc

{-------------------------------------------------------------------------------
  QualName
-------------------------------------------------------------------------------}

data QualName = QualName {
      qualNameName :: Name
    , qualNameKind :: NameKind
    }
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace QualName where
  prettyForTrace = textToCtxDoc . qualNameText

qualNameText :: QualName -> Text
qualNameText QualName{..} =
    let prefix = case qualNameKind of
          NameKindOrdinary -> ""
          NameKindStruct   -> "struct "
          NameKindUnion    -> "union "
          NameKindEnum     -> "enum "
    in  prefix <> getName qualNameName

parseQualName :: Text -> Maybe QualName
parseQualName t = case Text.words t of
    [n]           -> Just $ QualName (Name n) NameKindOrdinary
    ["struct", n] -> Just $ QualName (Name n) NameKindStruct
    ["union",  n] -> Just $ QualName (Name n) NameKindUnion
    ["enum",   n] -> Just $ QualName (Name n) NameKindEnum
    _otherwise    -> Nothing

{-------------------------------------------------------------------------------
  NameOrigin
-------------------------------------------------------------------------------}

-- | C name origin
--
-- This type describes the origin of a C name.
data NameOrigin =
    -- | Name in source
    --
    -- The name may be used to construct a valid C type.
    NameOriginInSource

    -- | Name is generated
    --
    -- The name may not be used to construct a valid C type.
  | NameOriginGenerated AnonId

    -- | Name is renamed
    --
    -- The name may not be used to construct a valid C type, but this original
    -- name may be used to construct a valid C type.
  | NameOriginRenamedFrom Name
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace NameOrigin where
  prettyForTrace = showToCtxDoc
