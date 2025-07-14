-- | C name types
--
-- Not intended for direct import; import via @HsBindgen.Language.C@.
module HsBindgen.Language.C.Name (
    -- * Name
    Name(..)

    -- * NameKind
  , NameKind(..)

    -- * QualName
  , QualName(..)
  , qualNameText
  , parseQualName
  ) where

import Data.Text qualified as Text

import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))
import Text.SimplePrettyPrint (showToCtxDoc, textToCtxDoc, (><))

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
