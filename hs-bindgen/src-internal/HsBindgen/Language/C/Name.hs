module HsBindgen.Language.C.Name (
    -- * Types
    AnonId(..)
  , CName(..)
  , NameKind(..)
  , QualName(..)
  , qualNameText
  , parseQualName
  , NameOrigin(..)
  ) where

import Data.Text qualified as Text

import Clang.HighLevel.Types (SingleLoc)
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  AnonId
-------------------------------------------------------------------------------}

-- | Identity of an anonymous declaration
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace AnonId where
  prettyForTrace (AnonId loc) = "<" ++ show loc ++ ">"

{-------------------------------------------------------------------------------
  CName
-------------------------------------------------------------------------------}

-- TODO Rename CName to Name, for qualified import

-- | C name
--
-- This type represents a C name, with no prefix.
--
-- Example: @foo@
newtype CName = CName {
      getCName :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)

instance PrettyForTrace CName where
  prettyForTrace = Text.unpack . getCName

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
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace NameKind where
  prettyForTrace = show

{-------------------------------------------------------------------------------
  QualName
-------------------------------------------------------------------------------}

data QualName = QualName {
      qualNameName :: CName
    , qualNameKind :: NameKind
    }
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace QualName where
  prettyForTrace = Text.unpack . qualNameText

qualNameText :: QualName -> Text
qualNameText QualName{..} =
    let prefix = case qualNameKind of
          NameKindOrdinary -> ""
          NameKindStruct   -> "struct "
          NameKindUnion    -> "union "
          NameKindEnum     -> "enum "
    in  prefix <> getCName qualNameName

parseQualName :: Text -> Maybe QualName
parseQualName t = case Text.words t of
    [n]           -> Just $ QualName (CName n) NameKindOrdinary
    ["struct", n] -> Just $ QualName (CName n) NameKindStruct
    ["union",  n] -> Just $ QualName (CName n) NameKindUnion
    ["enum",   n] -> Just $ QualName (CName n) NameKindEnum
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
  | NameOriginRenamedFrom CName
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace NameOrigin where
  prettyForTrace = show
