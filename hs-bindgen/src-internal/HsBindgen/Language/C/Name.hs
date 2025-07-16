-- | C name types
--
-- Not intended for direct import; import via @HsBindgen.Language.C@.
module HsBindgen.Language.C.Name (
    -- * Name
    Name(..)

    -- * TypeNamespace
  , TypeNamespace(..)

    -- * NameKind
  , NameKind(..)
  , nameKindTypeNamespace

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
newtype Name = Name {
      getName :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)

instance PrettyForTrace Name where
  prettyForTrace (Name name) = "'" >< textToCtxDoc name >< "'"

{-------------------------------------------------------------------------------
  TypeNamespace
-------------------------------------------------------------------------------}

-- | C type namespaces
--
-- C namespaces include:
--
-- * Ordinary namespace: a single namespace that includes @typedef@ names,
--   variable names, function names, @enum@ constant names, etc.
-- * Tag namespace: a single namespace that includes all @struct@, @union@, and
--   @enum@ tags
-- * Field namespaces: a separate namespace per @struct@/@union@ of field names
-- * Label namespace: a single namespace of @goto@ labels
--
-- A type name is in the ordinary or tag namespace.
data TypeNamespace =
    TypeNamespaceOrdinary
  | TypeNamespaceTag
  deriving stock (Show, Eq, Ord, Generic)

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

-- | Get the 'TypeNamespace' for a 'NameKind'
nameKindTypeNamespace :: NameKind -> TypeNamespace
nameKindTypeNamespace = \case
    NameKindOrdinary -> TypeNamespaceOrdinary
    NameKindStruct   -> TypeNamespaceTag
    NameKindUnion    -> TypeNamespaceTag
    NameKindEnum     -> TypeNamespaceTag

{-------------------------------------------------------------------------------
  QualName
-------------------------------------------------------------------------------}

-- | C name, qualified by the 'NameKind'
--
-- This is the parsed representation of a @libclang@ C spelling.
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
