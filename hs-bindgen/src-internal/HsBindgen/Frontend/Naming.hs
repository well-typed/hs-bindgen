-- | C naming and declaration identities
--
-- Intended for qualified import within frontend.
--
-- > import HsBindgen.Frontend.Naming qualified as C
--
-- Use outside of the frontend should be done via
-- "HsBindgen.Frontend.AST.External".
module HsBindgen.Frontend.Naming (
    -- * Name
    Name(..)

    -- * TypeNamespace
  , TypeNamespace(..)

    -- * TagKind
  , TagKind(..)

    -- * NameKind
  , NameKind(..)
  , nameKindTypeNamespace

    -- * QualName
  , QualName(..)
  , qualNameText
  , parseQualName

    -- * AnonId
  , AnonId(..)

    -- * PrelimDeclId
  , PrelimDeclId(..)
  , getPrelimDeclId
    -- ** NsPrelimDeclId
  , NsPrelimDeclId(..)
  , nsPrelimDeclId
    -- ** QualPrelimDeclId
  , QualPrelimDeclId(..)
  , qualPrelimDeclId

    -- * NameOrigin
  , NameOrigin(..)

    -- * DeclId
  , DeclId(..)
    -- ** QualDeclId
  , QualDeclId(..)
  , qualDeclId
  , qualDeclIdText
  , qualDeclIdNsPrelimDeclId

    -- * TaggedTypeId
  , TaggedTypeId(..)

    -- * Located
  , Located(..)
  ) where

import Data.Text qualified as Text

import Clang.HighLevel (ShowFile(..))
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))
import Text.SimplePrettyPrint ((<+>), (><))
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Name
-------------------------------------------------------------------------------}

-- | C name
--
-- This type represents C names, including type names, filed names, variable
-- names, function names, and macro names.
--
-- In type @struct foo@, the name is just @foo@.  The tag kind prefix is /not/
-- part of the name.
newtype Name = Name {
      getName :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)

instance PrettyForTrace Name where
  prettyForTrace (Name name) = "'" >< PP.textToCtxDoc name >< "'"

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
  TagKind
-------------------------------------------------------------------------------}

-- | C tag kind
--
-- This type distinguishes the kinds of C tags.
data TagKind =
    -- | @struct@ tag kind
    TagKindStruct

    -- | @union@ tag kind
  | TagKindUnion

    -- | @enum@ tag kind
  | TagKindEnum
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace TagKind where
  prettyForTrace = PP.showToCtxDoc

tagKindPrefix :: TagKind -> Text
tagKindPrefix = \case
    TagKindStruct -> "struct"
    TagKindUnion  -> "union"
    TagKindEnum   -> "enum"

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

    -- | Tagged kind
    --
    -- A tagged name is written with a prefix that specifies the tag kind.
  | NameKindTagged TagKind
  deriving stock (Show, Eq, Ord, Generic)

instance Bounded NameKind where
  minBound = NameKindOrdinary
  maxBound = NameKindTagged TagKindEnum

instance Enum NameKind where
  toEnum = \case
    0 -> NameKindOrdinary
    1 -> NameKindTagged TagKindStruct
    2 -> NameKindTagged TagKindUnion
    3 -> NameKindTagged TagKindEnum
    _ -> panicPure "invalid NameKind toEnum"

  fromEnum = \case
    NameKindOrdinary             -> 0
    NameKindTagged TagKindStruct -> 1
    NameKindTagged TagKindUnion  -> 2
    NameKindTagged TagKindEnum   -> 3

instance PrettyForTrace NameKind where
  prettyForTrace = PP.showToCtxDoc

-- | Get the 'TypeNamespace' for a 'NameKind'
nameKindTypeNamespace :: NameKind -> TypeNamespace
nameKindTypeNamespace = \case
    NameKindOrdinary -> TypeNamespaceOrdinary
    NameKindTagged{} -> TypeNamespaceTag

nameKindPrefix :: NameKind -> Maybe Text
nameKindPrefix = \case
    NameKindOrdinary       -> Nothing
    NameKindTagged tagKind -> Just (tagKindPrefix tagKind)

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
  prettyForTrace = PP.textToCtxDoc . qualNameText

qualNameText :: QualName -> Text
qualNameText QualName{..} = case nameKindPrefix qualNameKind of
    Nothing     -> getName qualNameName
    Just prefix -> prefix <> " " <> getName qualNameName

parseQualName :: Text -> Maybe QualName
parseQualName t = case Text.words t of
    [n]           -> Just $ QualName (Name n) NameKindOrdinary
    ["struct", n] -> Just $ QualName (Name n) (NameKindTagged TagKindStruct)
    ["union",  n] -> Just $ QualName (Name n) (NameKindTagged TagKindUnion)
    ["enum",   n] -> Just $ QualName (Name n) (NameKindTagged TagKindEnum)
    _otherwise    -> Nothing

{-------------------------------------------------------------------------------
  AnonId
-------------------------------------------------------------------------------}

-- | Identity of an anonymous declaration
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord, Generic)

-- | We mimick the syntax used by clang itself for anonymous declarations
instance PrettyForTrace AnonId where
  prettyForTrace (AnonId loc) = PP.string $
    "unnamed at " ++ HighLevel.prettySingleLoc ShowFile loc

{-------------------------------------------------------------------------------
  PrelimDeclId
-------------------------------------------------------------------------------}

-- | Preliminary declaration identity
--
-- Not all declarations in a C header have names; to be able to nonetheless
-- refer to these declarations we use the source location.  We replace these by
-- proper names in the 'NameAnon' pass.
data PrelimDeclId =
    -- | Named declaration
    PrelimDeclIdNamed Name

    -- | Anonymous declaration
    --
    -- This can only happen for tagged types: structs, unions and enums
  | PrelimDeclIdAnon AnonId

    -- | Built-in declaration
    --
    -- Note: since built-in declarations don't have a definition, we cannot
    -- in general generate bindings for them. If there are /specific/ built-in
    -- declarations we should support, we need to special-case them.
  | PrelimDeclIdBuiltin Name
  deriving stock (Show, Eq, Ord)

instance IsString PrelimDeclId where
  fromString = PrelimDeclIdNamed . fromString

instance PrettyForTrace PrelimDeclId where
  prettyForTrace = \case
    PrelimDeclIdNamed   name   -> prettyForTrace name
    PrelimDeclIdAnon    anonId -> PP.parens (prettyForTrace anonId)
    PrelimDeclIdBuiltin name   -> prettyForTrace name

instance PrettyForTrace (Located PrelimDeclId) where
  prettyForTrace (Located loc prelimDeclId) = case prelimDeclId of
    PrelimDeclIdNamed{} ->
      PP.hsep [prettyForTrace prelimDeclId, "at", PP.showToCtxDoc loc]
    PrelimDeclIdAnon{} ->
      -- No need to repeat the source location in this case
      prettyForTrace prelimDeclId
    PrelimDeclIdBuiltin{} ->
      -- Builtins don't /have/ a location
      prettyForTrace prelimDeclId

getPrelimDeclId :: MonadIO m => CXCursor -> m PrelimDeclId
getPrelimDeclId curr = do
    -- This function distinguishes /anonymous/ and /named/ declarations, but the
    -- Clang meaning of /anonymous/ is different from what we need.  We consider
    -- a @struct@, @union@, or @enum@ declaration /anonymous/ if there is no
    -- tag, even if there is a @typedef@ for the type.
    --
    -- See 'HighLevel.clang_getCursorLocation' for details.
    --
    -- Note that detection of user-provided names that are constructed via
    -- macros only works with @clang >= 19.1.0@ due to a bug in earlier
    -- versions.
    spelling <- HighLevel.clang_getCursorSpelling curr
    case spelling of
      UserProvided name ->
        return $ PrelimDeclIdNamed (Name name)
      ClangGenerated _ ->
        PrelimDeclIdAnon . AnonId . multiLocExpansion
          <$> HighLevel.clang_getCursorLocation curr
      ClangBuiltin name ->
        return $ PrelimDeclIdBuiltin (Name name)

{-------------------------------------------------------------------------------
  NsPrelimDeclId
-------------------------------------------------------------------------------}

-- | Preliminary declaration identity, with named identities qualified by
-- 'TypeNamespace'
--
-- This type is used when names in different namespaces must be distinguished
-- but we do not want to distinguish different tag kinds.
data NsPrelimDeclId =
    NsPrelimDeclIdNamed Name TypeNamespace
  | NsPrelimDeclIdAnon AnonId
  | NsPrelimDeclIdBuiltin Name
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace NsPrelimDeclId where
  prettyForTrace = \case
    NsPrelimDeclIdNamed name ns -> prettyForTrace name >< case ns of
      TypeNamespaceOrdinary -> " (ordinary)"
      TypeNamespaceTag      -> " (tag)"
    NsPrelimDeclIdAnon    anonId -> PP.parens (prettyForTrace anonId)
    NsPrelimDeclIdBuiltin name   -> prettyForTrace name

nsPrelimDeclId :: PrelimDeclId -> TypeNamespace -> NsPrelimDeclId
nsPrelimDeclId prelimDeclId ns = case prelimDeclId of
    PrelimDeclIdNamed   name   -> NsPrelimDeclIdNamed name ns
    PrelimDeclIdAnon    anonId -> NsPrelimDeclIdAnon anonId
    PrelimDeclIdBuiltin name   -> NsPrelimDeclIdBuiltin name

{-------------------------------------------------------------------------------
  QualPrelimDeclId
-------------------------------------------------------------------------------}

-- | Preliminary declaration identity, with named identities qualified by
-- 'NameKind'
--
-- This type is used when names with different tag kinds must be distinguished.
data QualPrelimDeclId =
    QualPrelimDeclIdNamed Name NameKind
  | QualPrelimDeclIdAnon AnonId
  | QualPrelimDeclIdBuiltin Name
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace QualPrelimDeclId where
  prettyForTrace = \case
    QualPrelimDeclIdNamed name kind -> case nameKindPrefix kind of
      Nothing     -> prettyForTrace name
      Just prefix -> PP.textToCtxDoc prefix <+> prettyForTrace name
    QualPrelimDeclIdAnon    anonId -> PP.parens (prettyForTrace anonId)
    QualPrelimDeclIdBuiltin name   -> prettyForTrace name

qualPrelimDeclId :: PrelimDeclId -> NameKind -> QualPrelimDeclId
qualPrelimDeclId prelimDeclId kind = case prelimDeclId of
    PrelimDeclIdNamed   name   -> QualPrelimDeclIdNamed name kind
    PrelimDeclIdAnon    anonId -> QualPrelimDeclIdAnon anonId
    PrelimDeclIdBuiltin name   -> QualPrelimDeclIdBuiltin name

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

    -- | Name is a Clang built-in
  | NameOriginBuiltin
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace NameOrigin where
  prettyForTrace = \case
    NameOriginInSource ->
      PP.string "in source"
    NameOriginGenerated anonId ->
      PP.string "generated for" <+> prettyForTrace anonId
    NameOriginRenamedFrom name ->
      PP.string "renamed from" <+> prettyForTrace name
    NameOriginBuiltin ->
      PP.string "built-in"

{-------------------------------------------------------------------------------
  DeclId
-------------------------------------------------------------------------------}

-- | Declaration identity
--
-- All declarations have names after renaming in the @NameAnon@ pass.  This type
-- is used until the @MangleNames@ pass.
data DeclId = DeclId {
      declIdName   :: Name
    , declIdOrigin :: NameOrigin
    }
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyForTrace DeclId where
  prettyForTrace DeclId{..} =
    prettyForTrace declIdName <+> PP.parens (prettyForTrace declIdOrigin)

instance PrettyForTrace (Located DeclId) where
  prettyForTrace (Located loc DeclId{..}) =
    let details = case declIdOrigin of
          NameOriginBuiltin -> prettyForTrace declIdOrigin
          _otherwise -> PP.hsep [
              prettyForTrace declIdOrigin
            , "at"
            , PP.showToCtxDoc loc
            ]
    in  prettyForTrace declIdName <+> PP.parens details

{-------------------------------------------------------------------------------
  QualDeclId
-------------------------------------------------------------------------------}

-- | Declaration identity, qualified by 'NameKind'
data QualDeclId = QualDeclId {
      qualDeclIdName   :: Name
    , qualDeclIdOrigin :: NameOrigin
    , qualDeclIdKind   :: NameKind
    }
  deriving stock (Eq, Generic, Ord, Show)

qualDeclId :: DeclId -> NameKind -> QualDeclId
qualDeclId DeclId{..} nameKind = QualDeclId {
      qualDeclIdName   = declIdName
    , qualDeclIdOrigin = declIdOrigin
    , qualDeclIdKind   = nameKind
    }

qualDeclIdText :: QualDeclId -> Text
qualDeclIdText QualDeclId{..} = case qualDeclIdOrigin of
    NameOriginGenerated{} -> "anon:" <> getName qualDeclIdName
    _otherwise -> qualNameText $ QualName qualDeclIdName qualDeclIdKind

qualDeclIdNsPrelimDeclId :: QualDeclId -> NsPrelimDeclId
qualDeclIdNsPrelimDeclId QualDeclId{..} = case qualDeclIdOrigin of
    NameOriginInSource             -> NsPrelimDeclIdNamed qualDeclIdName ns
    NameOriginRenamedFrom origName -> NsPrelimDeclIdNamed origName ns
    NameOriginGenerated   anonId   -> NsPrelimDeclIdAnon anonId
    NameOriginBuiltin              -> NsPrelimDeclIdBuiltin qualDeclIdName
  where
    ns :: TypeNamespace
    ns = nameKindTypeNamespace qualDeclIdKind

{-------------------------------------------------------------------------------
  TaggedTypeId
-------------------------------------------------------------------------------}

-- | C tagged type name, tag kind, and origin
data TaggedTypeId = TaggedTypeId {
      taggedTypeIdName   :: Name
    , taggedTypeIdKind   :: TagKind
    , taggedTypeIdOrigin :: NameOrigin
    }
  deriving stock (Eq, Generic, Ord, Show)

{-------------------------------------------------------------------------------
  Located
-------------------------------------------------------------------------------}

-- | Indirection for 'PrettyForTrace' instance for @DeclInfo@
--
-- By introducting this auxiliary type, used in the 'PrettyForTrace' instance
-- for @DeclInfo@, we delegate to @Id p@ instances.
data Located a = Located SingleLoc a
