-- | C naming and declaration identifiers
--
-- This module defines many types that are used to identify C declarations in
-- various contexts.  A goal is to make invalid state unrepresentable.  For
-- example, if the 'NameKind' is determined by some context, then the identifier
-- type must not contain a 'NameKind' because it would be possible to have a
-- value that does not match the context.  Having many types can be confusing,
-- but it is required for this design goal.
--
-- An identifier must include a 'NameKind' to uniquely identify a declaration
-- without context.  Terminology /qualified/ indicates that a type includes a
-- 'NameKind'.  Such types have a @Qual@ prefix.
--
-- At a high level, there are three types of identifiers:
--
-- * /Names/ are general identifiers that can be parsed from user input
--   (strings).  For example, @struct foo@ can be parsed to a 'QualName', which
--   uniquely identifies a declaration without context.
-- * /Preliminary declaration IDs/ are assigned to declarations when they are
--   parsed.  Anonymous declarations are identified using 'AnonId', which
--   includes the source location, so such IDs /cannot/ be parsed from user
--   input.  Type 'QualPrelimDeclId' uniquely identifies a declaration without
--   context.
-- * /Declaration IDs/ are assigned to declarations when anonymous declarations
--   are named.  Types 'NameOrigin' provides information needed to determine the
--   original preliminary declaration ID.  Declaration IDs therefore include
--   source locations, so they /cannot/ be parsed from user input.  Type
--   'QualDeclId' uniquely identifies a declaration without context.
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

    -- * TagKind
  , TagKind(..)

    -- * NameKind
  , NameKind(..)

    -- * QualName
  , QualName(..)
  , qualNameText
  , parseQualName

    -- * AnonId
  , AnonId(..)

    -- * PrelimDeclId
  , PrelimDeclId(..)
  , prelimDeclIdName
  , getPrelimDeclId

    -- * DeclId
  , DeclId(..)
  , DeclIdName(..)
  , OrigDeclId(..)
  , declIdName
  , declIdQualName
  , unsafeDeclIdHaskellName
  , declIdCName

    -- * Located
  , Located(..)
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint (CtxDoc, (<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel (ShowFile (..))
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  Name
-------------------------------------------------------------------------------}

-- | C name
--
-- This type represents C names, including type names, field names, variable
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
  prettyForTrace (Name name) = PP.singleQuotes $ PP.textToCtxDoc name

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
  prettyForTrace = PP.singleQuotes . PP.textToCtxDoc . qualNameText

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

-- | Anonymous declaration identifier
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord, Generic)

-- | We mimic the syntax used by Clang itself for anonymous declarations
instance PrettyForTrace AnonId where
  prettyForTrace (AnonId loc) = PP.string $
    "unnamed at " ++ HighLevel.prettySingleLoc ShowFile loc

{-------------------------------------------------------------------------------
  PrelimDeclId
-------------------------------------------------------------------------------}

-- | Preliminary declaration identifier
--
-- Not all declarations in a C header have names; to be able to nonetheless
-- refer to these declarations we use the source location.  We replace these by
-- proper names in the 'NameAnon' pass.
data PrelimDeclId =
    -- | Named declaration
    PrelimDeclIdNamed Name NameKind

    -- | Anonymous declaration
    --
    -- This can only happen for tagged types: structs, unions and enums
  | PrelimDeclIdAnon AnonId NameKind

    -- | Built-in declaration
    --
    -- Note: since builtin declarations don't have a definition, we cannot
    -- in general generate bindings for them.  If there are /specific/ builtin
    -- declarations we should support, we need to special-case them.
  | PrelimDeclIdBuiltin Name NameKind
  deriving stock (Show, Eq, Ord)

prelimDeclIdName :: PrelimDeclId -> Maybe Name
prelimDeclIdName = \case
    PrelimDeclIdNamed   name   _kind -> Just name
    PrelimDeclIdAnon   _anonId _kind -> Nothing
    PrelimDeclIdBuiltin name   _kind -> Just name

instance PrettyForTrace PrelimDeclId where
  prettyForTrace = \case
      PrelimDeclIdNamed n k ->
        prettyForTrace (QualName n k)
      PrelimDeclIdAnon anonId kind ->
        PP.singleQuotes $
          case kind of
            NameKindTagged kind' ->
                  PP.textToCtxDoc (tagKindPrefix kind')
              <+> PP.parens (prettyForTrace anonId)
            NameKindOrdinary ->
              panicPure "unexpected anonymous ordinary name"
      PrelimDeclIdBuiltin n k ->
        prettyForTrace (QualName n k)

instance PrettyForTrace (Located PrelimDeclId) where
  prettyForTrace (Located l i) =
      case i of
        PrelimDeclIdNamed n k ->
          prettyForTraceLoc (QualName n k) l
        PrelimDeclIdAnon{}  ->
          -- No need to repeat the source location in this case
          prettyForTrace i
        PrelimDeclIdBuiltin n k ->
          prettyForTraceLoc (QualName n k) l

getPrelimDeclId :: MonadIO m => CXCursor -> NameKind -> m PrelimDeclId
getPrelimDeclId curr nameKind = do
    -- TODO: <https://github.com/well-typed/hs-bindgen/pull/1292>
    -- Update this comment.
    --
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
        return $ PrelimDeclIdNamed (Name name) nameKind
      ClangGenerated _ -> do
        anonId <- AnonId . multiLocExpansion
                    <$> HighLevel.clang_getCursorLocation curr
        return $ PrelimDeclIdAnon anonId nameKind
      ClangBuiltin name ->
        return $ PrelimDeclIdBuiltin (Name name) nameKind

{-------------------------------------------------------------------------------
  DeclId

  TODO <https://github.com/well-typed/hs-bindgen/issues/1267>
  Should 'NamedDeclId' and 'BuiltinDeclId' use 'QualName'?
  Or should 'Name' just always be qualified itself?
-------------------------------------------------------------------------------}

-- | Declaration identifier
--
-- All declarations have names after renaming in the @NameAnon@ pass.  This type
-- is used until the @MangleNames@ pass.
data DeclId (p :: Pass) = DeclId{
      name       :: DeclIdName
    , nameKind   :: NameKind
    , origDeclId :: OrigDeclId
    , haskellId  :: HaskellId p
    }

deriving instance Show (HaskellId p) => Show (DeclId p)
deriving instance Eq   (HaskellId p) => Eq   (DeclId p)
deriving instance Ord  (HaskellId p) => Ord  (DeclId p)

data DeclIdName =
    DeclIdNamed Name
  | DeclIdBuiltin Name
  deriving stock (Show, Eq, Ord)

data OrigDeclId =
    -- | The original ID (as it appeared in the C source)
    OrigDeclId PrelimDeclId

    -- | Auxiliary declaration
    --
    -- In some cases we introduce auxiliary declarations that do not exist in
    -- the C source. In this case, we record which original declaration this
    -- declaration is in support of.
  | AuxForDecl PrelimDeclId
  deriving stock (Show, Eq, Ord)

declIdName :: DeclId p -> Name
declIdName = aux . (.name)
  where
    aux :: DeclIdName -> Name
    aux (DeclIdNamed   name) = name
    aux (DeclIdBuiltin name) = name

declIdQualName :: DeclId p -> QualName
declIdQualName declId = QualName (declIdName declId) declId.nameKind

-- | Construct name in arbitrary name space
--
-- The caller must ensure that name rules are adhered to.
unsafeDeclIdHaskellName :: HaskellId p ~ Hs.Identifier => DeclId p -> Hs.Name ns
unsafeDeclIdHaskellName declId =
    case declId.haskellId of
      Hs.Identifier name -> Hs.Name name

-- | How do we refer to this declaration in C code?
declIdCName :: DeclId p -> Maybe QualName
declIdCName declId =
   case declId.origDeclId of
     AuxForDecl _parent -> Nothing
     OrigDeclId orig    ->
       case orig of
         PrelimDeclIdNamed   name kind -> Just $ QualName name kind
         PrelimDeclIdBuiltin name kind -> Just $ QualName name kind
         PrelimDeclIdAnon{}            -> Nothing

instance PrettyForTrace (DeclId p) where
   prettyForTrace declId = PP.hsep [
         prettyForTrace (declIdQualName declId)
       , case declId.origDeclId of
           OrigDeclId orig | prelimDeclIdName orig /= Just (declIdName declId) ->
             PP.parens (prettyForTrace orig)
           _otherwise ->
             PP.empty
       ]

instance PrettyForTrace (Located (DeclId p)) where
  prettyForTrace (Located loc declId) =
      prettyForTraceLoc declId loc

{-------------------------------------------------------------------------------
  Located
-------------------------------------------------------------------------------}

-- | Indirection for 'PrettyForTrace' instance for @DeclInfo@
--
-- By introducing this auxiliary type, used in the 'PrettyForTrace' instance
-- for @DeclInfo@, we delegate to @Id p@ instances.
data Located a = Located SingleLoc a

prettyForTraceLoc :: PrettyForTrace a => a -> SingleLoc -> CtxDoc
prettyForTraceLoc x l = PP.hsep [
      prettyForTrace x
    , "at"
    , PP.showToCtxDoc l
    ]
