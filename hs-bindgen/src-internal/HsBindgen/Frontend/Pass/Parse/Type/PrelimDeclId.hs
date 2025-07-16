module HsBindgen.Frontend.Pass.Parse.Type.PrelimDeclId (
    -- * PrelimDeclId
    PrelimDeclId(..)
  , getPrelimDeclId

    -- * NsPrelimDeclId
  , NsPrelimDeclId(..)
  , nsPrelimDeclId
  , declNsPrelimDeclId

    -- * QualPrelimDeclId
  , QualPrelimDeclId(..)
  , qualPrelimDeclId
  , declQualPrelimDeclId
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

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
    PrelimDeclIdNamed C.Name

    -- | Anonymous declaration
    --
    -- This can only happen for tagged types: structs, unions and enums
  | PrelimDeclIdAnon AnonId

    -- | Built-in declaration
    --
    -- Note: since built-in declarations don't have a definition, we cannot
    -- in general generate bindings for them. If there are /specific/ built-in
    -- declarations we should support, we need to special-case them.
  | PrelimDeclIdBuiltin C.Name
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
        return $ PrelimDeclIdNamed (C.Name name)
      ClangGenerated _ ->
        PrelimDeclIdAnon . AnonId . multiLocExpansion
          <$> HighLevel.clang_getCursorLocation curr
      ClangBuiltin name ->
        return $ PrelimDeclIdBuiltin (C.Name name)

{-------------------------------------------------------------------------------
  NsPrelimDeclId
-------------------------------------------------------------------------------}

-- | Preliminary declaration identity, with named identities qualified by
-- 'C.TypeNamespace'
--
-- This type is used when names in different namespaces must be distinguished
-- but we do not want to distinguish different tag kinds.
data NsPrelimDeclId =
    NsPrelimDeclIdNamed C.Name C.TypeNamespace
  | NsPrelimDeclIdAnon AnonId
  | NsPrelimDeclIdBuiltin C.Name
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace NsPrelimDeclId where
  prettyForTrace = \case
    NsPrelimDeclIdNamed name ns -> prettyForTrace name >< case ns of
      C.TypeNamespaceOrdinary -> " (ordinary)"
      C.TypeNamespaceTag      -> " (tag)"
    NsPrelimDeclIdAnon    anonId -> PP.parens (prettyForTrace anonId)
    NsPrelimDeclIdBuiltin name   -> prettyForTrace name

nsPrelimDeclId :: PrelimDeclId -> C.TypeNamespace -> NsPrelimDeclId
nsPrelimDeclId prelimDeclId ns = case prelimDeclId of
    PrelimDeclIdNamed   name   -> NsPrelimDeclIdNamed name ns
    PrelimDeclIdAnon    anonId -> NsPrelimDeclIdAnon anonId
    PrelimDeclIdBuiltin name   -> NsPrelimDeclIdBuiltin name

declNsPrelimDeclId :: Id p ~ PrelimDeclId => C.Decl p -> NsPrelimDeclId
declNsPrelimDeclId C.Decl{declInfo = C.DeclInfo{declId}, declKind} =
    nsPrelimDeclId declId $
      C.nameKindTypeNamespace (C.declKindNameKind declKind)

{-------------------------------------------------------------------------------
  QualPrelimDeclId
-------------------------------------------------------------------------------}

-- | Preliminary declaration identity, with named identities qualified by
-- 'C.NameKind'
--
-- This type is used when names with different tag kinds must be distinguished.
data QualPrelimDeclId =
    QualPrelimDeclIdNamed C.Name C.NameKind
  | QualPrelimDeclIdAnon AnonId
  | QualPrelimDeclIdBuiltin C.Name
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace QualPrelimDeclId where
  prettyForTrace = \case
    QualPrelimDeclIdNamed name kind ->
      let prefix = case kind of
            C.NameKindOrdinary -> ""
            C.NameKindStruct   -> "struct "
            C.NameKindUnion    -> "union "
            C.NameKindEnum     -> "enum "
      in  prefix >< prettyForTrace name
    QualPrelimDeclIdAnon    anonId -> PP.parens (prettyForTrace anonId)
    QualPrelimDeclIdBuiltin name   -> prettyForTrace name

qualPrelimDeclId :: PrelimDeclId -> C.NameKind -> QualPrelimDeclId
qualPrelimDeclId prelimDeclId kind = case prelimDeclId of
    PrelimDeclIdNamed   name   -> QualPrelimDeclIdNamed name kind
    PrelimDeclIdAnon    anonId -> QualPrelimDeclIdAnon anonId
    PrelimDeclIdBuiltin name   -> QualPrelimDeclIdBuiltin name

declQualPrelimDeclId :: Id p ~ PrelimDeclId => C.Decl p -> QualPrelimDeclId
declQualPrelimDeclId C.Decl{declInfo = C.DeclInfo{declId}, declKind} =
    qualPrelimDeclId declId (C.declKindNameKind declKind)
