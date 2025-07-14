module HsBindgen.Frontend.Pass.Parse.Type.PrelimDeclId (
    -- * PrelimDeclId
    PrelimDeclId(..)
  , getPrelimDeclId

    -- * QualPrelimDeclId
  , QualPrelimDeclId(..)
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
-- proper names in the 'RenameAnon' pass.
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

instance PrettyForTrace (C.Located PrelimDeclId) where
  prettyForTrace (C.Located loc prelimDeclId) = case prelimDeclId of
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
    -- @clang_Cursor_isAnonymous@ does not do what we need.  It returns 'False'
    -- for an anonymous declaration if there is a @typedef@ for the type.  In
    -- older versions of LLVM, one could check @clang_getCursorSpelling@ for an
    -- empty result, but this has changed in later versions of LLVM.
    --
    -- See https://github.com/well-typed/hs-bindgen/issues/795
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
  QualPrelimDeclId
-------------------------------------------------------------------------------}

data QualPrelimDeclId = QualPrelimDeclId {
      qualPrelimDeclId   :: PrelimDeclId
    , qualPrelimDeclKind :: C.NameKind
    }
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace QualPrelimDeclId where
  prettyForTrace (QualPrelimDeclId prelimDeclId cNameKind) =
    let prefix = case cNameKind of
          C.NameKindOrdinary -> ""
          C.NameKindStruct   -> "struct "
          C.NameKindUnion    -> "union "
          C.NameKindEnum     -> "enum "
    in  prefix >< prettyForTrace prelimDeclId

declQualPrelimDeclId :: Id p ~ PrelimDeclId => C.Decl p -> QualPrelimDeclId
declQualPrelimDeclId C.Decl{declInfo = C.DeclInfo{declId}, declKind} =
    QualPrelimDeclId declId (C.declKindNameKind declKind)
