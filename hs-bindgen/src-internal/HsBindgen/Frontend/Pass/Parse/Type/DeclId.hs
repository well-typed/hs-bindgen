module HsBindgen.Frontend.Pass.Parse.Type.DeclId (
    DeclId (..)
  , isNamedDecl
  , isAnonDecl
  , getDeclId
  , QualDeclId (..)
  , declQualDeclId
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((><))

{-------------------------------------------------------------------------------
  Identity

  Not all declarations in a C header have names; to be able to nonetheless refer
  to these declarations we use the source location. We replace these by proper
  names in the 'RenameAnon' pass.
-------------------------------------------------------------------------------}

-- | Identity of a declaration
data DeclId =
    DeclNamed CName
  | DeclAnon AnonId
  deriving stock (Show, Eq, Ord)

isNamedDecl :: DeclId -> Maybe CName
isNamedDecl (DeclNamed name) = Just name
isNamedDecl (DeclAnon  _)    = Nothing

isAnonDecl :: DeclId -> Maybe AnonId
isAnonDecl (DeclNamed _)     = Nothing
isAnonDecl (DeclAnon anonId) = Just anonId

getDeclId :: MonadIO m => CXCursor -> m DeclId
getDeclId curr = do
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
    userProvided <- HighLevel.clang_getCursorSpelling curr
    case getUserProvided userProvided of
      Just name ->
        return $ DeclNamed (CName name)
      Nothing ->
        DeclAnon . AnonId . multiLocExpansion
          <$> HighLevel.clang_getCursorLocation curr

instance PrettyForTrace DeclId where
  prettyForTrace (DeclNamed name)   = prettyForTrace name
  prettyForTrace (DeclAnon  anonId) = prettyForTrace anonId

-- | Qualified declaration identity
data QualDeclId = QualDeclId DeclId C.NameKind
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace QualDeclId where
  prettyForTrace (QualDeclId declId cNameKind) =
    let prefix = case cNameKind of
          C.NameKindOrdinary -> ""
          C.NameKindStruct   -> "struct "
          C.NameKindUnion    -> "union "
          C.NameKindEnum     -> "enum "
    in  prefix >< prettyForTrace declId

declQualDeclId :: Id p ~ DeclId => C.Decl p -> QualDeclId
declQualDeclId C.Decl{declInfo = C.DeclInfo{declId}, declKind} =
    QualDeclId declId (C.declKindNameKind declKind)
