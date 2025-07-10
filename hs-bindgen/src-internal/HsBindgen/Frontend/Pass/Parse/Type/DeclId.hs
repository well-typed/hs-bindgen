module HsBindgen.Frontend.Pass.Parse.Type.DeclId (
    DeclId (..)
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
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Identity

  Not all declarations in a C header have names; to be able to nonetheless refer
  to these declarations we use the source location. We replace these by proper
  names in the 'RenameAnon' pass.
-------------------------------------------------------------------------------}

-- | Identity of a declaration
data DeclId =
    -- | Named declaration
    DeclNamed CName

    -- | Anonymous declaration
    --
    -- This can only happen for tagged types: structs, unions and enums
  | DeclAnon AnonId

    -- | Built-in declaration
    --
    -- Note: since built-in declarations don't have a definition, we cannot
    -- in general generate bindings for them. If there are /specific/ built-in
    -- declarations we should support, we need to special-case them.
  | DeclBuiltin CName

  deriving stock (Show, Eq, Ord)

instance IsString DeclId where
  fromString = DeclNamed . fromString

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
    spelling <- HighLevel.clang_getCursorSpelling curr
    case spelling of
      UserProvided name ->
        return $ DeclNamed (CName name)
      ClangGenerated _ ->
        DeclAnon . AnonId . multiLocExpansion
          <$> HighLevel.clang_getCursorLocation curr
      ClangBuiltin name ->
        return $ DeclBuiltin (CName name)

instance PrettyForTrace DeclId where
  prettyForTrace (DeclNamed   name)   = prettyForTrace name
  prettyForTrace (DeclAnon    anonId) = prettyForTrace anonId
  prettyForTrace (DeclBuiltin name)   = prettyForTrace name

instance PrettyForTrace (C.Located DeclId) where
  prettyForTrace (C.Located loc declId) =
      case declId of
        DeclNamed name -> PP.hcat [
            prettyForTrace name
          , " at "
          , PP.showToCtxDoc loc
          ]
        DeclAnon anonId ->
          -- No need to repeat the source location in this case
          prettyForTrace anonId
        DeclBuiltin builtin ->
          -- Builtins don't /have/ a location
          prettyForTrace builtin

-- | Qualified declaration identity
data QualDeclId = QualDeclId {
      qualDeclId   :: DeclId
    , qualDeclKind :: C.NameKind
    }
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
