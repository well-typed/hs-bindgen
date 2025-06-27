module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Identity
  , DeclId(..)
  , isNamedDecl
  , isAnonDecl
  , getDeclId
  , QualDeclId(..)
  , declQualDeclId
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
    -- * Tracing
  , ParseMsg(..)
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException)
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint (hcat, showToCtxDoc, (><))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parse :: Pass
data Parse a deriving anyclass ValidPass

type family AnnParse (ix :: Symbol) :: Star where
  AnnParse "TranslationUnit" = NonSelectedDecls
  AnnParse "StructField"     = ReparseInfo
  AnnParse "UnionField"      = ReparseInfo
  AnnParse "Typedef"         = ReparseInfo
  AnnParse "Function"        = ReparseInfo
  AnnParse _                 = NoAnn

instance IsPass Parse where
  type Id         Parse = DeclId
  type FieldName  Parse = CName
  type TypedefRef Parse = CName
  type MacroBody  Parse = UnparsedMacro
  type Ann ix     Parse = AnnParse ix

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

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

newtype UnparsedMacro = UnparsedMacro {
      unparsedTokens :: [Token TokenSpelling]
    }
  deriving stock (Show, Eq)

data ReparseInfo =
    -- | We need to reparse this declaration (to deal with macros)
    --
    -- NOTE: We do not use this for macros /themselves/ (see 'UnparsedMacro').
    ReparseNeeded [Token TokenSpelling]

    -- | This declaration does not use macros, so no need to reparse
  | ReparseNotNeeded
  deriving stock (Show, Eq)

getUnparsedMacro ::
     MonadIO m
  => CXTranslationUnit -> CXCursor -> m UnparsedMacro
getUnparsedMacro unit curr = do
    range  <- HighLevel.clang_getCursorExtent curr
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ UnparsedMacro tokens

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ParseMsg =
    -- | We skipped over a declaration
    Skipped Predicate.SkipReason

    -- | Struct with implicit fields
  | UnsupportedImplicitFields {
        -- | Name of the (outer) struct (which has implicit fields)
        unsupportedImplicitFieldsIn :: DeclId

        -- | The names of the implicit fields
      , unsupportedImplicitFields :: [DeclId]
      }

    -- | Unsupported type
    --
    -- Since types don't necessarily have associated source locations, we
    -- instead record information about the enclosing declaration.
  | UnsupportedType {
        unsupportedTypeContext   :: C.DeclInfo Parse
      , unsupportedTypeException :: ParseTypeException
      }
  deriving stock (Show, Eq)

instance PrettyForTrace ParseMsg where
  prettyForTrace = \case
      Skipped reason ->
          prettyForTrace reason
      UnsupportedImplicitFields {..} -> hcat [
          "Unsupported implicit fields "
        , showToCtxDoc unsupportedImplicitFields
        , " in "
        , showToCtxDoc unsupportedImplicitFieldsIn
        ]
      UnsupportedType {..} -> hcat [
          "Encountered unsupported type while parsing "
        , prettyForTrace (C.declId unsupportedTypeContext)
        , " at "
        , showToCtxDoc (C.declLoc unsupportedTypeContext)
        , ": "
        , prettyForTrace unsupportedTypeException
        , ". "
        , "No bindings generated for "
        , prettyForTrace (C.declId unsupportedTypeContext)
        , "."
        ]

instance HasDefaultLogLevel ParseMsg where
  getDefaultLogLevel = \case
      Skipped reason              -> getDefaultLogLevel reason
      UnsupportedImplicitFields{} -> Error
      UnsupportedType _ctxt err   -> getDefaultLogLevel err

instance HasSource ParseMsg where
    getSource = const HsBindgen
