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
  , ParseTrace(..)
  ) where

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException)
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parse :: Pass
data Parse a deriving anyclass ValidPass

type family ParseAnn (ix :: Symbol) :: Star where
  ParseAnn "TranslationUnit" = NonSelectedDecls
  ParseAnn "StructField"     = ReparseInfo
  ParseAnn "UnionField"      = ReparseInfo
  ParseAnn "Typedef"         = ReparseInfo
  ParseAnn "Function"        = ReparseInfo
  ParseAnn _                 = NoAnn

instance IsPass Parse where
  type Id         Parse = DeclId
  type FieldName  Parse = CName
  type TypedefRef Parse = CName
  type MacroBody  Parse = UnparsedMacro
  type Ann ix     Parse = ParseAnn ix

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
    -- for an anonymous declaration if there is a @typedef@ for the type.
    --
    -- In older versions of LLVM, one could check @clang_getCursorSpelling@ for
    -- an empty result, but this has changed in later versions of LLVM.  It is
    -- recommended to not parse cursor or type spellings for this purpose.
    --
    -- We instead check for non-tagged declarations.  See 'isNotTagged' below.
    notTagged <- isNotTagged curr
    if notTagged then
      DeclAnon . AnonId . multiLocExpansion <$>
        HighLevel.clang_getCursorLocation curr
    else
      DeclNamed . CName <$> clang_getCursorSpelling curr

-- | Check if the cursor is for a non-tagged declaration
--
-- This function returns 'True' if the cursor is for a @struct@, @union@, or
-- @enum@ declaration and the underlying tokens starts with that keyword and a
-- left bracket.
isNotTagged :: MonadIO m => CXCursor -> m Bool
isNotTagged curr = do
    kind <- clang_getCursorKind curr
    let mKeyword = case fromSimpleEnum kind of
          Right CXCursor_StructDecl -> Just "struct"
          Right CXCursor_UnionDecl  -> Just "union"
          Right CXCursor_EnumDecl   -> Just "enum"
          _otherwise                -> Nothing
    case mKeyword of
      Nothing      -> return False
      Just keyword -> do
        unit <-
          maybe (panicIO "isNotTagged: unable to get translation unit") return
            =<< clang_Cursor_getTranslationUnit curr
        tokens <- HighLevel.clang_tokenize unit . fmap multiLocExpansion
          =<< HighLevel.clang_getCursorExtent curr
        return $ case tokens of
          token0 : token1 : _rest -> and [
              fromSimpleEnum (tokenKind token0) == Right CXToken_Keyword
            , tokenSpelling token0 == TokenSpelling keyword
            , fromSimpleEnum (tokenKind token1) == Right CXToken_Punctuation
            , tokenSpelling token1 == TokenSpelling "{"
            ]
          _otherwise -> False

instance PrettyTrace DeclId where
  prettyTrace (DeclNamed name)   = prettyTrace name
  prettyTrace (DeclAnon  anonId) = prettyTrace anonId

-- | Qualified declaration identity
data QualDeclId = QualDeclId DeclId C.NameKind
  deriving stock (Show, Eq, Ord)

instance PrettyTrace QualDeclId where
  prettyTrace (QualDeclId declId cNameKind) =
    let prefix = case cNameKind of
          C.NameKindOrdinary -> ""
          C.NameKindStruct   -> "struct "
          C.NameKindUnion    -> "union "
          C.NameKindEnum     -> "enum "
    in  prefix <> prettyTrace declId

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

data ParseTrace =
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

instance PrettyTrace ParseTrace where
  prettyTrace = \case
      Skipped reason ->
          prettyTrace reason
      UnsupportedImplicitFields {..} -> concat [
          "Unsupported implicit fields "
        , show unsupportedImplicitFields
        , " in "
        , show unsupportedImplicitFieldsIn
        ]
      UnsupportedType {..} -> concat [
          "Encountered unsupported type while parsing "
        , prettyTrace (C.declId unsupportedTypeContext)
        , " at "
        , show (C.declLoc unsupportedTypeContext)
        , ": "
        , prettyTrace unsupportedTypeException
        , ". "
        , "No bindings generated for "
        , prettyTrace (C.declId unsupportedTypeContext)
        , "."
        ]

instance HasDefaultLogLevel ParseTrace where
  getDefaultLogLevel = \case
      Skipped reason              -> getDefaultLogLevel reason
      UnsupportedImplicitFields{} -> Error
      UnsupportedType _ctxt err   -> getDefaultLogLevel err

instance HasSource ParseTrace where
    getSource = const HsBindgen
