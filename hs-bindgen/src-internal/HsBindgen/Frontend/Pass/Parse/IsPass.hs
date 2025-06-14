module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Identity
  , DeclId(..)
  , AnonId(..)
  , isNamedDecl
  , isAnonDecl
  , getDeclId
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
    -- * Tracing
  , ParseTrace(..)
  ) where

import Data.Text qualified as Text

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

-- | Identity of an anonymous declaration
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord)

isNamedDecl :: DeclId -> Maybe CName
isNamedDecl (DeclNamed name) = Just name
isNamedDecl (DeclAnon  _)    = Nothing

isAnonDecl :: DeclId -> Maybe AnonId
isAnonDecl (DeclNamed _)     = Nothing
isAnonDecl (DeclAnon anonId) = Just anonId

getDeclId :: MonadIO m => CXCursor -> m DeclId
getDeclId curr = do
    name   <- clang_getCursorSpelling  curr
    isAnon <- clang_Cursor_isAnonymous curr
    if isAnon || Text.null name then
      DeclAnon . AnonId . multiLocExpansion <$>
        HighLevel.clang_getCursorLocation curr
    else
      return $ DeclNamed (CName name)

instance PrettyTrace DeclId where
  prettyTrace (DeclNamed name)   = prettyTrace name
  prettyTrace (DeclAnon  anonId) = prettyTrace anonId

instance PrettyTrace AnonId where
  prettyTrace (AnonId loc) = "<" ++ show loc ++ ">"

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
