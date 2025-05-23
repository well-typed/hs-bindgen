module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Identity
  , DeclId(..)
  , NamedId(..)
  , Namespace(..)
  , AnonId(..)
  , isNamedDecl
  , isAnonDecl
  , getDeclId
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
  ) where

import Data.Text qualified as Text
import GHC.TypeLits (Symbol)

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.Errors
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Util
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parse :: Pass
data Parse a

type family ParseAnn (ix :: Symbol) :: Star where
  ParseAnn "TranslationUnit" = UseDefGraph Parse
  ParseAnn "Field"           = ReparseInfo
  ParseAnn "Typedef"         = ReparseInfo
  ParseAnn _                 = NoAnn

instance IsPass Parse where
  type Id     Parse = DeclId
  type Macro  Parse = UnparsedMacro
  type Ann ix Parse = ParseAnn ix

instance ShowPass Parse

{-------------------------------------------------------------------------------
  Identity

  Not all declarations in a C header have names; to be able to nonetheless refer
  to these declarations we use the source location. We replace these by proper
  names in the 'RenameAnon' pass.
-------------------------------------------------------------------------------}

-- | Identity of a declaration
data DeclId =
    DeclNamed NamedId
  | DeclAnon AnonId
  deriving stock (Show, Eq, Ord)

-- | Identity of a named declaration
data NamedId = NamedId Namespace Text
  deriving stock (Show, Eq, Ord)

data Namespace =
    NamespaceTypedef
  | NamespaceStruct
  deriving stock (Show, Eq, Ord)

-- | Identity of an anonymous declaration
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord)

isNamedDecl :: DeclId -> Maybe NamedId
isNamedDecl (DeclNamed name) = Just name
isNamedDecl (DeclAnon  _)    = Nothing

isAnonDecl :: DeclId -> Maybe AnonId
isAnonDecl (DeclNamed _)     = Nothing
isAnonDecl (DeclAnon anonId) = Just anonId

getDeclId :: MonadIO m => CXCursor -> m DeclId
getDeclId curr = do
    name <- clang_getCursorSpelling curr
    if not (Text.null name) then do
      namespace <- dispatch curr $ \case
        CXCursor_StructDecl  -> return NamespaceStruct
        CXCursor_TypedefDecl -> return NamespaceTypedef
        kind -> panicIO $ "Unknown namespace for " ++ show kind
      return $ DeclNamed (NamedId namespace name)
    else
      DeclAnon . AnonId . multiLocExpansion <$>
        HighLevel.clang_getCursorLocation curr

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

data UnparsedMacro = UnparsedMacro [Token TokenSpelling]
  deriving stock (Show)

data ReparseInfo =
    -- | We need to reparse this declaration (to deal with macros)
    --
    -- NOTE: We do not use this for macros /themselves/ (see 'UnparsedMacro').
    ReparseNeeded [Token TokenSpelling]

    -- | This declaration does not use macros, so no need to reparse
  | ReparseNotNeeded
  deriving stock (Show)

getUnparsedMacro ::
     MonadIO m
  => CXTranslationUnit -> CXCursor -> m UnparsedMacro
getUnparsedMacro unit curr = do
    range  <- HighLevel.clang_getCursorExtent curr
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ UnparsedMacro tokens

