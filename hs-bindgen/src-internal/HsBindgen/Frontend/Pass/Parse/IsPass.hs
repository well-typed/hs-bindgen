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
  ) where

import Data.Text qualified as Text
import GHC.TypeLits (Symbol)

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)
import HsBindgen.Frontend.Pass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parse :: Pass
data Parse a deriving anyclass ValidPass

type family ParseAnn (ix :: Symbol) :: Star where
  ParseAnn "TranslationUnit" = UseDefGraph Parse
  ParseAnn "StructField"     = ReparseInfo
  ParseAnn "UnionField"      = ReparseInfo
  ParseAnn "Typedef"         = ReparseInfo
  ParseAnn _                 = NoAnn

instance IsPass Parse where
  type Id     Parse = DeclId
  type Macro  Parse = UnparsedMacro
  type Ann ix Parse = ParseAnn ix

{-------------------------------------------------------------------------------
  Identity

  Not all declarations in a C header have names; to be able to nonetheless refer
  to these declarations we use the source location. We replace these by proper
  names in the 'RenameAnon' pass.
-------------------------------------------------------------------------------}

-- | Identity of a declaration
data DeclId =
    DeclNamed Text
  | DeclAnon AnonId
  deriving stock (Show, Eq, Ord)

-- | Identity of an anonymous declaration
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord)

isNamedDecl :: DeclId -> Maybe Text
isNamedDecl (DeclNamed name) = Just name
isNamedDecl (DeclAnon  _)    = Nothing

isAnonDecl :: DeclId -> Maybe AnonId
isAnonDecl (DeclNamed _)     = Nothing
isAnonDecl (DeclAnon anonId) = Just anonId

getDeclId :: MonadIO m => CXCursor -> m DeclId
getDeclId curr = do
    name <- clang_getCursorSpelling curr
    if not (Text.null name) then
      return $ DeclNamed name
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
