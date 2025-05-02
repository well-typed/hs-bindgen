module HsBindgen.C.Raw.Pass.Parse.IsPass (
    Parsed
    -- * Identity
  , DeclId(..)
  , AnonId(..)
  , isNamedDecl
  , isAnonDecl
  , getDeclId
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Data.Text qualified as Text
import HsBindgen.C.Raw.Pass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parsed :: Pass
data Parsed a

instance IsPass Parsed where
  type Id Parsed = DeclId

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

-- | Identity of an anonymous node
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord)

isNamedDecl :: DeclId -> Maybe Text
isNamedDecl (DeclNamed name) = Just name
isNamedDecl (DeclAnon  _)    = Nothing

isAnonDecl :: DeclId -> Maybe AnonId
isAnonDecl (DeclNamed _)      = Nothing
isAnonDecl (DeclAnon  anonId) = Just anonId

getDeclId :: MonadIO m => CXCursor -> m DeclId
getDeclId curr = do
    name <- clang_getCursorSpelling curr
    if not (Text.null name)
      then return $ DeclNamed name
      else DeclAnon . AnonId . multiLocExpansion <$>
             HighLevel.clang_getCursorLocation curr

