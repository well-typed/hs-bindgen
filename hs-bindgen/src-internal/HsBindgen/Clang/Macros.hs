module HsBindgen.Clang.Macros (
    MacroDefinition (..)
  , MacroInvocation (..)
  ) where

import Data.Text (Text)

import Clang.HighLevel.Types (MultiLoc, Range, RealPath, SourcePath, Token,
                              TokenSpelling)

data MacroDefinition = MacroDefinition {
    name     :: Text
  , locRange :: Range (MultiLoc RealPath)
  , tokens   :: [Token SourcePath TokenSpelling]
  }

data MacroInvocation = MacroInvocation {
    name     :: Text
  , locRange :: Range (MultiLoc RealPath)
  , tokens   :: [Token SourcePath TokenSpelling]
  }
  deriving stock (Show, Eq, Ord)
