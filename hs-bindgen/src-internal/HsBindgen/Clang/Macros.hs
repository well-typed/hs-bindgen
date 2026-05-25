module HsBindgen.Clang.Macros (
    MacroDefinition (..)
  ) where

import Data.Text (Text)

import Clang.HighLevel.Types (MultiLoc, Range, Token, TokenSpelling)

data MacroDefinition = MacroDefinition {
    name     :: Text
  , locRange :: Range MultiLoc
  , tokens   :: [Token TokenSpelling]
  }
