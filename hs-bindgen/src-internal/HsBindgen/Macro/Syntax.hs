-- | Macro definitions and invocations, as reported by @libclang@.
module HsBindgen.Macro.Syntax (
    MacroDefinition (..)
  , MacroInvocation (..)
  ) where

import Data.Text (Text)

import Clang.HighLevel.Types (MultiLoc, Range, Token, TokenSpelling)

data MacroDefinition = MacroDefinition {
    name     :: Text
  , locRange :: Range MultiLoc
  , tokens   :: [Token TokenSpelling]
  }

data MacroInvocation = MacroInvocation {
    name     :: Text
  , locRange :: Range MultiLoc
  , tokens   :: [Token TokenSpelling]
  }
  deriving stock (Show, Eq, Ord)
