-- | Macro definitions and invocations, as reported by @libclang@.
module HsBindgen.Macro.Syntax (
    MacroDefinition (..)
  , MacroInvocation (..)
  ) where

import Data.Text (Text)

import Clang.HighLevel.Types (MultiLoc, Range, Token, TokenSpelling)

import HsBindgen.Runtime.Macro qualified as RawMacro

import HsBindgen.Macro.Error (MacroParseError)

data MacroDefinition = MacroDefinition {
    name     :: Text
  , locRange :: Range MultiLoc
    -- | The definition, split into name, parameters and body
    --
    -- The split is language-independent and happens once, while parsing; see
    -- 'HsBindgen.Macro.Parse.splitMacro'.
  , macro    :: Either MacroParseError (RawMacro.Raw (Token TokenSpelling))
  }

data MacroInvocation = MacroInvocation {
    name     :: Text
  , locRange :: Range MultiLoc
  , tokens   :: [Token TokenSpelling]
  }
  deriving stock (Show, Eq, Ord)
