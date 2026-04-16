module C.Expr.Parse (
    -- * Parsing macros
    parseMacro
  , parseMacroType
    -- * Parser infrastructure
  , Parser
  , runParser
  , MacroParseError(..)
  ) where

import C.Expr.Parse.Expr (parseMacro, parseMacroType)
import C.Expr.Parse.Infra (MacroParseError (..), Parser, runParser)
