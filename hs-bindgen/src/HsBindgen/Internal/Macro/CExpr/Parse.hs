module HsBindgen.Internal.Macro.CExpr.Parse (
    parseMacro
  ) where

import C.Expr.Parse qualified as CExpr

import Clang.CStandard
import Clang.HighLevel.Types

import HsBindgen.Internal.Macro.CExpr.Type (CExpr)
import HsBindgen.Internal.Macro.CExpr.Type qualified as CExpr
import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro

parseMacro ::
     ClangCStandard
  -> [Token TokenSpelling]
  -> Either MacroParseError (Macro.Unresolved CExpr)
parseMacro cStd tokens =
    case CExpr.runParser (CExpr.parseMacro cStd) tokens of
      Right macro -> Right $ Macro.Unresolved $ CExpr.Parsed macro
      Left  err   -> Left  $ MacroParseError err.parseError
