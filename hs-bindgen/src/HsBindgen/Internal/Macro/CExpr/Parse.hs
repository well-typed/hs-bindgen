module HsBindgen.Internal.Macro.CExpr.Parse (
    parseMacro
  ) where

import Data.Vec.Lazy qualified as Vec

import C.Expr.Parse qualified as CExpr
import C.Expr.Syntax qualified as CExpr

import Clang.CStandard
import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

import HsBindgen.Internal.Macro.CExpr.Type
import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro

-- | Parse the body of a macro definition as a C expression
--
-- The definition has already been split by
-- 'HsBindgen.Macro.Syntax.splitMacro'; we assemble the 'CExpr.Macro' from the
-- split and the parsed body.
parseMacro ::
     ClangCStandard
  -> Runtime.Macro.Raw (Token SourcePath TokenSpelling)
  -> Either MacroParseError (Macro.Unresolved CExpr)
parseMacro cStd macro =
    case macro.params of
      Runtime.Macro.NoParams                               -> parseBody []
      Runtime.Macro.Params params Runtime.Macro.NotVariadic     -> parseBody params
      Runtime.Macro.Params _      Runtime.Macro.Ellipsis        -> Left unsupportedVariadic
      Runtime.Macro.Params _      Runtime.Macro.NamedEllipsis{} -> Left unsupportedVariadic
  where
    sourcePath :: FilePath
    sourcePath =
          getSourcePath $ singleLocPath start
            where
              start :: SingleLoc SourcePath
              start = rangeStart $ multiLocExpansion <$> tokenExtent macro.name

    parseBody ::
         [Token SourcePath TokenSpelling]
      -> Either MacroParseError (Macro.Unresolved CExpr)
    parseBody params =
        Vec.reifyList (map identifier params) $ \macroParams ->
          case CExpr.runParser sourcePath (CExpr.parseMacroBody cStd macroParams) macro.body of
            Right body -> Right . Macro.Unresolved $ CExpr.Macro{
                macroLoc    = macro.name.tokenExtent.rangeStart
              , macroName   = identifier macro.name
              , macroParams = macroParams
              , macroExpr   = body
              }
            Left err -> Left $ MacroParseError err.parseError

    identifier :: Token SourcePath TokenSpelling -> CExpr.Identifier
    identifier = CExpr.Identifier . getTokenSpelling . tokenSpelling

    -- 'CExpr.Macro' does not support a variadic parameter list; without this
    -- check @__VA_ARGS__@, or the GNU name standing for it, would silently
    -- parse as a free variable.
    unsupportedVariadic :: MacroParseError
    unsupportedVariadic = MacroParseError {
        macroParseError = "variadic macros are not supported"
      }
