module HsBindgen.Frontend.Pass.HandleMacros.Error (
    HandleMacrosFailure(..)
  , HandleMacrosError(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import C.Expr.Parse.Infra qualified as CExpr.DSL
import C.Expr.Typecheck.Expr qualified as CExpr.DSL

import Clang.HighLevel.Types

import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
import HsBindgen.Util.Tracer

data HandleMacrosFailure = HandleMacrosFailure {
    name     :: C.Name
  , location :: SingleLoc
  , error    :: HandleMacrosError
  }
  deriving stock (Show)

data HandleMacrosError =
    -- | We could not parse the macro (macro def sites)
    --
    -- When this happens, we get /two/ parse errors: one for trying to parse the
    -- macro as a type, and one for trying to parse the macro as an expression.
    HandleMacrosErrorParse LanC.Error CExpr.DSL.MacroParseError

    -- | We could not type-check the macro
  | HandleMacrosErrorTc CExpr.DSL.MacroTcError
  deriving stock (Show)

instance PrettyForTrace HandleMacrosError where
  prettyForTrace = \case
      HandleMacrosErrorParse errType errExpr -> PP.hsep [
          "Could not parse macro as type:"
        , PP.nest 2 $ prettyForTrace errType
        , "nor as expression:"
        , PP.nest 2 $ prettyParseError errExpr
        ]
      HandleMacrosErrorTc x ->
          PP.textToCtxDoc $ CExpr.DSL.pprTcMacroError x

prettyParseError :: CExpr.DSL.MacroParseError -> PP.CtxDoc
prettyParseError err = PP.vcat [
      PP.hsep [
          "Reparse error: "
        , fromString reparseError
        ]
    , PP.hsep . map (PP.textToCtxDoc . getTokenSpelling . tokenSpelling) $
        reparseErrorTokens
    ]
  where
    CExpr.DSL.MacroParseError{
        reparseError
      , reparseErrorTokens
      } = err

instance IsTrace Level HandleMacrosError where
  getDefaultLogLevel = const Error
  getSource          = const HsBindgen
  getTraceId         = const "handle-macros"
