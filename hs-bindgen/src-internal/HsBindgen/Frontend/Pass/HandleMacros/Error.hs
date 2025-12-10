module HsBindgen.Frontend.Pass.HandleMacros.Error (
    -- * Parse
    FailedMacro(..)
  , HandleMacrosError(..)
    -- * Reparse
  , HandleMacrosReparseMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import C.Expr.Parse.Infra qualified as CExpr.DSL
import C.Expr.Typecheck.Expr qualified as CExpr.DSL

import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Parse messages
-------------------------------------------------------------------------------}

-- | Macro parse messages; see also 'HandleMacrosReparseMsg'
newtype FailedMacro = FailedMacro {
    unFailedMacro :: AttachedParseMsg HandleMacrosError
  }
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)

data HandleMacrosError =
    -- | We could not parse the macro (macro def sites)
    --
    -- When this happens, we get /two/ parse errors: one for trying to parse the
    -- macro as a type, and one for trying to parse the macro as an expression.
    HandleMacrosErrorParse LanC.Error CExpr.DSL.MacroParseError

  | HandleMacrosErrorEmpty C.DeclName

    -- | We could not type-check the macro
  | HandleMacrosErrorTc CExpr.DSL.MacroTcError
  deriving stock (Show)

instance PrettyForTrace HandleMacrosError where
  prettyForTrace = \case
      HandleMacrosErrorParse errType errExpr -> PP.vcat [
          "Could not parse macro as type:"
        , PP.nest 2 $ prettyForTrace errType
        , "nor as expression:"
        , PP.nest 2 $ prettyParseError errExpr
        ]
      HandleMacrosErrorEmpty name -> PP.hsep [
          "Ignoring empty macro:"
        , prettyForTrace name
        ]
      HandleMacrosErrorTc x -> PP.hsep [
          "Failed to typecheck macro:"
        , PP.textToCtxDoc $ CExpr.DSL.pprTcMacroError x
        ]

prettyParseError :: CExpr.DSL.MacroParseError -> PP.CtxDoc
prettyParseError err =
    PP.renderedLines (\_maxWidth -> lines reparseError)
  where
    CExpr.DSL.MacroParseError{reparseError} = err

instance IsTrace Level HandleMacrosError where
  getDefaultLogLevel = \case
    HandleMacrosErrorParse{} -> Info
    HandleMacrosErrorEmpty{} -> Info
    HandleMacrosErrorTc{}    -> Info
  getSource          = const HsBindgen
  getTraceId         = const "handle-macros"

{-------------------------------------------------------------------------------
  Reparse messages
-------------------------------------------------------------------------------}

-- | Macro reparse messages; see also 'FailedMacro'
data HandleMacrosReparseMsg =
    -- | We could not reparse a fragment of C (to recover macro use sites)
    HandleMacrosErrorReparse LanC.Error
  deriving stock (Show)

instance PrettyForTrace HandleMacrosReparseMsg where
  prettyForTrace = \case
      HandleMacrosErrorReparse x -> PP.hsep [
          "Failed to reparse: "
        , prettyForTrace x
        ]

instance IsTrace Level HandleMacrosReparseMsg where
  getDefaultLogLevel = const Info
  getSource          = const HsBindgen
  getTraceId         = const "handle-macros-reparse"
