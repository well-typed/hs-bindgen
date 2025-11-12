module HsBindgen.Frontend.Pass.HandleMacros.Error (
    -- * Parse
    HandleMacrosParseMsg(..)
  , HandleMacrosError(..)
    -- * Reparse
  , HandleMacrosReparseMsg(..)
  ) where

import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import C.Expr.Parse.Infra qualified as CExpr.DSL
import C.Expr.Typecheck.Expr qualified as CExpr.DSL

import Clang.HighLevel.Types

import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Parse messages
-------------------------------------------------------------------------------}

-- | Macro parse messages; see also 'HandleMacrosReparseMsg'
newtype HandleMacrosParseMsg = HandleMacrosParseMsg {
    unHandleMacrosParseMsg :: AttachedParseMsg HandleMacrosError
  }
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace)

data HandleMacrosError =
    -- | We could not parse the macro (macro def sites)
    --
    -- When this happens, we get /two/ parse errors: one for trying to parse the
    -- macro as a type, and one for trying to parse the macro as an expression.
    HandleMacrosErrorParse LanC.Error CExpr.DSL.MacroParseError

  | HandleMacrosErrorEmpty C.Name

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
      HandleMacrosErrorEmpty name ->
          "Ignoring empty macro:" <+> prettyForTrace name
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
  getDefaultLogLevel = \case
    HandleMacrosErrorParse{} -> Info
    HandleMacrosErrorEmpty{} -> Info
    HandleMacrosErrorTc{}    -> Info
  getSource          = const HsBindgen
  getTraceId         = const "handle-macros"

{-------------------------------------------------------------------------------
  Reparse messages
-------------------------------------------------------------------------------}

-- | Macro reparse messages; see also 'HandleMacrosParseMsg'
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
