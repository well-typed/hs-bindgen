module HsBindgen.Frontend.Pass.TypecheckMacros.Error (
    MacroTypecheckError(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import C.Expr.Typecheck qualified as CExpr

import HsBindgen.Imports
import HsBindgen.Util.Tracer

data MacroTypecheckError =
    -- | We could not type-check the macro expression
    MacroTypecheckErrorCExpr CExpr.MacroTcError
    -- | Macro type expression references a local argument
    --
    -- Type-parameterised macros (e.g. @#define MYPTR(T) T *@) are not
    -- supported.
  | MacroTypecheckErrorTypeArgInType Text
    -- | Macro type is @void@, which is not a valid standalone type
  | MacroTypecheckErrorVoidType
  deriving stock (Show)

instance PrettyForTrace MacroTypecheckError where
  prettyForTrace = \case
      MacroTypecheckErrorCExpr x -> PP.hsep [
          "Failed to typecheck macro:"
        , PP.text $ CExpr.pprMacroTcError x
        ]
      MacroTypecheckErrorTypeArgInType name -> PP.hsep [
          "Type-parameterised macros are not supported; local argument"
        , PP.text name
        , "used as type"
        ]
      MacroTypecheckErrorVoidType ->
        "Macro type 'void' is not supported as a standalone type"

instance IsTrace Level MacroTypecheckError where
  getDefaultLogLevel = \case
    MacroTypecheckErrorCExpr{}           -> Info
    MacroTypecheckErrorTypeArgInType{}   -> Info
    MacroTypecheckErrorVoidType{}        -> Info
  getSource          = const HsBindgen
  getTraceId         = const "macro-typecheck"
