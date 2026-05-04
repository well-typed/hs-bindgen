module HsBindgen.Frontend.Pass.TypecheckMacros.Error (
    MacroTypecheckError(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import C.Expr.Typecheck qualified as CExpr

import HsBindgen.Frontend.Naming
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
    -- | Macro type references a tagged type we could not resolve
    --
    -- This happens when a macro references a struct, union, or enum that
    -- @hs-bindgen@ did not parse (e.g., defined in an unprocessed header, or
    -- whose fields failed to parse).
  | MacroTypecheckErrorUnresolvedTaggedType CDeclName
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
      MacroTypecheckErrorUnresolvedTaggedType name -> PP.hsep [
          "Macro type references unknown tagged type:"
        , prettyForTrace name
        ]

instance IsTrace Level MacroTypecheckError where
  getDefaultLogLevel = \case
    MacroTypecheckErrorCExpr{}                -> Info
    MacroTypecheckErrorTypeArgInType{}        -> Info
    MacroTypecheckErrorVoidType{}             -> Info
    MacroTypecheckErrorUnresolvedTaggedType{} -> Warning
  getSource          = const HsBindgen
  getTraceId         = const "macro-typecheck"
