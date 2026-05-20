module HsBindgen.Frontend.Pass.TypecheckMacros.Error (
    TypecheckMacrosError(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import C.Expr.Typecheck qualified as CExpr

import HsBindgen.Frontend.Naming
import HsBindgen.Util.Tracer

data TypecheckMacrosError =
    -- | We could not type-check the macro expression
    TypecheckMacrosErrorCExpr CExpr.MacroTcError
    -- | Macro type references a tagged type we could not resolve
    --
    -- This happens when a macro references a struct, union, or enum that
    -- @hs-bindgen@ did not parse (e.g., defined in an unprocessed header, or
    -- whose fields failed to parse).
  | TypecheckMacrosErrorUnresolvedTaggedType CDeclName
  deriving stock (Show)

instance PrettyForTrace TypecheckMacrosError where
  prettyForTrace = \case
      TypecheckMacrosErrorCExpr x -> PP.hsep [
          "Failed to typecheck macro:"
        , PP.text $ CExpr.pprMacroTcError x
        ]
      TypecheckMacrosErrorUnresolvedTaggedType name -> PP.hsep [
          "Macro type references unknown tagged type:"
        , prettyForTrace name
        ]

instance IsTrace Level TypecheckMacrosError where
  getDefaultLogLevel = \case
    TypecheckMacrosErrorCExpr{}                -> Info
    TypecheckMacrosErrorUnresolvedTaggedType{} -> Warning
  getSource          = const HsBindgen
  getTraceId         = const "macro-typecheck"
