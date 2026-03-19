module HsBindgen.Frontend.Pass.TypecheckMacros.Error (
    MacroTypecheckError(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import C.Expr.Typecheck qualified as CExpr.DSL

import HsBindgen.Imports
import HsBindgen.Util.Tracer

data MacroTypecheckError =
    -- | We could not type-check the macro expression
    MacroTypecheckErrorCExpr CExpr.DSL.MacroTcError
    -- | Macro type is @void@, which is not a valid standalone type
  | MacroTypecheckErrorVoidType
    -- | Named type in macro could not be resolved
    --
    -- This is considered a 'Bug', because if we reference an non-existing
    -- variable or tagged specifier (such as @struct Foo@), the typechecker
    -- should fail instead.
  | MacroTypecheckErrorUnresolvedType Text
  deriving stock (Show)

instance PrettyForTrace MacroTypecheckError where
  prettyForTrace = \case
      MacroTypecheckErrorCExpr x -> PP.hsep [
          "Failed to typecheck macro:"
        , PP.text $ CExpr.DSL.pprMacroTcError x
        ]
      MacroTypecheckErrorVoidType ->
        "Macro type 'void' is not supported as a standalone type"
      MacroTypecheckErrorUnresolvedType name -> PP.hsep [
          "Unresolved type name in macro:"
        , PP.text name
        ]

instance IsTrace Level MacroTypecheckError where
  getDefaultLogLevel = \case
    MacroTypecheckErrorCExpr{}           -> Info
    MacroTypecheckErrorVoidType{}        -> Info
    MacroTypecheckErrorUnresolvedType{}  -> Bug
  getSource          = const HsBindgen
  getTraceId         = const "macro-typecheck"
