module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  , HandleMacrosMsg(..)
  ) where

import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Macro.Reparse.Infra qualified as Macro
import HsBindgen.Frontend.Macro.Tc
import HsBindgen.Frontend.Macro.Tc qualified as Macro
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta)
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type HandleMacros :: Pass
data HandleMacros a deriving anyclass ValidPass

-- We do not need the @ReparseInfo@ anymore, so we drop it from the annotations.
type family AnnHandleMacros (ix :: Symbol) :: Star where
  AnnHandleMacros "TranslationUnit" = DeclMeta HandleMacros
  AnnHandleMacros _                 = NoAnn

instance IsPass HandleMacros where
  type Id           HandleMacros = C.PrelimDeclId
  type FieldName    HandleMacros = C.Name
  type ArgumentName HandleMacros = Maybe C.Name
  type TypedefRef   HandleMacros = C.Name
  type MacroBody    HandleMacros = CheckedMacro HandleMacros
  type ExtBinding   HandleMacros = Void
  type Ann ix       HandleMacros = AnnHandleMacros ix
  type Msg          HandleMacros = HandleMacrosMsg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- TODO: We might want source location information here
data HandleMacrosMsg =
    -- | We could not reparse a fragment of C (to recover macro use sites)
    HandleMacrosErrorReparse LanC.Error

    -- | We could not parse the macro (macro def sites)
    --
    -- When this happens, we get /two/ parse errors: one for trying to parse the
    -- macro as a type, and one for trying to parse the macro as an expression.
    --
    -- TODO: Avoid the \"reparse\" terminology for macros.
  | HandleMacrosErrorParse LanC.Error Macro.ReparseError

    -- | We could not type-check the macro
  | HandleMacrosErrorTc TcMacroError

    -- | Macro that defines an unsupported type
    -- TODO: Do we still use this?
  | HandleMacrosErrorUnsupportedType String
  deriving stock (Show)

instance PrettyForTrace HandleMacrosMsg where
  prettyForTrace = \case
      HandleMacrosErrorReparse x -> PP.hsep [
          "Failed to reparse: "
        , prettyForTrace x
        ]
      HandleMacrosErrorParse errType errExpr -> PP.hsep [
          "Could not parse macro as type:"
        , PP.nest 2 $ prettyForTrace errType
        , "nor as expression:"
        , PP.nest 2 $ prettyForTrace errExpr
        ]
      HandleMacrosErrorTc x ->
          PP.textToCtxDoc $ Macro.pprTcMacroError x
      HandleMacrosErrorUnsupportedType err -> PP.hsep [
          "Unsupported type: "
        , PP.string err
        ]

-- | Default log level
instance IsTrace Level HandleMacrosMsg where
  getDefaultLogLevel = const Info
  getSource          = const HsBindgen
  getTraceId         = const "handle-macros"
