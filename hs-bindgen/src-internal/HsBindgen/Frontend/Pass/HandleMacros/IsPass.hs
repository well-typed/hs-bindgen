module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  , HandleMacrosMsg(..)
  ) where

import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Macro.Reparse.Infra
import HsBindgen.Frontend.Macro.Tc
import HsBindgen.Frontend.Macro.Tc qualified as Macro
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta)
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type HandleMacros :: Pass
data HandleMacros a deriving anyclass ValidPass

-- We do not need the @ReparseInfo@ anymore, so we drop it from the annotations.
type family AnnHandleMacros (ix :: Symbol) :: Star where
  AnnHandleMacros "TranslationUnit" = DeclMeta
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
    -- | We could not parse the macro
    HandleMacrosErrorReparse ReparseError

    -- | We could not type-check the macro
  | HandleMacrosErrorTc TcMacroError

    -- | Unsupported macro: empty body
  | HandleMacrosErrorEmpty

    -- | Unsupported macro: defines C compiler attribute
  | HandleMacrosErrorAttribute

    -- | Macro that defines an unsupported type
  | HandleMacrosErrorUnsupportedType String
  deriving stock (Show, Eq)

instance PrettyForTrace HandleMacrosMsg where
  prettyForTrace = \case
      HandleMacrosErrorReparse x ->
        prettyForTrace x
      HandleMacrosErrorTc x ->
        textToCtxDoc $ Macro.pprTcMacroError x
      HandleMacrosErrorEmpty ->
        "Unsupported empty macro"
      HandleMacrosErrorAttribute ->
        "Unsupported attribute macro"
      HandleMacrosErrorUnsupportedType err ->
        "Unsupported type: " >< string err

-- | Default log level
--
-- Reparse and typechecking errors may indicate that something went wrong, or
-- they may be caused by macro syntax that we do not yet support.  They are
-- 'Info' by default because there are many unsupported macros in standard
-- library implementations.  Users may optionally make them 'Warning' instead.
--
-- Other errors are 'Info' because they are /always/ unsupported.
instance IsTrace Level HandleMacrosMsg where
  getDefaultLogLevel = const Info
  getSource          = const HsBindgen
  getTraceId         = const "handle-macros"
