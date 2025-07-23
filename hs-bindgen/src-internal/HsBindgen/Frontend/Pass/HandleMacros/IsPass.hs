module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  , HandleMacrosMsg(..)
  ) where

import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Tc.Macro
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
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
  type Id         HandleMacros = PrelimDeclId
  type FieldName  HandleMacros = C.Name
  type TypedefRef HandleMacros = C.Name
  type MacroBody  HandleMacros = CheckedMacro HandleMacros
  type ExtBinding HandleMacros = Void
  type Ann ix     HandleMacros = AnnHandleMacros ix
  type Msg        HandleMacros = HandleMacrosMsg

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
-- We use 'Info' for macros that are /always/ unsupported, and 'Warning' for
-- macros that we might perhaps except to be supported but something went wrong.
instance HasDefaultLogLevel HandleMacrosMsg where
  getDefaultLogLevel = \case
    HandleMacrosErrorReparse{}         -> Warning
    HandleMacrosErrorTc{}              -> Warning
    HandleMacrosErrorEmpty{}           -> Info
    HandleMacrosErrorAttribute{}       -> Info
    HandleMacrosErrorUnsupportedType{} -> Info

instance HasSource HandleMacrosMsg where
  getSource = const HsBindgen
