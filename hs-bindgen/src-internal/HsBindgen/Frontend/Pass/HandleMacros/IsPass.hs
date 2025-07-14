module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  , Msg(..)
  ) where

import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Tc.Macro
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Type.PrelimDeclId
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
  -- TODO: We might want source location information here
  data Msg        HandleMacros =
      -- | We could not parse the macro
      MacroErrorReparse ReparseError

      -- | We could not type-check the macro
    | MacroErrorTc TcMacroError

      -- | Unsupported macro: empty body
    | MacroErrorEmpty

      -- | Unsupported macro: defines C compiler attribute
    | MacroErrorAttribute

      -- | Macro that defines an unsupported type
    | MacroErrorUnsupportedType String
    deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

instance PrettyForTrace (Msg HandleMacros) where
  prettyForTrace = \case
      MacroErrorReparse x ->
        prettyForTrace x
      MacroErrorTc x ->
        textToCtxDoc $ Macro.pprTcMacroError x
      MacroErrorEmpty ->
        "Unsupported empty macro"
      MacroErrorAttribute ->
        "Unsupported attribute macro"
      MacroErrorUnsupportedType err ->
        "Unsupported type: " >< string err

-- | Default log level
--
-- We use 'Info' for macros that are /always/ unsupported, and 'Warning' for
-- macros that we might perhaps except to be supported but something went wrong.
instance HasDefaultLogLevel (Msg HandleMacros) where
  getDefaultLogLevel = \case
    MacroErrorReparse{}         -> Warning
    MacroErrorTc{}              -> Warning
    MacroErrorEmpty{}           -> Info
    MacroErrorAttribute{}       -> Info
    MacroErrorUnsupportedType{} -> Info
