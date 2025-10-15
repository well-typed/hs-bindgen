module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  , HandleMacrosMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import C.Expr.Parse.Infra qualified as CExpr.DSL
import C.Expr.Typecheck.Expr qualified as CExpr.DSL

import Clang.HighLevel.Types

import HsBindgen.Frontend.AST.Coerce (CoercePass (..))
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (ConstructTranslationUnit,
                                                                DeclMeta)
import HsBindgen.Frontend.Pass.Parse.IsPass (OrigTypedefRef (..))
import HsBindgen.Imports
import HsBindgen.Util.Tracer

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
  type TypedefRef   HandleMacros = OrigTypedefRef HandleMacros
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
  | HandleMacrosErrorParse LanC.Error CExpr.DSL.MacroParseError

    -- | We could not type-check the macro
  | HandleMacrosErrorTc CExpr.DSL.MacroTcError

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
        , PP.nest 2 $ prettyParseError errExpr
        ]
      HandleMacrosErrorTc x ->
          PP.textToCtxDoc $ CExpr.DSL.pprTcMacroError x
      HandleMacrosErrorUnsupportedType err -> PP.hsep [
          "Unsupported type: "
        , PP.string err
        ]

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

-- | Default log level
instance IsTrace Level HandleMacrosMsg where
  getDefaultLogLevel = const Info
  getSource          = const HsBindgen
  getTraceId         = const "handle-macros"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePass TypedefRefWrapper ConstructTranslationUnit HandleMacros where
  coercePass (TypedefRefWrapper ref) = TypedefRefWrapper (coercePass ref)
