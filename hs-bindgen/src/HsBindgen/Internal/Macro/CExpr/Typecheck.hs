module HsBindgen.Internal.Macro.CExpr.Typecheck (
    typecheckMacros
  ) where

import Data.Map qualified as Map
import Data.Text qualified as Text

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr
import C.Expr.Typecheck.Type qualified as CExpr

import HsBindgen.Imports
import HsBindgen.Internal.Macro.CExpr.Type
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro

typeOfAnn :: C.DeclId -> Maybe CExpr.QuantTy
typeOfAnn declId = case declId.name.kind of
    C.NameKindOrdinary -> Just CExpr.simpleType
    _otherwise         -> Nothing

typecheckMacros ::
     [Macro.Resolved CExpr]
  -> Map Text (Macro.TypecheckResult CExpr)
typecheckMacros bodies =
    Map.mapKeysMonotonic (.getIdentifier) $ fmap convertResult $
      CExpr.tcMacros typeOfAnn (map (.macro) bodies)
  where
    convertResult ::
         CExpr.MacroTcResult C.DeclId
      -> Macro.TypecheckResult CExpr
    convertResult = \case
      CExpr.MacroTcTypeExpr x ->
        Macro.TypecheckType  x
      CExpr.MacroTcValueExpr x ->
        Macro.TypecheckValue x
      CExpr.MacroTcError err ->
        Macro.TypecheckError $
            MacroTypecheckError (Text.unpack (CExpr.pprMacroTcError err))
