module HsBindgen.Macro.Typecheck (
    typecheckMacros
  , typecheckedMacroTypeDeps
  ) where

import Data.Map qualified as Map
import Data.Text qualified as Text

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr
import C.Expr.Typecheck.Interface.Type qualified as T
import C.Expr.Typecheck.Type qualified as CExpr

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.CExpr (CExpr)
import HsBindgen.Macro.CExpr qualified as Macro
import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

typeOfAnn :: C.DeclId -> Maybe CExpr.QuantTy
typeOfAnn declId = case declId.name.kind of
    C.NameKindOrdinary -> Just CExpr.simpleType
    _otherwise         -> Nothing

typecheckMacros ::
     [Macro.Resolved CExpr]
  -> Map Text (Macro.TypecheckResult CExpr)
typecheckMacros bodies =
    Map.mapKeysMonotonic (.getIdentifier) $ fmap convertResult $
      CExpr.tcMacros typeOfAnn (map (.unwrap.unwrap) bodies)
  where
    convertResult ::
         CExpr.MacroTcResult C.DeclId
      -> Macro.TypecheckResult CExpr
    convertResult = \case
      CExpr.MacroTcTypeExpr x ->
        Macro.TypecheckType  (Macro.TypecheckedTypeCExpr  x)
      CExpr.MacroTcValueExpr x ->
        Macro.TypecheckValue (Macro.TypecheckedValueCExpr x)
      CExpr.MacroTcError err ->
        Macro.TypecheckError $
            MacroTypecheckError (Text.unpack (CExpr.pprMacroTcError err))

typecheckedMacroTypeDeps ::
     Macro.TypecheckedType CExpr C.DeclId
  -> [(C.ValOrRef, C.DeclId)]
typecheckedMacroTypeDeps (Macro.TypecheckedTypeCExpr tcExpr) =
    -- 'T.Expr' is a unary type-application tree, so it carries at most one variable.
    case go C.ByValue tcExpr.macroTypeBody of
      Nothing -> []
      Just x  -> [x]
  where
    go :: C.ValOrRef -> T.Expr C.DeclId -> Maybe (C.ValOrRef, C.DeclId)
    go depTy = \case
      T.TypeLit{}       -> Nothing
      T.App T.Pointer e -> go C.ByRef e
      T.App T.Const   e -> go depTy e
      T.Var v           -> Just (depTy, v)
