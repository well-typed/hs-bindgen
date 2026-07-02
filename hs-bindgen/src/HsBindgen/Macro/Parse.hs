module HsBindgen.Macro.Parse (
    parseMacro
  , parsedMacroDeps
  ) where

import C.Expr.Parse qualified as CExpr
import C.Expr.Syntax qualified as CExpr

import Clang.CStandard
import Clang.HighLevel.Types

import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.CExpr (CExpr)
import HsBindgen.Macro.CExpr qualified as Macro
import HsBindgen.Macro.Error
import HsBindgen.Macro.Type qualified as Macro

parseMacro ::
     ClangCStandard
  -> [Token TokenSpelling]
  -> Either MacroParseError (Macro.Unresolved CExpr)
parseMacro cStd tokens =
    case CExpr.runParser (CExpr.parseMacro cStd) tokens of
      Right macro -> Right $ Macro.Unresolved $ Macro.ParsedCExpr macro
      Left  err   -> Left  $ MacroParseError err.parseError

parsedMacroDeps ::
     Macro.Resolved CExpr
  -> [(C.ValOrRef, C.DeclId)]
parsedMacroDeps resolvedMacro =
    case resolvedMacro.unwrap.unwrap of
      CExpr.Macro _ _ _ expr -> goExpr C.ByValue expr
  where
    goExpr ::
         C.ValOrRef
      -> CExpr.Expr ctx (CExpr.Ps C.DeclId)
      -> [(C.ValOrRef, C.DeclId)]
    goExpr depTy = \case
      CExpr.Term term              -> goTerm depTy term
      -- Pointer: switch the dependency type to 'ByRef'.
      CExpr.TyApp CExpr.Pointer xs -> concatMap (goExpr C.ByRef) xs
      CExpr.TyApp CExpr.Const   xs -> concatMap (goExpr depTy)   xs
      CExpr.VaApp _ _ xs           -> concatMap (goExpr depTy)   xs

    goTerm ::
         C.ValOrRef
      -> CExpr.Term ctx (CExpr.Ps C.DeclId)
      -> [(C.ValOrRef, C.DeclId)]
    goTerm depTy = \case
      CExpr.Literal{}    -> []
      CExpr.LocalParam{} -> []
      CExpr.Var (CExpr.XVarPs declId) _nm args ->
        (depTy, declId) : concatMap (goExpr depTy) args
