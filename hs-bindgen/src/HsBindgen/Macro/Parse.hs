module HsBindgen.Macro.Parse (
    parseMacroBody
  , parsedMacroDeps
  ) where

import Data.Set qualified as Set

import C.Expr.Parse qualified as CExpr
import C.Expr.Syntax qualified as CExpr

import Clang.CStandard
import Clang.HighLevel.Types

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.CExpr
import HsBindgen.Macro.Interface

parseMacroBody ::
     ClangCStandard
  -> [Token TokenSpelling]
  -> Either MacroLangParseError (ParsedMacroBody CExpr)
parseMacroBody cStd tokens =
    case CExpr.runParser (CExpr.parseMacro cStd) tokens of
      Right macro -> Right (ParsedMacroBodyCExpr macro)
      Left  err   -> Left  (MacroLangParseError err.parseError)

parsedMacroDeps ::
     Set C.DeclId
  -> ParsedMacroBody CExpr
  -> [(C.ValOrRef, C.DeclId)]
parsedMacroDeps declIds (ParsedMacroBodyCExpr macro) =
    case macro of
      CExpr.Macro _ _ _ expr -> goExpr C.ByValue expr
  where
    goExpr :: C.ValOrRef -> CExpr.Expr ctx CExpr.Ps -> [(C.ValOrRef, C.DeclId)]
    goExpr depTy = \case
      CExpr.Term term              -> goTerm depTy term
      -- Pointer: switch the dependency type to 'ByRef'.
      CExpr.TyApp CExpr.Pointer xs -> concatMap (goExpr C.ByRef) xs
      CExpr.TyApp CExpr.Const   xs -> concatMap (goExpr depTy)   xs
      CExpr.VaApp _ _ xs           -> concatMap (goExpr depTy)   xs

    goTerm :: C.ValOrRef -> CExpr.Term ctx CExpr.Ps -> [(C.ValOrRef, C.DeclId)]
    goTerm depTy = \case
      CExpr.Literal lit  -> goLit depTy lit
      CExpr.LocalParam{} -> []
      CExpr.Var _ nm args
        | Just x <- resolveBare nm.getName ->
            (depTy, x) : concatMap (goExpr depTy) args
        | otherwise ->
            concatMap (goExpr depTy) args

    goLit :: C.ValOrRef -> CExpr.Literal -> [(C.ValOrRef, C.DeclId)]
    goLit depTy = \case
      CExpr.TypeTagged tag nm
        | Just a <- resolveTagged (convertTagKind tag) nm.getName -> [(depTy, a)]
        | otherwise -> []
      CExpr.TypeLit{}  -> []
      CExpr.ValueLit{} -> []

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1952>
    --
    -- We should only resolve the declaration IDs of macro dependencies once.
    -- Now we resolve them twice: when we get the dependencies of parsed macros,
    -- and when we typecheck macros.

    -- | Resolves a bare name found in a parsed macro body
    --
    -- Result:
    --
    -- - @'Just' 'CNameKind'@: This bare name refers to another declaration that is in
    --   the 'DeclIndex'. For example, a macro, or a @typedef@.
    --
    -- - @'Nothing'@: This bare name is not in the set of known declarations. It
    --   may simply be non-existent, or refer to a built-in type or a system
    --   @typedef@ not present in the 'DeclIndex'. In this case, we /do not
    --   generate/ a dependency.
    resolveBare :: Text -> Maybe C.DeclId
    resolveBare nm
      | macroId   `Set.member` declIds = Just macroId
      | typedefId `Set.member` declIds = Just typedefId
      | otherwise                         = Nothing
      where
        macroId, typedefId :: C.DeclId
        macroId   = C.DeclId (C.DeclName nm C.NameKindMacro)    False
        typedefId = C.DeclId (C.DeclName nm C.NameKindOrdinary) False

    resolveTagged :: C.TagKind -> Text -> Maybe C.DeclId
    resolveTagged tag nm
      | taggedId `Set.member` declIds = Just taggedId
      | otherwise                     = Nothing
      where
        taggedId :: C.DeclId
        taggedId = C.DeclId (C.DeclName nm $ C.NameKindTagged tag) False
