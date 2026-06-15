module HsBindgen.Macro.Parse (
    parseMacroBody
  , parsedMacroDeps
  ) where

import Data.Set qualified as Set

import C.Expr.Parse qualified as CExpr
import C.Expr.Syntax qualified as CExpr

import Clang.CStandard
import Clang.HighLevel.Types

import HsBindgen.Frontend.AST.Type
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
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
     Set DeclId
  -> ParsedMacroBody CExpr
  -> [(ValOrRef, DeclId)]
parsedMacroDeps declIds (ParsedMacroBodyCExpr macro) =
    case macro of
      CExpr.Macro _ _ _ expr -> goExpr ByValue expr
  where
    goExpr :: ValOrRef -> CExpr.Expr ctx CExpr.Ps -> [(ValOrRef, DeclId)]
    goExpr depTy = \case
      CExpr.Term term               -> goTerm depTy term
      -- Pointer: switch the dependency type to 'ByRef'.
      CExpr.TyApp CExpr.Pointer xs  -> concatMap (goExpr ByRef)  xs
      CExpr.TyApp CExpr.Const   xs  -> concatMap (goExpr depTy)  xs
      CExpr.VaApp _ _ xs            -> concatMap (goExpr depTy)  xs

    goTerm :: ValOrRef -> CExpr.Term ctx CExpr.Ps -> [(ValOrRef, DeclId)]
    goTerm depTy = \case
      CExpr.Literal lit   -> goLit depTy lit
      CExpr.LocalParam{}  -> []
      CExpr.Var _ nm args
        | Just x <- resolveBare nm.getName ->
            (depTy, x) : concatMap (goExpr depTy) args
        | otherwise ->
            concatMap (goExpr depTy) args

    goLit :: ValOrRef -> CExpr.Literal -> [(ValOrRef, DeclId)]
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
    resolveBare :: Text -> Maybe DeclId
    resolveBare nm
      | macroId   `Set.member` declIds = Just macroId
      | typedefId `Set.member` declIds = Just typedefId
      | otherwise                         = Nothing
      where
        macroId, typedefId :: DeclId
        macroId   = DeclId (CDeclName nm CNameKindMacro)    False
        typedefId = DeclId (CDeclName nm CNameKindOrdinary) False

    resolveTagged :: CTagKind -> Text -> Maybe DeclId
    resolveTagged tag nm
      | taggedId `Set.member` declIds = Just taggedId
      | otherwise                        = Nothing
      where
        taggedId :: DeclId
        taggedId = DeclId (CDeclName nm $ CNameKindTagged tag) False
