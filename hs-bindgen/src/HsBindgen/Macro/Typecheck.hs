module HsBindgen.Macro.Typecheck (
    typecheckMacroBodies
  , typecheckedMacroTypeDeps
  ) where

import Control.Monad.Except (Except, MonadError (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr
import C.Expr.Typecheck.Interface.Type qualified as T

import HsBindgen.Frontend.AST.Type
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Macro.CExpr
import HsBindgen.Macro.Interface

typecheckMacroBodies ::
     Set DeclId
  -> [ParsedMacroBody CExpr]
  -> Map Text (MacroTypecheckResult CExpr)
typecheckMacroBodies declsInScope bodies =
    Map.mapKeysMonotonic (.getName) $ fmap convertResult $
      CExpr.tcMacros
        typedefs
        injectTypeName
        injectValueName
        injectNonAnonTaggedTypeName
        [m | ParsedMacroBodyCExpr m <- bodies]
  where
    typedefs :: Set CExpr.Name
    typedefs = Set.fromList $ [ (CExpr.Name declId.name.text)
                              | declId <- Set.toList declsInScope
                              , declId.name.kind == CNameKindOrdinary ]

    convertResult ::
         CExpr.MacroTcResult MacroTypecheckError DeclId
      -> MacroTypecheckResult CExpr
    convertResult = \case
      CExpr.MacroTcTypeExpr x ->
        MacroTypecheckType  (TypecheckedMacroTypeBodyCExpr  x)
      CExpr.MacroTcValueExpr x ->
        MacroTypecheckValue (TypecheckedMacroValueBodyCExpr x)
      CExpr.MacroTcInjectError e ->
        MacroTypecheckError e
      CExpr.MacroTcError err ->
        MacroTypecheckError $
          MacroTypecheckTypecheckError $
            MacroLangTypecheckError (Text.unpack (CExpr.pprMacroTcError err))

    injectTypeName ::
         CExpr.CTypeSource
      -> CExpr.Name
      -> DeclId
    injectTypeName = \case
        CExpr.FromTypedef ->
          \n -> DeclId (CDeclName n.getName CNameKindOrdinary) False
        CExpr.FromMacroType ->
          \n -> DeclId (CDeclName n.getName CNameKindMacro) False

    injectValueName :: CExpr.Name -> DeclId
    injectValueName (CExpr.Name n) =
        let dn = CDeclName{text = n , kind = CNameKindMacro}
        in  DeclId dn False

    injectNonAnonTaggedTypeName ::
         CExpr.TagKind
      -> CExpr.Name
      -> Except MacroTypecheckError DeclId
    injectNonAnonTaggedTypeName k n =
        if Set.member declId declsInScope then
          pure declId
        else
          throwError  $ MacroTypecheckUnresolvedTaggedType declId
      where
        declId :: DeclId
        declId = DeclId{
            name = CDeclName{
              text = n.getName
            , kind = CNameKindTagged (convertTagKind k)
            }
          , isAnon = False
          }

typecheckedMacroTypeDeps ::
     TypecheckedMacroTypeBody CExpr DeclId -> [(ValOrRef, DeclId)]
typecheckedMacroTypeDeps (TypecheckedMacroTypeBodyCExpr tcExpr) =
    -- 'T.Expr' is a unary type-application tree, so it carries at most one variable.
    case go ByValue tcExpr.macroTypeBody of
      Nothing -> []
      Just x  -> [x]
  where
    go :: ValOrRef -> T.Expr DeclId -> Maybe (ValOrRef, DeclId)
    go depTy = \case
      T.TypeLit{}       -> Nothing
      T.App T.Pointer e -> go ByRef e
      T.App T.Const   e -> go depTy e
      T.Var v           -> Just (depTy, v)
