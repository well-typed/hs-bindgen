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

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.CExpr
import HsBindgen.Macro.Interface

typecheckMacroBodies ::
     Set C.DeclId
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
                              , declId.name.kind == C.NameKindOrdinary ]

    convertResult ::
         CExpr.MacroTcResult MacroTypecheckError C.DeclId
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
      -> C.DeclId
    injectTypeName = \case
        CExpr.FromTypedef ->
          \n -> C.DeclId (C.DeclName n.getName C.NameKindOrdinary) False
        CExpr.FromMacroType ->
          \n -> C.DeclId (C.DeclName n.getName C.NameKindMacro) False

    injectValueName :: CExpr.Name -> C.DeclId
    injectValueName (CExpr.Name n) =
        let dn = C.DeclName{text = n , kind = C.NameKindMacro}
        in  C.DeclId dn False

    injectNonAnonTaggedTypeName ::
         CExpr.TagKind
      -> CExpr.Name
      -> Except MacroTypecheckError C.DeclId
    injectNonAnonTaggedTypeName k n =
        if Set.member declId declsInScope then
          pure declId
        else
          throwError  $ MacroTypecheckUnresolvedTaggedType declId
      where
        declId :: C.DeclId
        declId = C.DeclId{
            name = C.DeclName{
              text = n.getName
            , kind = C.NameKindTagged (convertTagKind k)
            }
          , isAnon = False
          }

typecheckedMacroTypeDeps ::
     TypecheckedMacroTypeBody CExpr C.DeclId
  -> [(C.ValOrRef, C.DeclId)]
typecheckedMacroTypeDeps (TypecheckedMacroTypeBodyCExpr tcExpr) =
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
