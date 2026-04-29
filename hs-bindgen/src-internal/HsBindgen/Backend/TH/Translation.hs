{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.TH.Translation (
    mkDecl,
) where

import Control.Monad (liftM2)
import Data.Set qualified as Set
import Data.Text qualified as Text
import DeBruijn (Add (..), EmptyCtx, Env (..), lookupEnv)
import Foreign.C.Types qualified
import GHC.Base qualified
import GHC.Exts (Int (..), sizeofByteArray#)
import GHC.Exts qualified as IsList (IsList (..))
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat,
                  castWord64ToDouble)
import GHC.Ptr (Ptr (Ptr))
import Language.Haskell.TH (Quote)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import C.Expr.Syntax qualified as CExpr.DSL

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Level
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.Translation.Common
import HsBindgen.Config (FieldNamingStrategy)
import HsBindgen.Errors
import HsBindgen.Frontend.Naming
import HsBindgen.Guasi
import HsBindgen.Imports
import HsBindgen.Instances as Inst
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

mkGlobalExpr :: Quote q => Global LvlTerm -> q TH.Exp
mkGlobalExpr g = case g.cat of
  GVar -> TH.varE g.name
  GCon -> TH.conE g.name

mkExpr :: Guasi q => Env ctx TH.Name -> SExpr ctx -> q TH.Exp
mkExpr env expr = case asNaryEApp expr of
    (EBoxedTup n, args) ->
      prettyTupleExpr Boxed env n args
    (EUnboxedTup n, args) ->
      prettyTupleExpr Unboxed env n args
    (EApp{} , _) ->
      panicWith
        "Unexpected function application after unrolling function applications"
        expr
    _otherwise -> mkRolledExpr env expr

-- See 'mkExpr' but do not unroll/recognize function applications.
mkRolledExpr :: Guasi q => Env ctx TH.Name -> SExpr ctx -> q TH.Exp
mkRolledExpr env expr = case expr of
    EGlobal n     -> mkGlobalExpr n
    EFree n       -> TH.varE $ mkHsTermName n
    EBound x      -> TH.varE (lookupEnv x env)
    ECon n        -> TH.conE $ mkHsName n
    EUnboxedIntegral i -> TH.sigE (TH.litE (TH.IntPrimL i)) (TH.conT ''GHC.Base.Int#)
    EIntegral i Nothing -> TH.litE (TH.IntegerL i)
    EIntegral i (Just t) -> TH.sigE (TH.litE (TH.IntegerL i)) (mkType EmptyEnv t)
    -- TH doesn't have floating-point literals, because it represents them
    -- using the Rational type, which is incorrect. (See GHC ticket #13124.)
    --
    -- To work around this problem, we cast floating-point numbers to
    -- Word32/Word64 and then cast back.
    EFloat f t ->
      TH.sigE
        ( if CExpr.DSL.canBeRepresentedAsRational f
            then [| f |]
            else [| Foreign.C.Types.CFloat $ castWord32ToFloat  $( TH.lift $ castFloatToWord32  f ) |]
        )
        (mkType EmptyEnv t)
    EDouble d t ->
      TH.sigE
        ( if CExpr.DSL.canBeRepresentedAsRational d
            then [| d |]
            else [| Foreign.C.Types.CDouble $ castWord64ToDouble $( TH.lift $ castDoubleToWord64 d ) |]
        )
        (mkType EmptyEnv t)
    EChar c -> [| c |]
    EString s -> [| s |]
    ECString ba@(ByteArray ba#) ->
      let
        len :: Integer
        len = fromIntegral (I# (sizeofByteArray# ba#))
      in
        TH.sigE
          ( TH.tupE [ TH.conE 'GHC.Ptr.Ptr `TH.appE` TH.litE (TH.StringPrimL $ IsList.toList ba)
                    , TH.litE (TH.integerL len)
                    ]
          )
        (TH.conT $ (.name) $ bindgenGlobalType CStringLen_type)
    EApp f x      -> TH.appE (mkExpr env f) (mkExpr env x)
    EInfix op x y -> TH.infixE
                       (Just $ mkExpr env x)
                       (mkGlobalExpr $ infixOpGlobal op)
                       (Just $ mkExpr env y)
    ELam (NameHint x) f      -> do
        x' <- TH.newName x
        TH.lamE [TH.varP x'] (mkExpr (env :> x') f)
    EUnusedLam f ->
        TH.lamE [TH.wildP] (mkExpr env f)
    ECase x alts  -> TH.caseE (mkExpr env x)
                       [ case alt of
                           SAlt c add hints b -> do
                             (xs, env') <- newNames env add hints
                             TH.match
                                (TH.conP (mkHsName c) (map TH.varP xs))
                                (TH.normalB $ mkExpr env' b)
                                []
                           SAltNoConstr hints b -> do
                             -- SAltNoConstr has one name hint only
                             -- guaranteed by the type.
                             (xs, env') <- newNames env (AS AZ) hints
                             case xs of
                               []    ->
                                 panicPure "Expected one name hint, but SAltNoConstr had none"
                               [v]   ->
                                 TH.match
                                   (TH.varP v)
                                   (TH.normalB $ mkExpr env' b)
                                   []
                               vs ->
                                 panicPure $ "Expected one name hint, but SAltNoConstr had more: " <> show vs
                           SAltUnboxedTuple add hints b -> do
                             (xs, env') <- newNames env add hints
                             TH.match
                                (TH.unboxedTupP $ map TH.varP xs)
                                (TH.normalB $ mkExpr env' b)
                                []
                       | alt <- alts
                       ]
    EUnit -> TH.tupE []
    -- Handled in 'mkExpr'.
    EBoxedTup{} ->
      panicWith
        "Unexpected boxed, unsaturated tuple after unrolling function applications"
        expr
    -- Handled in 'mkExpr'.
    EUnboxedTup{} ->
      panicWith
        "Unexpected unboxed, unsaturated tuple after unrolling function applications"
        expr
    EList xs -> TH.listE $ mkExpr env <$> xs
    ETypeApp f t -> TH.appTypeE (mkExpr env f) (mkType EmptyEnv t)

mkPat :: Quote q => PatExpr -> q TH.Pat
mkPat = \case
    PEApps n xs -> TH.conP (mkHsName n) (map mkPat xs)
    PELit i -> TH.litP (TH.IntegerL i)

mkType :: Guasi q => Env ctx TH.Name -> SType ctx -> q TH.Type
mkType env ty = case asNaryTApp ty of
    (TBoxedTup n, args) ->
      prettyTupleType env n args
    (TApp{}, _) ->
      panicWith
        "Unexpected type application after unrolling type applications"
        ty
    _otherwise ->
      mkRolledType env ty

-- See 'mkType' but do not unroll/recognize type applications.
mkRolledType :: Guasi q => Env ctx TH.Name -> SType ctx -> q TH.Type
mkRolledType env ty = case ty of
    TGlobal n  -> TH.conT n.name
    TClass cls -> TH.conT $ (.name) $ typeClassGlobal cls
    TBound x   -> TH.varT (lookupEnv x env)
    TCon n     -> TH.conT $ mkHsName n
    TFree n    -> TH.varT $ mkHsName n
    TLit n     -> TH.litT (TH.numTyLit (toInteger n))
    TStrLit s  -> TH.litT (TH.strTyLit s)
    TFun a b   -> TH.arrowT `TH.appT` mkType env a `TH.appT` mkType env b
    TApp f t   -> TH.appT (mkType env f) (mkType env t)
    TUnit -> pure $ TH.TupleT 0
    -- Handled in 'mkType'.
    TBoxedTup{} ->
      panicWith
        "Unexpected unsaturated tuple after unrolling type application"
        ty
    TEq -> TH.conT ''(~)
    TForall hints add ctxt body -> do
        let bndr tv = TH.PlainTV tv TH.SpecifiedSpec
        (xs, env') <- newNames env add hints
        TH.forallT
            (map bndr xs)
            (traverse (mkType env') ctxt)
            (mkType env' body)
    TExt extRef _cTypeSpec _hsTypeSpec ->
        lookupExtType extRef

mkDecl :: forall q. Guasi q => FieldNamingStrategy -> SDecl -> q [TH.Dec]
mkDecl fns = \case
      DTypSyn typSyn -> do
        targetType <- mkType EmptyEnv typSyn.typ
        pure [TH.TySynD (mkHsName typSyn.name) [] targetType]
      DInst inst -> do
        instanceDec <-
          TH.instanceD
            (return [])
            (TH.forallT
              []
              (mapM (mkType EmptyEnv) inst.super)
              (appsT (TH.conT $ (.name) $ typeClassGlobal inst.clss)
                (map (mkType EmptyEnv) inst.args)))
            (concat [
                map instTySyn inst.types
              , map (\(x, f) -> simpleDecl x.name f) inst.decs
              ])

        -- TODO <https://github.com/well-typed/hs-bindgen/issues/976>
        -- We should add haddock comment to the class head, but we cannot due to
        -- a GHC bug. We also don't put the comments on any of the class members
        -- (type synonyms / functions) because that leads to similar bugs.
        --
        -- putDocTypeM _type_ inst.comment
        pure [instanceDec]

      DRecord record -> do
        let fields :: [q TH.VarBangType]
            docs   :: [(Hs.Name Hs.NsVar, Maybe HsDoc.Comment)]
            (fields, docs) = unzip
              [ ( TH.varBangType (mkHsName field.name) $
                    TH.bangType
                      (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness)
                      (mkType EmptyEnv field.typ)
                , (field.name, field.comment)
                )
              | field <- record.fields
              ]
        traverse_ (uncurry (putLocalFieldDocM fns record.con)) docs

        decl <-
          TH.dataD
            (TH.cxt [])
            (mkHsName record.typ)
            []
            Nothing
            [TH.recC (mkHsName record.con) fields]
            (nestedDeriving record.deriv)
        putLocalDocM record.typ record.comment

        pure [decl]

      DEmptyData empty -> do
        decl <- TH.dataD (TH.cxt []) (mkHsName empty.name) [] Nothing [] []
        putLocalDocM empty.name empty.comment
        pure [decl]

      DNewtype newtyp -> do
        let field :: q TH.VarBangType
            field = TH.varBangType (mkHsName newtyp.field.name) $
              TH.bangType
                (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness)
                (mkType EmptyEnv newtyp.field.typ)

        putLocalFieldDocM fns newtyp.con newtyp.field.name newtyp.field.comment

        decl <-
          TH.newtypeD
            (TH.cxt [])
            (mkHsName newtyp.name)
            []
            Nothing
            (TH.recC (mkHsName newtyp.con) [field])
            (nestedDeriving newtyp.deriv)
        putLocalDocM (newtyp.name) (newtyp.comment)
        pure [decl]

      DDerivingInstance deriv -> do
        s' <- strategy deriv.strategy

        -- NOTE: We can't attach documentation to standalone deriving clauses.
        -- See 'GHC.Internal.TH.Lib.withDecDoc.doc_loc'.
        fmap singleton $ TH.standaloneDerivWithStrategyD
          (Just s')
          (TH.cxt [])
          (mkType EmptyEnv deriv.typ)

      DForeignImport foreignImport -> do
        let safety :: TH.Safety
            safety = case foreignImport.safety of
              Safe   -> TH.Safe
              Unsafe -> TH.Unsafe

            callconv :: TH.Callconv
            impent   :: String
            (callconv, impent) =
              case foreignImport.callConv of
                CallConvUserlandCapi _ -> (TH.CCall,
                    Text.unpack foreignImport.origName.text
                  )
                CallConvGhcCapi header -> (TH.CApi, concat [
                    header
                  , Text.unpack foreignImport.origName.text
                  ])
                CallConvGhcCCall style -> (TH.CCall, concat [
                    case style of
                      ImportAsValue -> ""
                      ImportAsPtr   -> "&"
                  , Text.unpack foreignImport.origName.text
                  ])

            importType = foldr (TFun . (.typ)) foreignImport.result.typ foreignImport.parameters

        decl <-
          fmap TH.ForeignD $
            TH.ImportF
              <$> pure callconv
              <*> pure safety
              <*> pure impent
              <*> pure (mkHsTermName foreignImport.name)
              <*> mkType EmptyEnv importType
        putLocalDocTermNameM foreignImport.name foreignImport.comment
        pure [decl]

      DBinding binding -> do
        let bindingName :: TH.Name
            bindingName = mkHsTermName binding.name
            bindingType :: SType EmptyCtx
            bindingType = foldr (TFun . (.typ)) binding.result.typ binding.parameters

        decls <- sequence $
          map (pragma binding.name) binding.pragmas
          ++ [
                TH.SigD <$> pure bindingName
                  <*> mkType EmptyEnv bindingType
            , simpleDecl bindingName binding.body
            ]
        putLocalDocTermNameM binding.name binding.comment
        pure decls

      DPatternSynonym patSyn -> do
        let thPatSynName = mkHsName patSyn.name

        decls <- sequence
          [ TH.patSynSigD
              thPatSynName
              (mkType EmptyEnv patSyn.typ)
          , TH.patSynD
            thPatSynName
            (TH.prefixPatSyn [])
            TH.implBidir
            (mkPat patSyn.rhs)
          ]
        putLocalDocM patSyn.name patSyn.comment
        pure decls
    where
      simpleDecl :: TH.Name -> SExpr EmptyCtx -> q TH.Dec
      simpleDecl x f = TH.valD (TH.varP x) (TH.normalB $ mkExpr EmptyEnv f) []

      instTySyn :: (Global LvlType, [ClosedType], ClosedType) -> q TH.Dec
      instTySyn (g, typArgs, typSyn) =
        TH.TySynInstD
          <$> liftM2
                (TH.TySynEqn Nothing)
                (mkType EmptyEnv (foldl (\acc x -> acc `TApp` x) (TGlobal g) typArgs))
                (mkType EmptyEnv typSyn)

      pragma :: Hs.TermName -> Pragma -> q TH.Dec
      pragma n = \case
        NOINLINE ->
            TH.pragInlD (mkHsTermName n) TH.NoInline TH.FunLike TH.AllPhases

-- | Nested deriving clauses (part of a datatype declaration)
nestedDeriving :: forall q.
     Guasi q
  => [(Hs.Strategy ClosedType, [Inst.TypeClass])] -> [q TH.DerivClause]
nestedDeriving = map aux
  where
    aux :: (Hs.Strategy ClosedType, [Inst.TypeClass]) -> q TH.DerivClause
    aux (s, clss) = do
        s' <- strategy s
        TH.derivClause (Just s') (map (TH.conT . (.name) . typeClassGlobal) clss)

strategy :: Guasi q => Hs.Strategy ClosedType -> q TH.DerivStrategy
strategy Hs.DeriveNewtype  = return TH.NewtypeStrategy
strategy Hs.DeriveStock    = return TH.StockStrategy
strategy (Hs.DeriveVia ty) = TH.ViaStrategy <$> mkType EmptyEnv ty

{-------------------------------------------------------------------------------
  Monad functionality
-------------------------------------------------------------------------------}

appsT :: Quote q => q TH.Type -> [q TH.Type] -> q TH.Type
appsT = foldl' TH.appT

-- | Create a 'TH.name' from an 'Hs.Name'
--
-- Be careful! This function uses 'TH.mkName'. Names created with 'TH.mkName'
-- are resolved in the context of the use site of the splice. That is, used
-- symbols /must be in scope/, and users must import the probably only
-- indirectly-used modules.
mkHsName :: Hs.Name ns -> TH.Name
mkHsName = TH.mkName . Hs.nameToStr

-- | Create a 'TH.name' from an 'Hs.TermName'
--
-- Be careful! This function uses 'TH.mkName'. Names created with 'TH.mkName'
-- are resolved in the context of the use site of the splice. That is, used
-- symbols /must be in scope/, and users must import the probably only
-- indirectly-used modules.
mkHsTermName :: Hs.TermName -> TH.Name
mkHsTermName = TH.mkName . Hs.termNameToStr

newNames ::
     Quote q
  => Env ctx TH.Name
  -> Add n ctx ctx'
  -> Vec n NameHint
  -> q ([TH.Name], Env ctx' TH.Name)
newNames env AZ _ = return ([], env)
newNames env (AS n) (NameHint hint ::: hints) = do
    (xs, env') <- newNames env n hints
    x <- TH.newName hint
    return (x : xs, env' :> x)


putLocalFieldDocM ::
     Guasi g
  => FieldNamingStrategy
  -> Hs.Name Hs.NsConstr
  -> Hs.Name Hs.NsVar
  -> Maybe HsDoc.Comment
  -> g ()
putLocalFieldDocM fns parent field = traverse_ (putLocalFieldDoc fns parent field)

{-------------------------------------------------------------------------------
  Tuples
-------------------------------------------------------------------------------}

data TupleType = Boxed | Unboxed

prettyTupleExpr ::
     Guasi q
  => TupleType
  -> Env ctx TH.Name
  -> Plus2
  -> [SExpr ctx]
  -> q TH.Exp
prettyTupleExpr ty env n decls = case compare arity nDecls of
  LT ->
    panicPure $ mconcat [
        "Too many declarations ("
      , show nDecls
      , ") for "
      , show arity ++ "-tuple"
      ]
  _otherwise -> do
    declExprs <- mapM (mkExpr env) decls
    let nMissing :: Int
        nMissing = arity - nDecls

        fakeDecls :: [Maybe TH.Exp]
        fakeDecls = map Just declExprs ++ replicate nMissing Nothing
    pure $ thTupleCon fakeDecls
  where
    arity, nDecls :: Int
    arity  = fromIntegral (applyPlus2 n)
    nDecls = length decls

    thTupleCon :: [Maybe TH.Exp] -> TH.Exp
    thTupleCon = case ty of
      Boxed   -> TH.TupE
      Unboxed -> TH.UnboxedTupE

prettyTupleType :: Guasi q => Env ctx TH.Name -> Plus2 -> [SType ctx] -> q TH.Type
prettyTupleType env n decls = case compare arity nDecls of
  LT ->
    panicPure $ mconcat [
        "Too many declarations ("
      , show nDecls
      , ") for "
      , show arity ++ "-tuple"
      ]
  _otherwise -> foldl' TH.appT (TH.tupleT arity) $ mkType env <$> decls
  where
    arity, nDecls :: Int
    arity  = fromIntegral (applyPlus2 n)
    nDecls = length decls

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

panicWith :: Show a => String -> a -> b
panicWith msg x = panicPure $ msg ++ ": " ++ show x

-- | Look up a type name from an external binding spec.
--
-- If the name is not in scope, report a helpful error message telling the user
-- which module they need to import, then falls back to 'TH.mkName' to avoid
-- cascading type errors.
--
-- See https://github.com/well-typed/hs-bindgen/issues/1622.
lookupExtType :: Guasi q => Hs.ExtRef -> q TH.Type
lookupExtType extRef = do
    mName <- lookupTypeName qualName
    case mName of
      Just n  -> TH.conT n
      Nothing -> do
          modifyGuasi (putMissingModule extRef.moduleName)
          TH.conT (TH.mkName qualName)
  where
    qualName :: String
    qualName = concat [
          Hs.moduleNameToString extRef.moduleName
        , "."
        , Text.unpack extRef.name.text
        ]

    putMissingModule :: Hs.ModuleName -> GuasiState -> GuasiState
    putMissingModule m s = s & #missingModules %~ Set.insert m

