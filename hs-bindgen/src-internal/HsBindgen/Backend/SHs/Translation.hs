-- | Simplified HS translation (from high level HS)
module HsBindgen.Backend.SHs.Translation (
    translateDecls,
    translateType,
) where

-- previously Backend.Common.Translation

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Type.Nat qualified as Fin
import Data.Vec.Lazy qualified as Vec

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation (Comment)
import HsBindgen.Backend.SHs.AST
import HsBindgen.Errors
import HsBindgen.Frontend.Macro qualified as Macro
import HsBindgen.Frontend.RootHeader (getHashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.NameHint

import DeBruijn (rzeroAdd)
import DeBruijn.Internal.Size (Size (UnsafeSize))
import Witherable (ordNub)

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

translateDecls :: [Hs.Decl] -> [SDecl]
translateDecls decls
    | null csources' =                      concatMap translateDecl decls
    | otherwise      = DCSource csources' : concatMap translateDecl decls
  where
    csources' = getCSources decls

-- Find and assemble C sources required by foreign imports.
getCSources :: [Hs.Decl] -> String
getCSources decls = unlines $ headers ++ bodies
  where
    getUserlandCapiWrapper :: Hs.Decl -> Maybe UserlandCapiWrapper
    getUserlandCapiWrapper = \case
      Hs.DeclForeignImport (Hs.ForeignImportDecl{foreignImportCallConv}) ->
        case foreignImportCallConv of
          CallConvUserlandCAPI w -> Just w
          _otherCallConv         -> Nothing
      _otherDecl          -> Nothing

    wrappers :: [UserlandCapiWrapper]
    wrappers = mapMaybe getUserlandCapiWrapper decls

    getImport :: UserlandCapiWrapper -> String
    getImport =
      (\h -> "#include <" ++ getHashIncludeArg h ++ ">") . capiWrapperImport

    -- It is important that we don't include the same header more than once,
    -- /especially/ for non-extern non-static globals.
    headers, bodies :: [String]
    headers = ordNub $ map getImport wrappers
    bodies = map capiWrapperDefinition wrappers

translateDecl :: Hs.Decl -> [SDecl]
translateDecl (Hs.DeclData d) = singleton $ translateDeclData d
translateDecl (Hs.DeclEmpty d) = singleton $ translateDeclEmpty d
translateDecl (Hs.DeclNewtype n) = singleton $ translateNewtype n
translateDecl (Hs.DeclDefineInstance i) = singleton $ translateDefineInstanceDecl i
translateDecl (Hs.DeclDeriveInstance i) = singleton $ translateDeriveInstance i
translateDecl (Hs.DeclVar v) = singleton $ translateVarDecl v
translateDecl (Hs.DeclForeignImport i) = translateForeignImportDecl i
translateDecl (Hs.DeclPatSyn ps) = singleton $ translatePatSyn ps
translateDecl (Hs.DeclUnionGetter u) = singleton $ translateUnionGetter u
translateDecl (Hs.DeclUnionSetter u) = singleton $ translateUnionSetter u
translateDecl (Hs.DeclSimple d) = [d]

translateDefineInstanceDecl :: Hs.DefineInstance -> SDecl
translateDefineInstanceDecl Hs.DefineInstance {..} =
  case defineInstanceDeclarations of
    Hs.InstanceStorable struct i -> DInst $ translateStorableInstance struct i defineInstanceComment
    Hs.InstanceHasFLAM struct fty i -> DInst
      Instance
        { instanceClass   = HasFlexibleArrayMember_class
        , instanceArgs    = [ translateType fty, TCon $ Hs.structName struct ]
        , instanceTypes   = []
        , instanceDecs    = [(HasFlexibleArrayMember_offset, ELam "_ty" $ EIntegral (toInteger i) Nothing)]
        , instanceComment = defineInstanceComment
        }
    Hs.InstanceCEnum struct fTyp vMap isSequential ->
      DInst $ translateCEnumInstance struct fTyp vMap isSequential defineInstanceComment
    Hs.InstanceSequentialCEnum struct nameMin nameMax ->
      DInst $ translateSequentialCEnum struct nameMin nameMax defineInstanceComment
    Hs.InstanceCEnumShow struct ->
      DInst $ translateCEnumInstanceShow struct defineInstanceComment
    Hs.InstanceCEnumRead struct ->
      DInst $ translateCEnumInstanceRead struct defineInstanceComment

translateDeclData :: Hs.Struct n -> SDecl
translateDeclData struct = DRecord
  Record
    { dataType = Hs.structName struct
    , dataCon  = Hs.structConstr struct
    , dataFields =
        [ Field {
              fieldName    = Hs.fieldName f
            , fieldType    = translateType $ Hs.fieldType f
            , fieldOrigin  = Hs.fieldOrigin f
            , fieldComment = Hs.fieldComment f
            }
        | f <- toList $ Hs.structFields struct
        ]
    , dataOrigin =
        case Hs.structOrigin struct of
          Just origin -> origin
          Nothing     -> panicPure "Missing structOrigin"
    , dataDeriv   = []
    , dataComment = Hs.structComment struct
    }

translateDeclEmpty :: Hs.EmptyData -> SDecl
translateDeclEmpty d = DEmptyData
  EmptyData
    { emptyDataName    = Hs.emptyDataName d
    , emptyDataOrigin  = Hs.emptyDataOrigin d
    , emptyDataComment = Hs.emptyDataComment d
    }

translateNewtype :: Hs.Newtype -> SDecl
translateNewtype n = DNewtype
  Newtype
    { newtypeName   = Hs.newtypeName n
    , newtypeCon    = Hs.newtypeConstr n
    , newtypeField  = Field {
          fieldName    = Hs.fieldName $ Hs.newtypeField n
        , fieldType    = translateType . Hs.fieldType $ Hs.newtypeField n
        , fieldOrigin  = Hs.fieldOrigin $ Hs.newtypeField n
        , fieldComment = Hs.fieldComment $ Hs.newtypeField n
        }
    , newtypeOrigin  = Hs.newtypeOrigin n
    , newtypeDeriv   = []
    , newtypeComment = Hs.newtypeComment n
    }

translateDeriveInstance :: Hs.DeriveInstance -> SDecl
translateDeriveInstance Hs.DeriveInstance{..} = DDerivingInstance
  DerivingInstance {
        derivingInstanceStrategy = fmap translateType deriveInstanceStrategy
      , derivingInstanceType     = TApp (translateTypeClass deriveInstanceClass) (TCon deriveInstanceName)
      , derivingInstanceComment  = deriveInstanceComment
      }

translateTypeClass :: HsTypeClass -> ClosedType
translateTypeClass Hs.Bits       = TGlobal Bits_class
translateTypeClass Hs.Bounded    = TGlobal Bounded_class
translateTypeClass Hs.Enum       = TGlobal Enum_class
translateTypeClass Hs.Eq         = TGlobal Eq_class
translateTypeClass Hs.FiniteBits = TGlobal FiniteBits_class
translateTypeClass Hs.Floating   = TGlobal Floating_class
translateTypeClass Hs.Fractional = TGlobal Fractional_class
translateTypeClass Hs.Integral   = TGlobal Integral_class
translateTypeClass Hs.Ix         = TGlobal Ix_class
translateTypeClass Hs.Num        = TGlobal Num_class
translateTypeClass Hs.Ord        = TGlobal Ord_class
translateTypeClass Hs.Read       = TGlobal Read_class
translateTypeClass Hs.ReadRaw    = TGlobal ReadRaw_class
translateTypeClass Hs.Real       = TGlobal Real_class
translateTypeClass Hs.RealFloat  = TGlobal RealFloat_class
translateTypeClass Hs.RealFrac   = TGlobal RealFrac_class
translateTypeClass Hs.Show       = TGlobal Show_class
translateTypeClass Hs.StaticSize = TGlobal StaticSize_class
translateTypeClass Hs.Storable   = TGlobal Storable_class
translateTypeClass Hs.WriteRaw   = TGlobal WriteRaw_class

translateVarDecl :: Hs.VarDecl -> SDecl
translateVarDecl Hs.VarDecl {..} = DVar
  Var { varName    = varDeclName
      , varType    = translateSigma varDeclType
      , varExpr    = translateBody varDeclBody
      , varComment = varDeclComment
      }

translateForeignImportDecl :: Hs.ForeignImportDecl -> [SDecl]
translateForeignImportDecl Hs.ForeignImportDecl { foreignImportParameters = args
                                                , foreignImportResultType = resType
                                                , ..
                                                } =
    [  DForeignImport ForeignImport
        { foreignImportParameters =
          map (\Hs.FunctionParameter { functionParameterType = argType
                                     , ..
                                     } ->
                    FunctionParameter
                      { functionParameterType = translateType argType
                      , ..
                      }
                ) args
        , foreignImportResultType = fmap translateType resType
        , ..
        }
    ]

translatePatSyn :: Hs.PatSyn -> SDecl
translatePatSyn Hs.PatSyn {..} = DPatternSynonym
  PatternSynonym
    { patSynName
    , patSynOrigin
    , patSynComment
    , patSynType = TCon patSynType
    , patSynRHS  = PEApps patSynConstr [PELit patSynValue]
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

translateType :: Hs.HsType -> ClosedType
translateType = \case
    Hs.HsPrimType t         -> TGlobal (PrimType t)
    Hs.HsTypRef r           -> TCon r
    Hs.HsPtr t              -> TApp (TGlobal Foreign_Ptr) (translateType t)
    Hs.HsFunPtr t           -> TApp (TGlobal Foreign_FunPtr) (translateType t)
    Hs.HsConstArray n t     -> TGlobal ConstantArray `TApp` TLit n `TApp` (translateType t)
    Hs.HsIncompleteArray t  -> TGlobal IncompleteArray `TApp` (translateType t)
    Hs.HsIO t               -> TApp (TGlobal IO_type) (translateType t)
    Hs.HsFun a b            -> TFun (translateType a) (translateType b)
    Hs.HsExtBinding i t     -> TExt i t
    Hs.HsByteArray          -> TGlobal ByteArray_type
    Hs.HsSizedByteArray n m -> TGlobal SizedByteArray_type `TApp` TLit n `TApp` TLit m
    Hs.HsBlock t            -> TGlobal Block_type `TApp` translateType t
    Hs.HsComplexType t      -> TApp (TGlobal ComplexType) (translateType (HsPrimType t))

{-------------------------------------------------------------------------------
  Sigma/Phi/Tau types
-------------------------------------------------------------------------------}

translateSigma :: Hs.SigmaType -> ClosedType
translateSigma (Hs.ForallTy hints (Hs.QuantTy cts ty)) =
  TForall
    hints
    (rzeroAdd $ UnsafeSize $ length hints)
    (map translatePredTy cts)
    (translateTau ty)

translatePredTy :: Hs.PredType ctx -> SType ctx
translatePredTy (Hs.DictTy (Hs.AClass cls) args) =
  foldl' TApp (tyConGlobal cls) (fmap translateTau args)
translatePredTy (Hs.NomEqTy a b) =
  TGlobal NomEq_class `TApp` translateTau a `TApp` translateTau b

translateTau :: Hs.TauType ctx -> SType ctx
translateTau = \case
  Hs.FunTy a b -> TFun (translateTau a) (translateTau b)
  Hs.TyVarTy x -> TBound x
  Hs.TyConAppTy (Hs.ATyCon tc) args
    | Just ty <- simpleTyConApp tc args
    -> ty
    | otherwise
    -> foldl' TApp (tyConGlobal tc) (fmap translateTau args)

-- | Convert @IntLike inty@ and @FloatLike floaty@ to Haskell types.
--
-- The macro typechecker will never produce a type such as @IntLike a@ for
-- a skolem type variable @a@, because this type has no Haskell counterpart.
--
-- See 'HsBindgen.C.Tc.Macro.isAtomicType'.
simpleTyConApp :: Macro.TyCon args Macro.Ty -> [Hs.TauType ctx] -> Maybe (SType ctx)
simpleTyConApp
  (Macro.GenerativeTyCon (Macro.DataTyCon Macro.IntLikeTyCon))
  [Hs.TyConAppTy (Hs.ATyCon (Macro.GenerativeTyCon (Macro.DataTyCon (Macro.PrimIntInfoTyCon inty)))) []]
    = Just $ TGlobal $ PrimType $
        case inty of
          Macro.HsIntType -> HsPrimInt
          Macro.CIntegralType primIntTy -> hsPrimIntTy primIntTy
simpleTyConApp
  (Macro.GenerativeTyCon (Macro.DataTyCon Macro.FloatLikeTyCon))
  [Hs.TyConAppTy (Hs.ATyCon (Macro.GenerativeTyCon (Macro.DataTyCon (Macro.PrimFloatInfoTyCon floaty)))) []]
    = Just $ TGlobal $ PrimType $ hsPrimFloatTy floaty
simpleTyConApp _ _ = Nothing

tyConGlobal :: Macro.TyCon args res -> SType ctx
tyConGlobal = \case
  Macro.GenerativeTyCon tc ->
    case tc of
      Macro.DataTyCon dc ->
        case dc of
          Macro.TupleTyCon n ->
            TGlobal $ Tuple_type n
          Macro.VoidTyCon ->
            TGlobal $ PrimType HsPrimVoid
          Macro.PrimIntInfoTyCon inty ->
            TGlobal $ PrimType $
              case inty of
                Macro.CIntegralType primIntTy -> hsPrimIntTy primIntTy
                Macro.HsIntType -> HsPrimInt
          Macro.PrimFloatInfoTyCon floaty ->
            TGlobal $ PrimType $ hsPrimFloatTy floaty
          Macro.PtrTyCon ->
            TGlobal Foreign_Ptr
          Macro.CharLitTyCon ->
            TGlobal CharValue_tycon

          -- These two TyCons are handled by 'simpleTyConApp'.
          Macro.IntLikeTyCon   ->
            panicPure "tyConGlobal IntLikeTyCon"
          Macro.FloatLikeTyCon ->
            panicPure "tyConGlobal FloatLikeTyCon"

      Macro.ClassTyCon cls -> TGlobal $
        case cls of
          Macro.NotTyCon        -> Not_class
          Macro.LogicalTyCon    -> Logical_class
          Macro.RelEqTyCon      -> RelEq_class
          Macro.RelOrdTyCon     -> RelOrd_class
          Macro.PlusTyCon       -> Plus_class
          Macro.MinusTyCon      -> Minus_class
          Macro.AddTyCon        -> Add_class
          Macro.SubTyCon        -> Sub_class
          Macro.MultTyCon       -> Mult_class
          Macro.DivTyCon        -> Div_class
          Macro.RemTyCon        -> Rem_class
          Macro.ComplementTyCon -> Complement_class
          Macro.BitwiseTyCon    -> Bitwise_class
          Macro.ShiftTyCon      -> Shift_class
  Macro.FamilyTyCon tc -> TGlobal $
    case tc of
      Macro.PlusResTyCon       -> Plus_resTyCon
      Macro.MinusResTyCon      -> Minus_resTyCon
      Macro.AddResTyCon        -> Add_resTyCon
      Macro.SubResTyCon        -> Sub_resTyCon
      Macro.MultResTyCon       -> Mult_resTyCon
      Macro.DivResTyCon        -> Div_resTyCon
      Macro.RemResTyCon        -> Rem_resTyCon
      Macro.ComplementResTyCon -> Complement_resTyCon
      Macro.BitsResTyCon       -> Bitwise_resTyCon
      Macro.ShiftResTyCon      -> Shift_resTyCon

mfunGlobal :: Macro.MFun arity -> Global
mfunGlobal = \case
  Macro.MUnaryPlus  -> Plus_plus
  Macro.MUnaryMinus -> Minus_negate
  Macro.MLogicalNot -> Not_not
  Macro.MBitwiseNot -> Complement_complement
  Macro.MMult       -> Mult_mult
  Macro.MDiv        -> Div_div
  Macro.MRem        -> Rem_rem
  Macro.MAdd        -> Add_add
  Macro.MSub        -> Sub_minus
  Macro.MShiftLeft  -> Shift_shiftL
  Macro.MShiftRight -> Shift_shiftR
  Macro.MRelLT      -> RelOrd_lt
  Macro.MRelLE      -> RelOrd_le
  Macro.MRelGT      -> RelOrd_gt
  Macro.MRelGE      -> RelOrd_ge
  Macro.MRelEQ      -> RelEq_eq
  Macro.MRelNE      -> RelEq_uneq
  Macro.MBitwiseAnd -> Bitwise_and
  Macro.MBitwiseXor -> Bitwise_xor
  Macro.MBitwiseOr  -> Bitwise_or
  Macro.MLogicalAnd -> Logical_and
  Macro.MLogicalOr  -> Logical_or
  Macro.MTuple @n   -> Tuple_constructor $ 2 + Fin.reflectToNum @n Proxy

{-------------------------------------------------------------------------------
 VarDeclRHS
-------------------------------------------------------------------------------}

translateBody :: Hs.VarDeclRHS ctx -> SExpr ctx
translateBody (Hs.VarDeclVar x)                     = EBound x
translateBody (Hs.VarDeclFloat f)                   = EFloat f HsPrimCFloat
translateBody (Hs.VarDeclDouble d)                  = EDouble d HsPrimCDouble
translateBody (Hs.VarDeclIntegral i ty)             = EIntegral i (Just ty)
translateBody (Hs.VarDeclChar c)                    = EChar c
translateBody (Hs.VarDeclString s)                  = ECString s
translateBody (Hs.VarDeclLambda (Hs.Lambda hint b)) = ELam hint (translateBody b)
translateBody (Hs.VarDeclApp f as)                  = foldl' EApp (translateAppHead f) (map translateBody as)

translateAppHead :: Hs.VarDeclRHSAppHead -> SExpr ctx
translateAppHead = \case
  Hs.InfixAppHead mfun ->
    EGlobal $ mfunGlobal mfun
  Hs.VarAppHead macroNm -> do
    EFree macroNm

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

translateStorableInstance :: Hs.Struct n -> Hs.StorableInstance -> Maybe Comment -> Instance
translateStorableInstance struct Hs.StorableInstance{..} mbComment = do
    let peek = lambda (idiom structCon translatePeekByteOff) storablePeek
    let poke = lambda (lambda (translateElimStruct (doAll translatePokeByteOff))) storablePoke
    Instance
      { instanceClass = Storable_class
      , instanceArgs  = [TCon $ Hs.structName struct]
      , instanceTypes = []
      , instanceDecs  = [
            (Storable_sizeOf    , EUnusedLam $ EInt storableSizeOf)
          , (Storable_alignment , EUnusedLam $ EInt storableAlignment)
          , (Storable_peek      , peek)
          , (Storable_poke      , poke)
          ]
      , instanceComment = mbComment
      }

translatePeekByteOff :: Hs.PeekByteOff ctx -> SExpr ctx
translatePeekByteOff (Hs.PeekByteOff ptr i) = appMany Storable_peekByteOff [EBound ptr, EInt i]
translatePeekByteOff (Hs.PeekBitOffWidth ptr i w) = appMany Bitfield_peekBitOffWidth [EBound ptr, EInt i, EInt w] -- TODO

translatePokeByteOff :: Hs.PokeByteOff ctx -> SExpr ctx
translatePokeByteOff (Hs.PokeByteOff ptr i x) = appMany Storable_pokeByteOff [EBound ptr, EInt i, EBound x]
translatePokeByteOff (Hs.PokeBitOffWidth ptr i w x) = appMany Bitfield_pokeBitOffWidth [EBound ptr, EInt i, EInt w, EBound x] -- TODO

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

translateElimStruct :: (forall ctx'. t ctx' -> SExpr ctx') -> Hs.ElimStruct t ctx -> SExpr ctx
translateElimStruct f (Hs.ElimStruct x struct add k) = ECase
    (EBound x)
    [SAlt (Hs.structConstr struct) add hints (f k)]
  where
    hints = fmap (toNameHint . Hs.fieldName) $ Hs.structFields struct

toNameHint :: HsName 'NsVar -> NameHint
toNameHint (HsName t) = NameHint (T.unpack t)

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

translateUnionGetter :: Hs.UnionGetter -> SDecl
translateUnionGetter Hs.UnionGetter{..} = DVar
  Var { varName    = unionGetterName
      , varType    = TFun (TCon unionGetterConstr) (translateType unionGetterType)
      , varExpr    = EGlobal ByteArray_getUnionPayload
      , varComment = unionGetterComment
      }

translateUnionSetter :: Hs.UnionSetter -> SDecl
translateUnionSetter Hs.UnionSetter{..} = DVar
  Var { varName    = unionSetterName
      , varType    = (TFun (translateType unionSetterType) (TCon unionSetterConstr))
      , varExpr    = EGlobal ByteArray_setUnionPayload
      , varComment = unionSetterComment
      }

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

translateCEnumInstance ::
     Hs.Struct (S Z)
  -> HsType
  -> Map Integer (NonEmpty String)
  -> Bool
  -> Maybe Comment
  -> Instance
translateCEnumInstance struct fTyp vMap isSequential mbComment = Instance {
      instanceClass = CEnum_class
    , instanceArgs  = [tcon]
    , instanceTypes = [(CEnumZ_tycon, tcon, translateType fTyp)]
    , instanceDecs  = [
          (CEnum_toCEnum, ECon (Hs.structConstr struct))
        , (CEnum_fromCEnum, EFree fname)
        , (CEnum_declaredValues, EUnusedLam declaredValuesE)
        , (CEnum_showsUndeclared, EApp (EGlobal CEnum_showsWrappedUndeclared) dconStrE)
        , (CEnum_readPrecUndeclared, EApp (EGlobal CEnum_readPrecWrappedUndeclared) dconStrE)
        ] ++ seqDecs
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

    dconStrE :: SExpr ctx
    dconStrE = EString . T.unpack $ getHsName (Hs.structConstr struct)

    fname :: HsName NsVar
    fname = Hs.fieldName $
      NonEmpty.head (Vec.toNonEmpty (Hs.structFields struct))

    declaredValuesE :: SExpr ctx
    declaredValuesE = EApp (EGlobal CEnum_declaredValuesFromList) $ EList [
        ETup [
            EIntegral v Nothing
          , if null names
              then EApp (EGlobal NonEmpty_singleton) (EString name)
              else
                EInfix
                  NonEmpty_constructor
                  (EString name)
                  (EList (EString <$> names))
          ]
      | (v, name :| names) <- Map.toList vMap
      ]

    seqDecs :: [(Global, ClosedExpr)]
    seqDecs
      | isSequential = [
            (CEnum_isDeclared, EGlobal CEnum_seqIsDeclared)
          , (CEnum_mkDeclared, EGlobal CEnum_seqMkDeclared)
          ]
      | otherwise = []

translateSequentialCEnum ::
     Hs.Struct (S Z)
  -> HsName NsConstr
  -> HsName NsConstr
  -> Maybe Comment
  -> Instance
translateSequentialCEnum struct nameMin nameMax mbComment = Instance {
      instanceClass = SequentialCEnum_class
    , instanceArgs  = [tcon]
    , instanceTypes = []
    , instanceDecs  = [
          (SequentialCEnum_minDeclaredValue, ECon nameMin)
        , (SequentialCEnum_maxDeclaredValue, ECon nameMax)
        ]
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

translateCEnumInstanceShow ::
     Hs.Struct (S Z)
  -> Maybe Comment
  -> Instance
translateCEnumInstanceShow struct mbComment = Instance {
      instanceClass = Show_class
    , instanceArgs  = [tcon]
    , instanceTypes = []
    , instanceDecs  = [
          (Show_showsPrec, EGlobal CEnum_showsCEnum)
        ]
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

translateCEnumInstanceRead ::
     Hs.Struct (S Z)
  -> Maybe Comment
  -> Instance
translateCEnumInstanceRead struct mbComment = Instance {
      instanceClass = Read_class
    , instanceArgs  = [tcon]
    , instanceTypes = []
    , instanceDecs  = [
          (Read_readPrec, EGlobal CEnum_readPrecCEnum)
        , (Read_readList, EGlobal Read_readListDefault)
        , (Read_readListPrec, EGlobal Read_readListPrecDefault)
        ]
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

{-------------------------------------------------------------------------------
  Internal auxiliary: derived functionality
-------------------------------------------------------------------------------}

-- | Apply function to many arguments
appMany :: Global -> [SExpr ctx] -> SExpr ctx
appMany = foldl' EApp . EGlobal

-- | Struct constructor
structCon :: Hs.StructCon ctx -> SExpr ctx
structCon (Hs.StructCon s) = ECon (Hs.structConstr s)

-- | Idiom brackets
idiom :: (pure ctx -> SExpr ctx) -> (xs ctx -> SExpr ctx) -> Hs.Ap pure xs ctx -> SExpr ctx
idiom f g (Hs.Ap p xs) = foldl'
    (\ acc x -> EInfix Applicative_seq acc (g x))
    (EApp (EGlobal Applicative_pure) (f p))
    xs

-- | Translate lambda
lambda :: (t (S ctx) -> SExpr (S ctx)) -> Hs.Lambda t ctx -> SExpr ctx
lambda f (Hs.Lambda hint t) = ELam hint (f t)

-- | Monad sequencing
doAll :: (t ctx -> SExpr ctx) -> Hs.Seq t ctx -> SExpr ctx
doAll _ (Hs.Seq []) = EGlobal Monad_return `EApp` EGlobal (Tuple_constructor 0)
doAll f (Hs.Seq ss) = foldr1 (EInfix Monad_seq) (map f ss)
