-- | Simplified HS translation (from high level HS)
module HsBindgen.SHs.Translation (
    translateDecl,
) where

-- previously Backend.Common.Translation

import Data.Text qualified as T

import HsBindgen.C.AST qualified as C (MFun(..))
import HsBindgen.C.Tc.Macro qualified as C (DataTyCon(..), ClassTyCon(..))
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Hs.Translation (integralType, floatingType)
import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.SHs.AST

import DeBruijn (rzeroAdd)

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

translateDecl :: Hs.Decl -> SDecl
translateDecl (Hs.DeclData d) = translateDeclData d
translateDecl (Hs.DeclEmpty n) = translateDeclEmpty n
translateDecl (Hs.DeclNewtype n) = translateNewtype n
translateDecl (Hs.DeclInstance i) = translateInstanceDecl i
translateDecl (Hs.DeclNewtypeInstance tc c) = translateNewtypeInstance tc c
translateDecl (Hs.DeclVar v) = translateVarDecl v
translateDecl (Hs.DeclForeignImport i) = translateForeignImportDecl i
translateDecl (Hs.DeclPatSyn ps) = translatePatSyn ps

translateInstanceDecl :: Hs.InstanceDecl -> SDecl
translateInstanceDecl (Hs.InstanceStorable struct i) =
    DInst $ translateStorableInstance struct i

translateDeclData :: Hs.Struct n -> SDecl
translateDeclData struct = DRecord $ Record
    { dataType = Hs.structName struct
    , dataCon  = Hs.structConstr struct
    , dataFields =
        [ Field {
              fieldName = Hs.fieldName f
            , fieldType = translateType $ Hs.fieldType f
            }
        | f <- toList $ Hs.structFields struct
        ]
    }

translateDeclEmpty :: HsName NsTypeConstr -> SDecl
translateDeclEmpty n = DEmptyData n

translateNewtype :: Hs.Newtype -> SDecl
translateNewtype n = DNewtype $ Newtype
    { newtypeName  = Hs.newtypeName n
    , newtypeCon   = Hs.newtypeConstr n
    , newtypeField = Field {
          fieldName = Hs.fieldName $ Hs.newtypeField n
        , fieldType = translateType . Hs.fieldType $ Hs.newtypeField n
        }
    }

translateNewtypeInstance :: Hs.TypeClass -> HsName NsTypeConstr -> SDecl
translateNewtypeInstance tc n = DDerivingNewtypeInstance $ TApp (translateTypeClass tc) (TCon n)

translateTypeClass :: Hs.TypeClass -> ClosedType
translateTypeClass Hs.Storable = TGlobal Storable_Storable

translateVarDecl :: Hs.VarDecl -> SDecl
translateVarDecl Hs.VarDecl {..} = DVar
    varDeclName
    (Just (translateSigma varDeclType))
    (translateBody varDeclBody)

translateForeignImportDecl :: Hs.ForeignImportDecl -> SDecl
translateForeignImportDecl Hs.ForeignImportDecl {..} = DForeignImport ForeignImport
    { foreignImportName     = foreignImportName
    , foreignImportType     = translateType foreignImportType
    , foreignImportOrigName = foreignImportOrigName
    , foreignImportHeader   = foreignImportHeader
    }

translatePatSyn :: Hs.PatSyn -> SDecl
translatePatSyn Hs.PatSyn {..} = DPatternSynonym PatternSynonym
    { patSynName = patSynName
    , patSynType = TCon patSynType
    , patSynRHS  = EApp (ECon patSynConstr) (EIntegral patSynValue Nothing)
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

translateType :: Hs.HsType -> ClosedType
translateType (Hs.HsPrimType t)     = TGlobal (PrimType t)
translateType (Hs.HsTypRef r)       = TCon r
translateType (Hs.HsPtr t)          = TApp (TGlobal Foreign_Ptr) (translateType t)
translateType (Hs.HsFunPtr t)       = TApp (TGlobal Foreign_FunPtr) (translateType t)
translateType (Hs.HsConstArray n t) = TGlobal ConstantArray `TApp` TLit n `TApp` (translateType t)
translateType (Hs.HsType _)         = TGlobal (PrimType HsPrimVoid)
translateType (Hs.HsIO t)           = TApp (TGlobal IO_type) (translateType t)
translateType (Hs.HsFun a b)        = TFun (translateType a) (translateType b)

{-------------------------------------------------------------------------------
  Sigma/Phi/Tau types
-------------------------------------------------------------------------------}

translateSigma :: Hs.SigmaType -> ClosedType
translateSigma (Hs.ForallTy size hints (Hs.QuantTy cts ty)) = TForall
    hints
    (rzeroAdd size)
    (map translateClassTy cts)
    (translateTau ty)

translateClassTy :: Hs.ClassTy ctx -> SType ctx
translateClassTy (Hs.ClassTy cls args) =
    foldl' TApp (TGlobal $ classGlobal cls) (fmap translateTau args)

classGlobal :: C.ClassTyCon n -> Global
classGlobal = \case
  C.EqTyCon         -> Eq_class
  C.OrdTyCon        -> Ord_class
  C.NumTyCon        -> Num_class
  C.IntegralTyCon   -> Integral_class
  C.FractionalTyCon -> Fractional_class
  C.BitsTyCon       -> Bits_class
  C.DivTyCon        -> Div_class

translateTau :: Hs.TauType ctx -> SType ctx
translateTau (Hs.FunTy a b) = TFun (translateTau a) (translateTau b)
translateTau (Hs.TyVarTy x) = TBound x
translateTau (Hs.TyConAppTy (Hs.TyConApp dc args)) =
    foldl' TApp (tyConGlobal dc) (fmap translateTau args)

tyConGlobal :: C.DataTyCon n -> SType be
tyConGlobal = \case
  C.BoolTyCon             -> mkPrimTy HsPrimCBool
  C.StringTyCon           -> TApp (TGlobal Foreign_Ptr) (mkPrimTy HsPrimCChar)
  C.IntLikeTyCon   inty   -> mkPrimTy $ integralType inty
  C.FloatLikeTyCon floaty -> mkPrimTy $ floatingType floaty
  C.PrimTyTyCon           -> error "tyConGlobal PrimTyTyCon"
  C.EmptyTyCon            -> error "tyConGlobal EmptyTyCon"
  where
    mkPrimTy = TGlobal . PrimType

mfunGlobal :: C.MFun arity -> Global
mfunGlobal = \case
  C.MUnaryPlus  -> Base_identity -- TODO: want some function like `numId :: forall a. Num a => a -> a; numId x = x`
  C.MUnaryMinus -> Num_negate
  C.MLogicalNot -> Base_not
  C.MBitwiseNot -> Bits_complement
  C.MMult       -> Num_times
  C.MDiv        -> Div_div
  C.MRem        -> Integral_rem
  C.MAdd        -> Num_add
  C.MSub        -> Num_minus
  C.MShiftLeft  -> Bits_shiftL
  C.MShiftRight -> Bits_shiftR
  C.MRelLT      -> Ord_lt
  C.MRelLE      -> Ord_le
  C.MRelGT      -> Ord_gt
  C.MRelGE      -> Ord_ge
  C.MRelEQ      -> Eq_eq
  C.MRelNE      -> Eq_uneq
  C.MBitwiseAnd -> Bits_and
  C.MBitwiseXor -> Bits_xor
  C.MBitwiseOr  -> Bits_or
  C.MLogicalAnd -> Base_and
  C.MLogicalOr  -> Base_or

{-------------------------------------------------------------------------------
 VarDeclRHS
-------------------------------------------------------------------------------}

translateBody :: Hs.VarDeclRHS ctx -> SExpr ctx
translateBody (Hs.VarDeclVar x)                     = EBound x
translateBody (Hs.VarDeclFloat f)                   = EFloat f
translateBody (Hs.VarDeclDouble d)                  = EDouble d
translateBody (Hs.VarDeclIntegral i ty)             = EIntegral i (Just ty)
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

translateStorableInstance :: Hs.Struct n -> Hs.StorableInstance -> Instance
translateStorableInstance struct Hs.StorableInstance{..} = do
    let peek = lambda (idiom structCon translatePeekByteOff) storablePeek
    let poke = lambda (lambda (translateElimStruct (doAll translatePokeByteOff))) storablePoke
    Instance
      { instanceClass = Storable_Storable
      , instanceType  = Hs.structName struct
      , instanceDecs  = [
            (Storable_sizeOf    , EUnusedLam $ EInt storableSizeOf)
          , (Storable_alignment , EUnusedLam $ EInt storableAlignment)
          , (Storable_peek      , peek)
          , (Storable_poke      , poke)
          ]
      }

translatePeekByteOff :: Hs.PeekByteOff ctx -> SExpr ctx
translatePeekByteOff (Hs.PeekByteOff ptr i) = appMany Storable_peekByteOff [EBound ptr, EInt i]

translatePokeByteOff :: Hs.PokeByteOff ctx -> SExpr ctx
translatePokeByteOff (Hs.PokeByteOff ptr i x) = appMany Storable_pokeByteOff [EBound ptr, EInt i, EBound x]

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
doAll _ (Hs.Seq []) = EGlobal Monad_return `EApp` EGlobal Unit_constructor
doAll f (Hs.Seq ss) = foldr1 (EInfix Monad_seq) (map f ss)
