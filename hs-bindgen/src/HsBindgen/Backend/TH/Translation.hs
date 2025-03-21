{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.TH.Translation (
    mkDecl,
) where

import Data.Bits qualified
import Data.Ix qualified
import Data.Text qualified as Text
import Data.Void qualified
import Foreign.C.Types qualified
import Foreign.C.String qualified
import Foreign.Ptr qualified
import Foreign.Storable qualified
import Language.Haskell.TH (Quote)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import GHC.Ptr ( Ptr(Ptr) )
import GHC.Exts qualified as IsList(IsList(..))

import GHC.Float
  ( castWord64ToDouble, castDoubleToWord64
  , castWord32ToFloat , castFloatToWord32 )

import C.Char (CharValue(..), charValueFromAddr)
import C.Expr.HostPlatform qualified as C
import HsBindgen.C.AST.Literal (canBeRepresentedAsRational)
import HsBindgen.ExtBindings
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.Runtime.Bitfield qualified
import HsBindgen.Runtime.ByteArray qualified
import HsBindgen.Runtime.ConstantArray qualified
import HsBindgen.Runtime.FlexibleArrayMember qualified
import HsBindgen.Runtime.Syntax qualified
import HsBindgen.Runtime.SizedByteArray qualified
import HsBindgen.SHs.AST

import DeBruijn
import GHC.Exts (Int(..), sizeofByteArray#)

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

mkGlobal :: Global -> TH.Name
mkGlobal = \case
      Tuple_type i         -> tupleTypeName $ fromIntegral i
      Tuple_constructor i  -> TH.tupleDataName $ fromIntegral i
      Applicative_pure     -> 'pure
      Applicative_seq      -> '(<*>)
      Monad_return         -> 'return
      Monad_seq            -> '(>>)
      Storable_class       -> ''Foreign.Storable.Storable
      Storable_sizeOf      -> 'Foreign.Storable.sizeOf
      Storable_alignment   -> 'Foreign.Storable.alignment
      Storable_peekByteOff -> 'Foreign.Storable.peekByteOff
      Storable_pokeByteOff -> 'Foreign.Storable.pokeByteOff
      Storable_peek        -> 'Foreign.Storable.peek
      Storable_poke        -> 'Foreign.Storable.poke
      Foreign_Ptr          -> ''Foreign.Ptr.Ptr
      Ptr_constructor      -> 'GHC.Ptr.Ptr
      Foreign_FunPtr       -> ''Foreign.Ptr.FunPtr
      ConstantArray        -> ''HsBindgen.Runtime.ConstantArray.ConstantArray
      IO_type              -> ''IO
      HasFlexibleArrayMember_class -> ''HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember
      HasFlexibleArrayMember_offset -> 'HsBindgen.Runtime.FlexibleArrayMember.flexibleArrayMemberOffset
      Bitfield_peekBitOffWidth -> 'HsBindgen.Runtime.Bitfield.peekBitOffWidth
      Bitfield_pokeBitOffWidth -> 'HsBindgen.Runtime.Bitfield.pokeBitOffWidth
      CharValue_tycon        -> ''C.Char.CharValue
      CharValue_constructor  -> 'C.Char.CharValue
      CharValue_fromAddr    -> 'C.Char.charValueFromAddr

      Bits_class       -> ''Data.Bits.Bits
      Bounded_class    -> ''Bounded
      Enum_class       -> ''Enum
      Eq_class         -> ''Eq
      FiniteBits_class -> ''Data.Bits.FiniteBits
      Floating_class   -> ''Floating
      Fractional_class -> ''Fractional
      Integral_class   -> ''Integral
      Ix_class         -> ''Data.Ix.Ix
      Num_class        -> ''Num
      Ord_class        -> ''Ord
      Read_class       -> ''Read
      Real_class       -> ''Real
      RealFloat_class  -> ''RealFloat
      RealFrac_class   -> ''RealFrac
      Show_class       -> ''Show

      NomEq_class -> ''(~)

      Not_class             -> ''C.Not
      Not_not               ->  'C.not
      Logical_class         -> ''C.Logical
      Logical_and           -> '(C.&&)
      Logical_or            -> '(C.||)
      RelEq_class           -> ''C.RelEq
      RelEq_eq              -> '(C.==)
      RelEq_uneq            -> '(C.!=)
      RelOrd_class          -> ''C.RelOrd
      RelOrd_lt             -> '(C.<)
      RelOrd_le             -> '(C.<=)
      RelOrd_gt             -> '(C.>)
      RelOrd_ge             -> '(C.>=)
      Plus_class            -> ''C.Plus
      Plus_resTyCon         -> ''C.PlusRes
      Plus_plus             ->  'C.plus
      Minus_class           -> ''C.Minus
      Minus_resTyCon        -> ''C.MinusRes
      Minus_negate          ->  'C.negate
      Add_class             -> ''C.Add
      Add_resTyCon          -> ''C.AddRes
      Add_add               -> '(C.+)
      Sub_class             -> ''C.Sub
      Sub_resTyCon          -> ''C.SubRes
      Sub_minus             -> '(C.-)
      Mult_class            -> ''C.Mult
      Mult_resTyCon         -> ''C.MultRes
      Mult_mult             -> '(C.*)
      Div_class             -> ''C.Div
      Div_resTyCon          -> ''C.DivRes
      Div_div               -> '(C./)
      Rem_class             -> ''C.Rem
      Rem_resTyCon          -> ''C.RemRes
      Rem_rem               -> '(C.%)
      Complement_class      -> ''C.Complement
      Complement_resTyCon   -> ''C.ComplementRes
      Complement_complement -> '(C..~)
      Bitwise_class         -> ''C.Bitwise
      Bitwise_resTyCon      -> ''C.BitsRes
      Bitwise_and           -> '(C..&.)
      Bitwise_or            -> '(C..|.)
      Bitwise_xor           -> '(C..^.)
      Shift_class           -> ''C.Shift
      Shift_resTyCon        -> ''C.ShiftRes
      Shift_shiftL          -> '(C.<<)
      Shift_shiftR          -> '(C.>>)

      IntLike_tycon        -> ''HsBindgen.Runtime.Syntax.IntLike
      FloatLike_tycon      -> ''HsBindgen.Runtime.Syntax.FloatLike
      GHC_Float_castWord32ToFloat  -> 'GHC.Float.castWord32ToFloat
      GHC_Float_castWord64ToDouble -> 'GHC.Float.castWord64ToDouble
      CFloat_constructor  -> 'Foreign.C.Types.CFloat
      CDouble_constructor -> 'Foreign.C.Types.CDouble

      ByteArray_type       -> ''ByteArray
      SizedByteArray_type  -> ''HsBindgen.Runtime.SizedByteArray.SizedByteArray

      ByteArray_getUnionPayload -> 'HsBindgen.Runtime.ByteArray.getUnionPayload
      ByteArray_setUnionPayload -> 'HsBindgen.Runtime.ByteArray.setUnionPayload

      PrimType t           -> mkGlobalP t

-- | A version of 'TH.tupleTypeName' that always uses the @(,,)@ syntax rather
-- than @Tuple3@. This ensures consistency in TH tests across GHC versions.
tupleTypeName :: Int -> TH.Name
tupleTypeName n =
  TH.Name
    (TH.mkOccName tup_occ)
    (TH.NameG ns (TH.mkPkgName "ghc-internal") (TH.mkModName "GHC.Tuple"))
      -- package name doesn't matter, as this is built-in syntax
  where
    ns = TH.TcClsName
    tup_occ
      | n == 0 = "Unit"
      | n == 1 = "Solo"
      | otherwise = "(" ++ replicate (n - 1) ',' ++ ")"

mkGlobalP :: HsPrimType -> TH.Name
mkGlobalP HsPrimVoid       = ''Data.Void.Void
mkGlobalP HsPrimUnit       = ''()
mkGlobalP HsPrimCChar      = ''Foreign.C.Types.CChar
mkGlobalP HsPrimCUChar     = ''Foreign.C.Types.CUChar
mkGlobalP HsPrimCSChar     = ''Foreign.C.Types.CSChar
mkGlobalP HsPrimCInt       = ''Foreign.C.Types.CInt
mkGlobalP HsPrimCUInt      = ''Foreign.C.Types.CUInt
mkGlobalP HsPrimCShort     = ''Foreign.C.Types.CShort
mkGlobalP HsPrimCUShort    = ''Foreign.C.Types.CUShort
mkGlobalP HsPrimCLong      = ''Foreign.C.Types.CLong
mkGlobalP HsPrimCULong     = ''Foreign.C.Types.CULong
mkGlobalP HsPrimCLLong     = ''Foreign.C.Types.CLLong
mkGlobalP HsPrimCULLong    = ''Foreign.C.Types.CULLong
mkGlobalP HsPrimCFloat     = ''Foreign.C.Types.CFloat
mkGlobalP HsPrimCDouble    = ''Foreign.C.Types.CDouble
mkGlobalP HsPrimCBool      = ''Foreign.C.Types.CBool
mkGlobalP HsPrimCPtrDiff   = ''Foreign.C.Types.CPtrdiff
mkGlobalP HsPrimCStringLen = ''Foreign.C.String.CStringLen
mkGlobalP HsPrimInt        = ''Int

mkExpr :: Quote q => Env ctx TH.Name -> SExpr ctx -> q TH.Exp
mkExpr env = \case
      EGlobal n     -> TH.varE (mkGlobal n)
      EFree n       -> hsVarE n
      EBound x      -> TH.varE (lookupEnv x env)
      ECon n        -> hsConE n
      EIntegral i Nothing -> TH.litE (TH.IntegerL i)
      EIntegral i (Just t)-> TH.sigE (TH.litE (TH.IntegerL i)) (mkPrimType t)
      -- TH doesn't have floating-point literals, because it represents them
      -- using the Rational type, which is incorrect. (See GHC ticket #13124.)
      --
      -- To work around this problem, we cast floating-point numbers to
      -- Word32/Word64 and then cast back.
      EFloat f t ->
        TH.sigE
          ( if canBeRepresentedAsRational f
              then [| f |]
              else [| Foreign.C.Types.CFloat $ castWord32ToFloat  $( TH.lift $ castFloatToWord32  f ) |]
          )
          (mkPrimType t)
      EDouble d t ->
        TH.sigE
          ( if canBeRepresentedAsRational d
              then [| d |]
              else [| Foreign.C.Types.CDouble $ castWord64ToDouble $( TH.lift $ castDoubleToWord64 d ) |]
          )
          (mkPrimType t)
      EChar c -> [| c |]
      EString ba@(ByteArray ba#) ->
        let
          len :: Integer
          len = fromIntegral (I# (sizeofByteArray# ba#))
        in
          TH.sigE
            ( TH.tupE [ TH.conE 'GHC.Ptr.Ptr `TH.appE` TH.litE (TH.StringPrimL $ IsList.toList ba)
                      , TH.litE (TH.integerL len)
                      ]
            )
          (mkPrimType HsPrimCStringLen)
      EApp f x      -> TH.appE (mkExpr env f) (mkExpr env x)
      EInfix op x y -> TH.infixE
                         (Just $ mkExpr env x)
                         (TH.varE $ mkGlobal op)
                         (Just $ mkExpr env y)
      ELam (NameHint x) f      -> do
          x' <- TH.newName x
          TH.lamE [TH.varP x'] (mkExpr (env :> x') f)
      EUnusedLam f ->
          TH.lamE [TH.wildP] (mkExpr env f)
      ECase x alts  -> TH.caseE (mkExpr env x)
                         [ do
                              (xs, env') <- newNames env add hints
                              TH.match
                                 (hsConP c $ map TH.varP xs)
                                 (TH.normalB $ mkExpr env' b)
                                 []
                         | SAlt c add hints b <- alts
                         ]

mkPat :: Quote q => PatExpr -> q TH.Pat
mkPat = \case
    PEApps n xs -> hsConP n (map mkPat xs)
    PELit i -> TH.litP (TH.IntegerL i)

mkType :: Quote q => Env ctx TH.Name -> SType ctx -> q TH.Type
mkType env = \case
    TGlobal n -> TH.conT (mkGlobal n)
    TBound x  -> TH.varT (lookupEnv x env)
    TCon n    -> hsConT n
    TLit n    -> TH.litT (TH.numTyLit (toInteger n))
    TFun a b  -> TH.arrowT `TH.appT` mkType env a `TH.appT` mkType env b
    TApp f t  -> TH.appT (mkType env f) (mkType env t)
    TForall hints add ctxt body -> do
        let bndr tv = TH.PlainTV tv TH.SpecifiedSpec
        (xs, env') <- newNames env add hints
        TH.forallT
            (map bndr xs)
            (traverse (mkType env') ctxt)
            (mkType env' body)
    TExt ExtIdentifier{..} ->
        TH.conT . TH.mkName $ concat [
              Text.unpack (getHsModuleName extIdentifierModule)
            , "."
            , Text.unpack (getHsIdentifier extIdentifierIdentifier)
            ]

mkPrimType :: Quote q => HsPrimType -> q TH.Type
mkPrimType = TH.conT . mkGlobalP

mkDecl :: forall q. Quote q => SDecl -> q [TH.Dec]
mkDecl = \case
      DVar x Nothing   f -> singleton <$> simpleDecl (hsNameToTH x) f
      DVar x (Just ty) f -> sequence
          [ TH.sigD (hsNameToTH x) (mkType EmptyEnv ty)
          , simpleDecl (hsNameToTH x) f
          ]

      DInst i  -> singleton <$> TH.instanceD
                    (return [])
                    (appsT (TH.conT $ mkGlobal $ instanceClass i)
                        (map (mkType EmptyEnv) $ instanceArgs i))
                    ( map (\(x, f) -> simpleDecl (mkGlobal x) f) $
                        instanceDecs i
                    )
      DRecord d ->
        let fields :: [q TH.VarBangType]
            fields =
              [ TH.varBangType (hsNameToTH (fieldName f)) $
                  TH.bangType
                    (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness)
                    (mkType EmptyEnv (fieldType f))
              | f <- dataFields d
              ]
        in singleton <$>
          TH.dataD (TH.cxt []) (hsNameToTH $ dataType d) [] Nothing [TH.recC (hsNameToTH (dataCon d)) fields] []

      DEmptyData d -> singleton <$>
          TH.dataD (TH.cxt []) (hsNameToTH (emptyDataName d)) [] Nothing [] []

      DNewtype n ->
        let field :: q TH.VarBangType
            field = TH.varBangType (hsNameToTH (fieldName (newtypeField n))) $
              TH.bangType
                (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness)
                (mkType EmptyEnv (fieldType (newtypeField n)))
        in singleton <$> TH.newtypeD (TH.cxt []) (hsNameToTH $ newtypeName n) [] Nothing (TH.recC (hsNameToTH (newtypeCon n)) [field]) []

      DDerivingInstance s ty -> do
          s' <- strategy s
          singleton <$> TH.standaloneDerivWithStrategyD (Just s') (TH.cxt []) (mkType EmptyEnv ty)

      DForeignImport ForeignImport {..} ->
           singleton . TH.ForeignD . TH.ImportF TH.CApi TH.Safe
              (foreignImportHeader ++ " " ++ Text.unpack foreignImportOrigName) -- TODO: header
              (hsNameToTH foreignImportName)
              <$>
              (mkType EmptyEnv foreignImportType)

      DPatternSynonym ps -> sequence
          [ TH.patSynSigD
            (hsNameToTH (patSynName ps))
            (mkType EmptyEnv (patSynType ps))
          , TH.patSynD
            (hsNameToTH (patSynName ps))
            (TH.prefixPatSyn [])
            TH.implBidir
            (mkPat (patSynRHS ps))
          ]
    where
      simpleDecl :: TH.Name -> SExpr EmptyCtx -> q TH.Dec
      simpleDecl x f = TH.valD (TH.varP x) (TH.normalB $ mkExpr EmptyEnv f) []

strategy :: Quote q => Hs.Strategy ClosedType -> q TH.DerivStrategy
strategy Hs.DeriveNewtype  = return TH.NewtypeStrategy
strategy Hs.DeriveStock    = return TH.StockStrategy
strategy (Hs.DeriveVia ty) = TH.ViaStrategy <$> mkType EmptyEnv ty

{-------------------------------------------------------------------------------
  Monad functionality
-------------------------------------------------------------------------------}

appsT :: Quote q => q TH.Type -> [q TH.Type] -> q TH.Type
appsT = foldl' TH.appT

hsConE :: Quote m => HsName NsConstr -> m TH.Exp
hsConE = TH.conE . hsNameToTH

hsConP :: Quote m => HsName NsConstr -> [m TH.Pat] -> m TH.Pat
hsConP = TH.conP . hsNameToTH

hsConT :: Quote m => HsName NsTypeConstr -> m TH.Type
hsConT = TH.conT . hsNameToTH

hsVarE :: Quote m => HsName NsVar -> m TH.Exp
hsVarE = TH.varE . hsNameToTH

hsNameToTH :: HsName ns -> TH.Name
hsNameToTH = TH.mkName . Text.unpack  . getHsName

newNames :: Quote q => Env ctx TH.Name -> Add n ctx ctx' -> Vec n NameHint -> q ([TH.Name], Env ctx' TH.Name)
newNames env AZ _ = return ([], env)
newNames env (AS n) (NameHint hint ::: hints) = do
    (xs, env') <- newNames env n hints
    x <- TH.newName hint
    return (x : xs, env' :> x)
