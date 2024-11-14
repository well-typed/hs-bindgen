{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.TH (
    BE(..)
  , M
  , runM
  ) where

import Data.Bits qualified
import Data.Text qualified as Text
import Data.Void qualified
import Foreign.C.Types qualified
import Foreign.Ptr qualified
import Foreign.Storable qualified
import Language.Haskell.TH (Quote)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import GHC.Float
  ( castWord64ToDouble, castDoubleToWord64
  , castWord32ToFloat , castFloatToWord32 )

import HsBindgen.Backend.Common
import HsBindgen.Imports
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Util.PHOAS
import HsBindgen.ConstantArray qualified
import HsBindgen.C.AST.Literal (canBeRepresentedAsRational)

import HsBindgen.Patterns qualified (Div(..))

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

type BE :: (Star -> Star) -> Star
data BE q = BE

instance TH.Quote q => BackendRep (BE q) where
  type Name (BE q) = TH.Name
  type Expr (BE q) = q TH.Exp
  type Decl (BE q) = q [TH.Dec]
  type Ty   (BE q) = q TH.Type

  resolve _ =  \case
      Unit_type            -> ''()
      Unit_constructor     -> '()
      Applicative_pure     -> 'pure
      Applicative_seq      -> '(<*>)
      Monad_return         -> 'return
      Monad_seq            -> '(>>)
      Storable_Storable    -> ''Foreign.Storable.Storable
      Storable_sizeOf      -> 'Foreign.Storable.sizeOf
      Storable_alignment   -> 'Foreign.Storable.alignment
      Storable_peekByteOff -> 'Foreign.Storable.peekByteOff
      Storable_pokeByteOff -> 'Foreign.Storable.pokeByteOff
      Storable_peek        -> 'Foreign.Storable.peek
      Storable_poke        -> 'Foreign.Storable.poke
      Foreign_Ptr          -> ''Foreign.Ptr.Ptr
      ConstantArray        -> ''HsBindgen.ConstantArray.ConstantArray

      Eq_class             -> ''Eq
      Ord_class            -> ''Ord
      Num_class            -> ''Num
      Integral_class       -> ''Integral
      Fractional_class     -> ''Fractional
      Div_class            -> ''HsBindgen.Patterns.Div
      Bits_class           -> ''Data.Bits.Bits

      Eq_eq                -> '(==)
      Eq_uneq              -> '(/=)
      Ord_lt               -> '(<)
      Ord_le               -> '(<=)
      Ord_gt               -> '(>)
      Ord_ge               -> '(>=)
      Base_identity        -> 'id
      Base_not             -> 'not
      Base_and             -> '(&&)
      Base_or              -> '(||)
      Bits_shiftL          -> 'Data.Bits.shiftL
      Bits_shiftR          -> 'Data.Bits.shiftR
      Bits_and             -> '(Data.Bits..&.)
      Bits_xor             -> 'Data.Bits.xor
      Bits_or              -> '(Data.Bits..|.)
      Bits_complement      -> 'Data.Bits.complement
      Num_negate           -> 'negate
      Num_add              -> '(+)
      Num_minus            -> '(-)
      Num_times            -> '(*)
      Div_div              -> '(HsBindgen.Patterns./)
      Integral_rem         -> 'rem
      GHC_Float_castWord32ToFloat  -> 'GHC.Float.castWord32ToFloat
      GHC_Float_castWord64ToDouble -> 'GHC.Float.castWord64ToDouble
      CFloat_constructor  -> 'Foreign.C.Types.CFloat
      CDouble_constructor -> 'Foreign.C.Types.CDouble

      PrimType t           -> resolveP t
    where
      resolveP HsPrimVoid    = ''Data.Void.Void
      resolveP HsPrimCChar   = ''Foreign.C.Types.CChar
      resolveP HsPrimCUChar  = ''Foreign.C.Types.CUChar
      resolveP HsPrimCSChar  = ''Foreign.C.Types.CSChar
      resolveP HsPrimCInt    = ''Foreign.C.Types.CInt
      resolveP HsPrimCUInt   = ''Foreign.C.Types.CUInt
      resolveP HsPrimCShort  = ''Foreign.C.Types.CShort
      resolveP HsPrimCUShort = ''Foreign.C.Types.CUShort
      resolveP HsPrimCLong   = ''Foreign.C.Types.CLong
      resolveP HsPrimCULong  = ''Foreign.C.Types.CULong
      resolveP HsPrimCLLong  = ''Foreign.C.Types.CLLong
      resolveP HsPrimCULLong = ''Foreign.C.Types.CULLong
      resolveP HsPrimCFloat  = ''Foreign.C.Types.CFloat
      resolveP HsPrimCDouble = ''Foreign.C.Types.CDouble
      resolveP HsPrimCBool   = ''Foreign.C.Types.CBool

  mkExpr be = \case
      EGlobal n     -> TH.varE (resolve be n)
      EVar x        -> TH.varE (getFresh x)
      EFreeVar x    -> TH.varE $ TH.mkName (Text.unpack $ getHsName x)
      ECon n        -> hsConE n
      EIntegral i _ -> TH.litE (TH.IntegerL i)
      -- TH doesn't have floating-point literals, because it represents them
      -- using the Rational type, which is incorrect. (See GHC ticket #13124.)
      --
      -- To work around this problem, we cast floating-point numbers to
      -- Word32/Word64 and then cast back.
      EFloat f
        | canBeRepresentedAsRational f
        -> [| f |]
        | otherwise
        -> [| Foreign.C.Types.CFloat $ castWord32ToFloat  $( TH.lift $ castFloatToWord32  f ) |]
      EDouble d
        | canBeRepresentedAsRational d
        -> [| d |]
        | otherwise
        -> [| Foreign.C.Types.CDouble $ castWord64ToDouble $( TH.lift $ castDoubleToWord64 d ) |]
      EApp f x      -> TH.appE (mkExpr be f) (mkExpr be x)
      EInfix op x y -> TH.infixE
                         (Just $ mkExpr be x)
                         (TH.varE $ resolve be op)
                         (Just $ mkExpr be y)
      ELam x f      -> TH.lamE
                         [maybe TH.wildP (TH.varP . getFresh) x]
                         (mkExpr be f)
      ECase x ms    -> TH.caseE (mkExpr be x) [
                           TH.match
                             (hsConP c $ map (TH.varP . getFresh) xs)
                             (TH.normalB $ mkExpr be b)
                             []
                         | (c, xs, b) <- ms
                         ]
      EInj x        -> x

  mkType :: BE q -> SType (BE q) -> Ty (BE q)
  mkType be = \case
      TGlobal n  -> TH.conT (resolve be n)
      TCon n     -> hsConT n
      TLit n     -> TH.litT (TH.numTyLit (toInteger n))
      TApp f t   -> TH.appT (mkType be f) (mkType be t)
      TFunTy a b -> TH.appT (TH.appT TH.arrowT (mkType be a)) (mkType be b)
      TForall qtvs ctxt body ->
        let bndr tv = TH.PlainTV tv TH.SpecifiedSpec
        in  TH.forallT
              (map bndr qtvs)
              (traverse (mkType be) ctxt)
              (mkType be body)
      TTyVar tv     -> TH.varT $ getFresh tv

  mkDecl :: BE q -> SDecl (BE q) -> Decl (BE q)
  mkDecl be = \case
      DVar hsNm mbTy f -> do
        let nm = TH.mkName $ Text.unpack $ getHsName hsNm
        addSigs <-
          case mbTy of
            Just ty -> (:) <$> TH.sigD nm (mkType be ty)
            Nothing -> return id
        addSigs . (:[]) <$> simpleDecl nm f
      DInst i  -> (:[]) <$> TH.instanceD
                    (return [])
                    [t| $(TH.conT $ resolve be $ instanceClass i)
                        $(hsConT  $ instanceType i)
                    |]
                    ( map (\(x, f) -> simpleDecl (resolve be x) f) $
                        instanceDecs i
                    )
      DRecord d ->
        let fields :: [q TH.VarBangType]
            fields =
              [ TH.varBangType (hsNameToTH n) $ TH.bangType (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness) (mkType be t)
              | (n, t) <- dataFields d
              ]
        in (:[]) <$>
          TH.dataD (TH.cxt []) (hsNameToTH $ dataType d) [] Nothing [TH.recC (hsNameToTH (dataCon d)) fields] []

      DNewtype n ->
        let field :: q TH.VarBangType
            field = TH.varBangType (hsNameToTH (newtypeField n)) $ TH.bangType (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness) (mkType be (newtypeType n))
        in (:[]) <$> TH.newtypeD (TH.cxt []) (hsNameToTH $ newtypeName n) [] Nothing (TH.recC (hsNameToTH (newtypeCon n)) [field]) []

      DDerivingNewtypeInstance ty ->
          (:[]) <$> TH.standaloneDerivWithStrategyD (Just TH.NewtypeStrategy) (TH.cxt []) (mkType be ty)
    where
      simpleDecl :: TH.Name -> SExpr (BE q) -> q TH.Dec
      simpleDecl x f = TH.valD (TH.varP x) (TH.normalB $ mkExpr be f) []

instance TH.Quote q => Backend (BE q) where
  newtype M (BE q) a = Gen { unwrapGen :: q a }
    deriving newtype (
        Functor
      , Applicative
      , Monad
      , TH.Quote
      )

  fresh ::
       BE q
    -> HsName NsVar
    -> (Fresh (BE q) Bound -> M (BE q) a)
    -> M (BE q) a
  fresh _ = \x k -> TH.newName (Text.unpack (getHsName x)) >>= k . Fresh

{-------------------------------------------------------------------------------
  Monad functionality
-------------------------------------------------------------------------------}

runM :: M (BE q) a -> q a
runM = unwrapGen

{-------------------------------------------------------------------------------
  Dealing with names
-------------------------------------------------------------------------------}

hsConE :: Quote m => HsName NsConstr -> m TH.Exp
hsConE = TH.conE . hsNameToTH

hsConP :: Quote m => HsName NsConstr -> [m TH.Pat] -> m TH.Pat
hsConP = TH.conP . hsNameToTH

hsConT :: Quote m => HsName NsTypeConstr -> m TH.Type
hsConT = TH.conT . hsNameToTH

hsNameToTH :: HsName ns -> TH.Name
hsNameToTH = TH.mkName . Text.unpack  . getHsName
