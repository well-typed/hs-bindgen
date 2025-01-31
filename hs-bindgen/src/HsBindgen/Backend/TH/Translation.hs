{-# LANGUAGE TemplateHaskellQuotes #-}
module HsBindgen.Backend.TH.Translation (
    mkDecl,
) where

import Data.Bits qualified
import Data.Ix qualified
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

import C.Operator.Classes qualified as C
import HsBindgen.C.AST.Literal (canBeRepresentedAsRational)
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.Runtime.ConstantArray qualified
import HsBindgen.Runtime.FlexibleArrayMember qualified
import HsBindgen.Runtime.Syntax qualified
import HsBindgen.SHs.AST

import DeBruijn

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

mkGlobal :: Global -> TH.Name
mkGlobal =  \case
      Unit_type            -> ''()
      Unit_constructor     -> '()
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
      Foreign_FunPtr       -> ''Foreign.Ptr.FunPtr
      ConstantArray        -> ''HsBindgen.Runtime.ConstantArray.ConstantArray
      IO_type              -> ''IO
      HasFlexibleArrayMember_class -> ''HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember
      HasFlexibleArrayMember_offset -> 'HsBindgen.Runtime.FlexibleArrayMember.flexibleArrayMemberOffset

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

      PrimType t           -> mkGlobalP t
    where
      mkGlobalP HsPrimVoid     = ''Data.Void.Void
      mkGlobalP HsPrimUnit     = ''()
      mkGlobalP HsPrimCChar    = ''Foreign.C.Types.CChar
      mkGlobalP HsPrimCUChar   = ''Foreign.C.Types.CUChar
      mkGlobalP HsPrimCSChar   = ''Foreign.C.Types.CSChar
      mkGlobalP HsPrimCInt     = ''Foreign.C.Types.CInt
      mkGlobalP HsPrimCUInt    = ''Foreign.C.Types.CUInt
      mkGlobalP HsPrimCShort   = ''Foreign.C.Types.CShort
      mkGlobalP HsPrimCUShort  = ''Foreign.C.Types.CUShort
      mkGlobalP HsPrimCLong    = ''Foreign.C.Types.CLong
      mkGlobalP HsPrimCULong   = ''Foreign.C.Types.CULong
      mkGlobalP HsPrimCLLong   = ''Foreign.C.Types.CLLong
      mkGlobalP HsPrimCULLong  = ''Foreign.C.Types.CULLong
      mkGlobalP HsPrimCFloat   = ''Foreign.C.Types.CFloat
      mkGlobalP HsPrimCDouble  = ''Foreign.C.Types.CDouble
      mkGlobalP HsPrimCBool    = ''Foreign.C.Types.CBool
      mkGlobalP HsPrimCPtrDiff = ''Foreign.C.Types.CPtrdiff

{-
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
-}

mkExpr :: Quote q => Env ctx TH.Name -> SExpr ctx -> q TH.Exp
mkExpr env = \case
      EGlobal n     -> TH.varE (mkGlobal n)
      EFree n       -> hsVarE n
      EBound x      -> TH.varE (lookupEnv x env)
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

mkPat :: Quote q => SExpr EmptyCtx -> q TH.Pat
mkPat = \case
    EGlobal {}    -> error "unexpected"
    EFree {}      -> error "cannot happen"
    EBound {}     -> error "cannot happen"
    EFloat {}     -> error "cannot happen"
    EDouble {}    -> error "cannot happen"
    EInfix {}     -> error "cannot happen"
    ELam {}       -> error "cannot happen"
    EUnusedLam {} -> error "cannot happen"
    ECase {}      -> error "cannot happen"
    EApp f t      -> liftA2 appP (mkPat f) (mkPat t)
    ECon n        -> hsConP n []
    EIntegral i _ -> TH.litP (TH.IntegerL i)
  where
    appP :: TH.Pat -> TH.Pat -> TH.Pat
    appP (TH.ConP n ts xs) p = TH.ConP n ts (xs ++ [p])
    appP _                 _ = error "cannot happen"

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

      DEmptyData n -> singleton <$>
          TH.dataD (TH.cxt []) (hsNameToTH n) [] Nothing [] []

      DNewtype n ->
        let field :: q TH.VarBangType
            field = TH.varBangType (hsNameToTH (fieldName (newtypeField n))) $
              TH.bangType
                (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness)
                (mkType EmptyEnv (fieldType (newtypeField n)))
        in singleton <$> TH.newtypeD (TH.cxt []) (hsNameToTH $ newtypeName n) [] Nothing (TH.recC (hsNameToTH (newtypeCon n)) [field]) []

      DDerivingInstance s ty ->
          singleton <$> TH.standaloneDerivWithStrategyD (Just $ strategy s) (TH.cxt []) (mkType EmptyEnv ty)

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

strategy :: Hs.Strategy -> TH.DerivStrategy
strategy Hs.DeriveNewtype = TH.NewtypeStrategy
strategy Hs.DeriveStock   = TH.StockStrategy

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
