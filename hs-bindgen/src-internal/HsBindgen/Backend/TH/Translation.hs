{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.TH.Translation (
    mkDecl,
) where

import Control.Exception (Exception (displayException))
import Control.Monad (liftM2)
import Data.Bits qualified
import Data.Complex qualified
import Data.Ix qualified
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Void qualified
import Foreign qualified
import Foreign.C.String qualified
import Foreign.C.Types qualified
import Foreign.Ptr qualified
import Foreign.Storable qualified
import GHC.Exts (Int (..), sizeofByteArray#)
import GHC.Exts qualified as IsList (IsList (..))
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat,
                  castWord64ToDouble)
import GHC.Ptr (Ptr (Ptr))
import Language.Haskell.TH (Quote)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import System.IO.Unsafe qualified
import Text.Read qualified

import C.Char (CharValue (..), charValueFromAddr)
import C.Expr.HostPlatform qualified as C

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation (Comment)
import HsBindgen.Backend.SHs.AST
import HsBindgen.Errors
import HsBindgen.Guasi
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell
import HsBindgen.NameHint
import HsBindgen.Runtime.Bitfield qualified
import HsBindgen.Runtime.Block qualified
import HsBindgen.Runtime.ByteArray qualified
import HsBindgen.Runtime.CAPI qualified
import HsBindgen.Runtime.CEnum qualified
import HsBindgen.Runtime.ConstantArray qualified
import HsBindgen.Runtime.FlexibleArrayMember qualified
import HsBindgen.Runtime.IncompleteArray qualified
import HsBindgen.Runtime.Marshal qualified
import HsBindgen.Runtime.SizedByteArray qualified

import DeBruijn (Add (..), EmptyCtx, Env (..), lookupEnv)

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
      StaticSize_class     -> ''HsBindgen.Runtime.Marshal.StaticSize
      ReadRaw_class        -> ''HsBindgen.Runtime.Marshal.ReadRaw
      WriteRaw_class       -> ''HsBindgen.Runtime.Marshal.WriteRaw
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
      IncompleteArray      -> ''HsBindgen.Runtime.IncompleteArray.IncompleteArray
      IO_type              -> ''IO
      HasFlexibleArrayMember_class -> ''HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember
      HasFlexibleArrayMember_offset -> 'HsBindgen.Runtime.FlexibleArrayMember.flexibleArrayMemberOffset
      Bitfield_peekBitOffWidth -> 'HsBindgen.Runtime.Bitfield.peekBitOffWidth
      Bitfield_pokeBitOffWidth -> 'HsBindgen.Runtime.Bitfield.pokeBitOffWidth
      CharValue_tycon        -> ''C.Char.CharValue
      CharValue_constructor  -> 'C.Char.CharValue
      CharValue_fromAddr    -> 'C.Char.charValueFromAddr
      CAPI_with             -> 'Foreign.with
      CAPI_allocaAndPeek    -> 'HsBindgen.Runtime.CAPI.allocaAndPeek
      ConstantArray_withPtr -> 'HsBindgen.Runtime.ConstantArray.withPtr
      IncompleteArray_withPtr -> 'HsBindgen.Runtime.IncompleteArray.withPtr

      -- Unsafe
      IO_unsafePerformIO -> 'System.IO.Unsafe.unsafePerformIO

      Bits_class        -> ''Data.Bits.Bits
      Bounded_class     -> ''Bounded
      Enum_class        -> ''Enum
      Eq_class          -> ''Eq
      FiniteBits_class  -> ''Data.Bits.FiniteBits
      Floating_class    -> ''Floating
      Fractional_class  -> ''Fractional
      Integral_class    -> ''Integral
      Ix_class          -> ''Data.Ix.Ix
      Num_class         -> ''Num
      Ord_class         -> ''Ord
      Read_class        -> ''Read
      Read_readPrec     -> 'Text.Read.readPrec
      Read_readList     -> 'Text.Read.readList
      Read_readListPrec -> 'Text.Read.readListPrec

      Real_class       -> ''Real
      RealFloat_class  -> ''RealFloat
      RealFrac_class   -> ''RealFrac
      Show_class       -> ''Show
      Show_showsPrec   -> 'showsPrec

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

      GHC_Float_castWord32ToFloat  -> 'GHC.Float.castWord32ToFloat
      GHC_Float_castWord64ToDouble -> 'GHC.Float.castWord64ToDouble
      CFloat_constructor  -> 'Foreign.C.Types.CFloat
      CDouble_constructor -> 'Foreign.C.Types.CDouble

      NonEmpty_constructor     -> '(NonEmpty.:|)
      NonEmpty_singleton       -> 'NonEmpty.singleton
      Map_fromList             -> 'Map.fromList
      Read_readListDefault     -> 'Text.Read.readListDefault
      Read_readListPrecDefault -> 'Text.Read.readListPrecDefault

      CEnum_class -> ''HsBindgen.Runtime.CEnum.CEnum
      CEnumZ_tycon -> ''HsBindgen.Runtime.CEnum.CEnumZ
      CEnum_toCEnum -> 'HsBindgen.Runtime.CEnum.toCEnum
      CEnum_fromCEnum -> 'HsBindgen.Runtime.CEnum.fromCEnum
      CEnum_declaredValues -> 'HsBindgen.Runtime.CEnum.declaredValues
      CEnum_showsUndeclared -> 'HsBindgen.Runtime.CEnum.showsUndeclared
      CEnum_readPrecUndeclared -> 'HsBindgen.Runtime.CEnum.readPrecUndeclared
      CEnum_isDeclared -> 'HsBindgen.Runtime.CEnum.isDeclared
      CEnum_mkDeclared -> 'HsBindgen.Runtime.CEnum.mkDeclared
      SequentialCEnum_class -> ''HsBindgen.Runtime.CEnum.SequentialCEnum
      SequentialCEnum_minDeclaredValue -> 'HsBindgen.Runtime.CEnum.minDeclaredValue
      SequentialCEnum_maxDeclaredValue -> 'HsBindgen.Runtime.CEnum.maxDeclaredValue
      CEnum_declaredValuesFromList -> 'HsBindgen.Runtime.CEnum.declaredValuesFromList
      CEnum_showsCEnum -> 'HsBindgen.Runtime.CEnum.showsCEnum
      CEnum_showsWrappedUndeclared -> 'HsBindgen.Runtime.CEnum.showsWrappedUndeclared
      CEnum_readPrecCEnum -> 'HsBindgen.Runtime.CEnum.readPrecCEnum
      CEnum_readPrecWrappedUndeclared -> 'HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared
      CEnum_seqIsDeclared -> 'HsBindgen.Runtime.CEnum.seqIsDeclared
      CEnum_seqMkDeclared -> 'HsBindgen.Runtime.CEnum.seqMkDeclared
      AsCEnum_type -> ''HsBindgen.Runtime.CEnum.AsCEnum
      AsSequentialCEnum_type -> ''HsBindgen.Runtime.CEnum.AsSequentialCEnum

      ByteArray_type       -> ''ByteArray
      SizedByteArray_type  -> ''HsBindgen.Runtime.SizedByteArray.SizedByteArray
      Block_type           -> ''HsBindgen.Runtime.Block.Block

      ByteArray_getUnionPayload -> 'HsBindgen.Runtime.ByteArray.getUnionPayload
      ByteArray_setUnionPayload -> 'HsBindgen.Runtime.ByteArray.setUnionPayload

      PrimType t           -> mkGlobalP t
      ComplexType          -> ''Data.Complex.Complex

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
mkGlobalP HsPrimCSize      = ''Foreign.C.Types.CSize
mkGlobalP HsPrimCStringLen = ''Foreign.C.String.CStringLen
mkGlobalP HsPrimInt        = ''Int

mkGlobalExpr :: Quote q => Global -> q TH.Exp
mkGlobalExpr n = case mkGlobalExpr' n of
  Left  err   -> panicPure $ displayException err
  Right mkExp -> mkExp name
  where
    name :: TH.Name
    name = mkGlobal n

data NoExpression =
    NoExpressionButType  Global
  | NoExpressionButClass Global
  deriving stock (Show)

instance Exception NoExpression where
  displayException = \case
    NoExpressionButType  g -> "Expected expression, but got type "  <> show g
    NoExpressionButClass g -> "Expected expression, but got class " <> show g

-- | Construct an 'TH.Exp' for a 'Global'
mkGlobalExpr' :: Quote q => Global -> Either NoExpression (TH.Name -> q TH.Exp)
mkGlobalExpr' n = case n of -- in definition order, no wildcards
    Tuple_type{}                  -> errType
    Tuple_constructor{}           -> con
    Applicative_pure              -> var
    Applicative_seq               -> var
    Monad_return                  -> var
    Monad_seq                     -> var
    StaticSize_class              -> errClass
    ReadRaw_class                 -> errClass
    WriteRaw_class                -> errClass
    Storable_class                -> errClass
    Storable_sizeOf               -> var
    Storable_alignment            -> var
    Storable_peekByteOff          -> var
    Storable_pokeByteOff          -> var
    Storable_peek                 -> var
    Storable_poke                 -> var
    Foreign_Ptr                   -> errType
    Ptr_constructor               -> con
    Foreign_FunPtr                -> errType
    ConstantArray                 -> errType
    IncompleteArray               -> errType
    IO_type                       -> errType
    HasFlexibleArrayMember_class  -> errClass
    HasFlexibleArrayMember_offset -> var
    Bitfield_peekBitOffWidth      -> var
    Bitfield_pokeBitOffWidth      -> var
    CharValue_tycon               -> errType
    CharValue_constructor         -> con
    CharValue_fromAddr            -> var
    ByteArray_setUnionPayload     -> var
    ByteArray_getUnionPayload     -> var
    CAPI_with                     -> var
    CAPI_allocaAndPeek            -> var
    ConstantArray_withPtr         -> var
    IncompleteArray_withPtr       -> var

    -- Unsafe
    IO_unsafePerformIO -> var

    -- Other type classes
    Bits_class        -> errClass
    Bounded_class     -> errClass
    Enum_class        -> errClass
    Eq_class          -> errClass
    FiniteBits_class  -> errClass
    Floating_class    -> errClass
    Fractional_class  -> errClass
    Integral_class    -> errClass
    Ix_class          -> errClass
    Num_class         -> errClass
    Ord_class         -> errClass
    Read_class        -> errClass
    Read_readPrec     -> var
    Read_readList     -> var
    Read_readListPrec -> var
    Real_class        -> errClass
    RealFloat_class   -> errClass
    RealFrac_class    -> errClass
    Show_class        -> errClass
    Show_showsPrec    -> var

    NomEq_class -> errClass

    Not_class             -> errClass
    Not_not               -> var
    Logical_class         -> errClass
    Logical_and           -> var
    Logical_or            -> var
    RelEq_class           -> errClass
    RelEq_eq              -> var
    RelEq_uneq            -> var
    RelOrd_class          -> errClass
    RelOrd_lt             -> var
    RelOrd_le             -> var
    RelOrd_gt             -> var
    RelOrd_ge             -> var
    Plus_class            -> errClass
    Plus_resTyCon         -> var
    Plus_plus             -> var
    Minus_class           -> errClass
    Minus_resTyCon        -> var
    Minus_negate          -> var
    Add_class             -> errClass
    Add_resTyCon          -> var
    Add_add               -> var
    Sub_class             -> errClass
    Sub_resTyCon          -> var
    Sub_minus             -> var
    Mult_class            -> errClass
    Mult_resTyCon         -> var
    Mult_mult             -> var
    Div_class             -> errClass
    Div_div               -> var
    Div_resTyCon          -> var
    Rem_class             -> errClass
    Rem_resTyCon          -> var
    Rem_rem               -> var
    Complement_class      -> errClass
    Complement_resTyCon   -> var
    Complement_complement -> var
    Bitwise_class         -> errClass
    Bitwise_resTyCon      -> var
    Bitwise_and           -> var
    Bitwise_or            -> var
    Bitwise_xor           -> var
    Shift_class           -> errClass
    Shift_resTyCon        -> var
    Shift_shiftL          -> var
    Shift_shiftR          -> var

    CFloat_constructor           -> con
    CDouble_constructor          -> con
    GHC_Float_castWord32ToFloat  -> var
    GHC_Float_castWord64ToDouble -> var

    NonEmpty_constructor     -> con
    NonEmpty_singleton       -> var
    Map_fromList             -> var
    Read_readListDefault     -> var
    Read_readListPrecDefault -> var

    CEnum_class                      -> errClass
    CEnumZ_tycon                     -> con
    CEnum_toCEnum                    -> var
    CEnum_fromCEnum                  -> var
    CEnum_declaredValues             -> var
    CEnum_showsUndeclared            -> var
    CEnum_readPrecUndeclared         -> var
    CEnum_isDeclared                 -> var
    CEnum_mkDeclared                 -> var
    SequentialCEnum_class            -> errClass
    SequentialCEnum_minDeclaredValue -> var
    SequentialCEnum_maxDeclaredValue -> var
    CEnum_declaredValuesFromList     -> var
    CEnum_showsCEnum                 -> var
    CEnum_showsWrappedUndeclared     -> var
    CEnum_readPrecCEnum              -> var
    CEnum_readPrecWrappedUndeclared  -> var
    CEnum_seqIsDeclared              -> var
    CEnum_seqMkDeclared              -> var
    AsCEnum_type                     -> errType
    AsSequentialCEnum_type           -> errType

    ByteArray_type      -> errType
    SizedByteArray_type -> errType
    Block_type          -> errType
    PrimType{}          -> errType
    ComplexType{}       -> errType
  where
    errType  = Left $ NoExpressionButType  n
    errClass = Left $ NoExpressionButClass n

    var = Right TH.varE
    con = Right TH.conE

mkExpr :: Quote q => Env ctx TH.Name -> SExpr ctx -> q TH.Exp
mkExpr env = \case
      EGlobal n     -> mkGlobalExpr n
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
          ( if C.canBeRepresentedAsRational f
              then [| f |]
              else [| Foreign.C.Types.CFloat $ castWord32ToFloat  $( TH.lift $ castFloatToWord32  f ) |]
          )
          (mkPrimType t)
      EDouble d t ->
        TH.sigE
          ( if C.canBeRepresentedAsRational d
              then [| d |]
              else [| Foreign.C.Types.CDouble $ castWord64ToDouble $( TH.lift $ castDoubleToWord64 d ) |]
          )
          (mkPrimType t)
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
          (mkPrimType HsPrimCStringLen)
      EApp f x      -> TH.appE (mkExpr env f) (mkExpr env x)
      EInfix op x y -> TH.infixE
                         (Just $ mkExpr env x)
                         (mkGlobalExpr op)
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
      ETup xs -> TH.tupE $ mkExpr env <$> xs
      EList xs -> TH.listE $ mkExpr env <$> xs

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
    TExt ExtHsRef{..} _typeSpec ->
        TH.conT . TH.mkName $ concat [
              Text.unpack (getHsModuleName extHsRefModule)
            , "."
            , Text.unpack (getHsIdentifier extHsRefIdentifier)
            ]

mkPrimType :: Quote q => HsPrimType -> q TH.Type
mkPrimType = TH.conT . mkGlobalP

mkDecl :: forall q. Guasi q => SDecl -> q [TH.Dec]
mkDecl = \case
      DVar Var {..} -> do
        let thVarName = hsNameToTH varName

        sequence $
          [ withDecDoc varComment $
              TH.sigD thVarName (mkType EmptyEnv varType)
          , simpleDecl thVarName varExpr
          ]

      DInst i  -> do

        let instComment = instanceComment i

        fmap singleton $
          TH.instanceD (return [])
                       (appsT (TH.conT $ mkGlobal $ instanceClass i)
                           (map (mkType EmptyEnv) $ instanceArgs i))
                       -- Workaround for issue #976
                       ( ( (\case
                              (h:t) -> withDecDoc instComment h : t
                              x     -> x
                           )
                         $ map instTySyn (instanceTypes i)
                         )
                           ++ map (\(x, f) -> simpleDecl (mkGlobal x) f) (instanceDecs i)
                         )
      DRecord d -> do
        let _fieldsAndDocs :: ([q TH.VarBangType], [(TH.DocLoc, Maybe Comment)])
            _fieldsAndDocs@(fields, docs) = unzip
              [ ( TH.varBangType thFieldName $
                    TH.bangType
                      (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness)
                      (mkType EmptyEnv (fieldType f))
                , ((TH.DeclDoc thFieldName), fComment)
                )
              | f <- dataFields d
              , let thFieldName = hsNameToTH (fieldName f)
                    fComment = fieldComment f
              ]

        traverse_ (uncurry putFieldDoc) docs

        fmap singleton $
          withDecDoc (dataComment d) $
            TH.dataD
              (TH.cxt [])
              (hsNameToTH $ dataType d)
              []
              Nothing
              [TH.recC (hsNameToTH (dataCon d)) fields]
              (nestedDeriving $ dataDeriv d)

      DEmptyData d -> do

        let thEmptyDataName = hsNameToTH (emptyDataName d)

        fmap singleton $
          withDecDoc (emptyDataComment d) $
            TH.dataD (TH.cxt []) thEmptyDataName [] Nothing [] []

      DNewtype n -> do
        let thFieldName = hsNameToTH (fieldName (newtypeField n))
            thNewtypeName = hsNameToTH $ newtypeName n
            fComment = fieldComment (newtypeField n)
            newTyComment = newtypeComment n
            field :: q TH.VarBangType
            field = TH.varBangType thFieldName $
              TH.bangType
                (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness)
                (mkType EmptyEnv (fieldType (newtypeField n)))

        putFieldDoc (TH.DeclDoc thFieldName) fComment

        fmap singleton $
          withDecDoc newTyComment $
            TH.newtypeD
              (TH.cxt [])
              thNewtypeName
              []
              Nothing
              (TH.recC (hsNameToTH (newtypeCon n))
              [field])
              (nestedDeriving $ newtypeDeriv n)

      DDerivingInstance DerivingInstance {..} -> do
        s' <- strategy derivingInstanceStrategy

        fmap singleton $
          withDecDoc derivingInstanceComment $
            TH.standaloneDerivWithStrategyD
              (Just s')
              (TH.cxt [])
              (mkType EmptyEnv derivingInstanceType)

      DForeignImport ForeignImport {..} -> do
        -- Variable names here refer to the syntax of foreign declarations at
        -- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1540008.4>
        --
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/94>
        -- We should generate both safe and unsafe bindings.
        let safety :: TH.Safety
            safety = TH.Safe

            callconv :: TH.Callconv
            impent   :: String
            (callconv, impent) =
              case foreignImportCallConv of
                CallConvUserlandCAPI _ -> (TH.CCall,
                    Text.unpack foreignImportOrigName
                  )
                CallConvGhcCAPI header -> (TH.CApi, concat [
                    header
                  , Text.unpack foreignImportOrigName
                  ])
                CallConvGhcCCall style -> (TH.CCall, concat [
                    case style of
                      ImportAsValue -> ""
                      ImportAsPtr   -> "&"
                  , Text.unpack foreignImportOrigName
                  ])

            importType =
              case foreignImportResultType of
                NormalResultType t ->
                  foldr (TFun . functionParameterType) t foreignImportParameters
                HeapResultType t   ->
                  foldr TFun (TApp (TGlobal IO_type) (TGlobal (PrimType HsPrimUnit)))
                             (map functionParameterType foreignImportParameters ++ [t])

        fmap singleton $
          withDecDoc foreignImportComment $
            fmap TH.ForeignD $
              TH.ImportF
                <$> pure callconv
                <*> pure safety
                <*> pure impent
                <*> pure (hsNameToTH foreignImportName)
                <*> mkType EmptyEnv importType

      DPatternSynonym ps -> do
        let thPatSynName = hsNameToTH (patSynName ps)

        sequence
          [ withDecDoc (patSynComment ps) $
              TH.patSynSigD
              thPatSynName
              (mkType EmptyEnv (patSynType ps))
          , TH.patSynD
            thPatSynName
            (TH.prefixPatSyn [])
            TH.implBidir
            (mkPat (patSynRHS ps))
          ]

      DPragma p ->
          case p of
            NOINLINE n ->
              singleton <$>
                TH.pragInlD (hsNameToTH n) TH.NoInline TH.FunLike TH.AllPhases
    where
      simpleDecl :: TH.Name -> SExpr EmptyCtx -> q TH.Dec
      simpleDecl x f = TH.valD (TH.varP x) (TH.normalB $ mkExpr EmptyEnv f) []

      instTySyn :: (Global, ClosedType, ClosedType) -> q TH.Dec
      instTySyn (g, typArg, typSyn) =
        TH.TySynInstD
          <$> liftM2
                (TH.TySynEqn Nothing)
                (mkType EmptyEnv (TApp (TGlobal g) typArg))
                (mkType EmptyEnv typSyn)

-- | Nested deriving clauses (part of a datatype declaration)
nestedDeriving :: forall q.
     Quote q
  => [(Hs.Strategy ClosedType, [Global])] -> [q TH.DerivClause]
nestedDeriving = map aux
  where
    aux :: (Hs.Strategy ClosedType, [Global]) -> q TH.DerivClause
    aux (s, clss) = do
        s' <- strategy s
        TH.derivClause (Just s') (map (TH.conT . mkGlobal) clss)

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
