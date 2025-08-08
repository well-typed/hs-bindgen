{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.Artefact.TH.Translation (
    mkDecl,
) where

import Control.Monad (liftM2)
import Data.Bits qualified
import Data.Ix qualified
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Void qualified
import Foreign qualified
import Foreign.C.Types qualified
import Foreign.C.String qualified
import Foreign.Ptr qualified
import Foreign.Storable qualified
import Language.Haskell.TH (Quote)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import GHC.Exts qualified as IsList(IsList(..))
import GHC.Ptr ( Ptr(Ptr) )
import Text.Read qualified

import GHC.Float
  ( castWord64ToDouble, castDoubleToWord64
  , castWord32ToFloat , castFloatToWord32 )

import C.Char (CharValue(..), charValueFromAddr)
import C.Expr.HostPlatform qualified as C
import HsBindgen.Errors
import HsBindgen.Guasi
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell
import HsBindgen.NameHint
import HsBindgen.Backend.SHs.AST

import HsBindgen.Runtime.IncompleteArray     qualified
import HsBindgen.Runtime.Bitfield            qualified
import HsBindgen.Runtime.Block               qualified
import HsBindgen.Runtime.ByteArray           qualified
import HsBindgen.Runtime.CAPI                qualified
import HsBindgen.Runtime.CEnum               qualified
import HsBindgen.Runtime.ConstantArray       qualified
import HsBindgen.Runtime.FlexibleArrayMember qualified
import HsBindgen.Runtime.Marshal             qualified
import HsBindgen.Runtime.SizedByteArray      qualified

import DeBruijn (Env (..), lookupEnv, EmptyCtx, Add (..))
import GHC.Exts (Int(..), sizeofByteArray#)
import HsBindgen.Backend.Hs.Haddock.Documentation (Comment)

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

-- | Construct an 'TH.Exp' for a 'Global'
mkGlobalExpr :: Quote q => Global -> q TH.Exp
mkGlobalExpr n = case n of -- in definition order, no wildcards
    Tuple_type{}         -> panicPure "type in expression"
    Tuple_constructor{}  -> TH.conE name
    Applicative_pure     -> TH.varE name
    Applicative_seq      -> TH.varE name
    Monad_return         -> TH.varE name
    Monad_seq            -> TH.varE name
    StaticSize_class     -> panicPure "class in expression"
    ReadRaw_class        -> panicPure "class in expression"
    WriteRaw_class       -> panicPure "class in expression"
    Storable_class       -> panicPure "class in expression"
    Storable_sizeOf      -> TH.varE name
    Storable_alignment   -> TH.varE name
    Storable_peekByteOff -> TH.varE name
    Storable_pokeByteOff -> TH.varE name
    Storable_peek        -> TH.varE name
    Storable_poke        -> TH.varE name
    Foreign_Ptr          -> panicPure "type in expression"
    Ptr_constructor      -> TH.conE name
    Foreign_FunPtr       -> panicPure "type in expression"
    ConstantArray        -> panicPure "type in expression"
    IncompleteArray      -> panicPure "type in expression"
    IO_type              -> panicPure "type in expression"
    HasFlexibleArrayMember_class -> panicPure "class in expression"
    HasFlexibleArrayMember_offset -> TH.varE name
    Bitfield_peekBitOffWidth -> TH.varE name
    Bitfield_pokeBitOffWidth -> TH.varE name
    CharValue_tycon      -> panicPure "type in expression"
    CharValue_constructor -> TH.conE name
    CharValue_fromAddr   -> TH.varE name
    ByteArray_setUnionPayload -> TH.varE name
    ByteArray_getUnionPayload -> TH.varE name
    CAPI_with             -> TH.varE name
    CAPI_allocaAndPeek    -> TH.varE name
    ConstantArray_withPtr -> TH.varE name
    IncompleteArray_withPtr -> TH.varE name

    -- Other type classes
    Bits_class        -> panicPure "class in expression"
    Bounded_class     -> panicPure "class in expression"
    Enum_class        -> panicPure "class in expression"
    Eq_class          -> panicPure "class in expression"
    FiniteBits_class  -> panicPure "class in expression"
    Floating_class    -> panicPure "class in expression"
    Fractional_class  -> panicPure "class in expression"
    Integral_class    -> panicPure "class in expression"
    Ix_class          -> panicPure "class in expression"
    Num_class         -> panicPure "class in expression"
    Ord_class         -> panicPure "class in expression"
    Read_class        -> panicPure "class in expression"
    Read_readPrec     -> TH.varE name
    Read_readList     -> TH.varE name
    Read_readListPrec -> TH.varE name
    Real_class        -> panicPure "class in expression"
    RealFloat_class   -> panicPure "class in expression"
    RealFrac_class    -> panicPure "class in expression"
    Show_class        -> panicPure "class in expression"
    Show_showsPrec    -> TH.varE name

    NomEq_class -> panicPure "class in expression"

    Not_class             -> panicPure "class in expression"
    Not_not               -> TH.varE name
    Logical_class         -> panicPure "class in expression"
    Logical_and           -> TH.varE name
    Logical_or            -> TH.varE name
    RelEq_class           -> panicPure "class in expression"
    RelEq_eq              -> TH.varE name
    RelEq_uneq            -> TH.varE name
    RelOrd_class          -> panicPure "class in expression"
    RelOrd_lt             -> TH.varE name
    RelOrd_le             -> TH.varE name
    RelOrd_gt             -> TH.varE name
    RelOrd_ge             -> TH.varE name
    Plus_class            -> panicPure "class in expression"
    Plus_resTyCon         -> TH.varE name
    Plus_plus             -> TH.varE name
    Minus_class           -> panicPure "class in expression"
    Minus_resTyCon        -> TH.varE name
    Minus_negate          -> TH.varE name
    Add_class             -> panicPure "class in expression"
    Add_resTyCon          -> TH.varE name
    Add_add               -> TH.varE name
    Sub_class             -> panicPure "class in expression"
    Sub_resTyCon          -> TH.varE name
    Sub_minus             -> TH.varE name
    Mult_class            -> panicPure "class in expression"
    Mult_resTyCon         -> TH.varE name
    Mult_mult             -> TH.varE name
    Div_class             -> panicPure "class in expression"
    Div_div               -> TH.varE name
    Div_resTyCon          -> TH.varE name
    Rem_class             -> panicPure "class in expression"
    Rem_resTyCon          -> TH.varE name
    Rem_rem               -> TH.varE name
    Complement_class      -> panicPure "class in expression"
    Complement_resTyCon   -> TH.varE name
    Complement_complement -> TH.varE name
    Bitwise_class         -> panicPure "class in expression"
    Bitwise_resTyCon      -> TH.varE name
    Bitwise_and           -> TH.varE name
    Bitwise_or            -> TH.varE name
    Bitwise_xor           -> TH.varE name
    Shift_class           -> panicPure "class in expression"
    Shift_resTyCon        -> TH.varE name
    Shift_shiftL          -> TH.varE name
    Shift_shiftR          -> TH.varE name

    CFloat_constructor           -> TH.conE name
    CDouble_constructor          -> TH.conE name
    GHC_Float_castWord32ToFloat  -> TH.varE name
    GHC_Float_castWord64ToDouble -> TH.varE name

    NonEmpty_constructor     -> TH.conE name
    NonEmpty_singleton       -> TH.varE name
    Map_fromList             -> TH.varE name
    Read_readListDefault     -> TH.varE name
    Read_readListPrecDefault -> TH.varE name

    CEnum_class                      -> panicPure "class in expression"
    CEnumZ_tycon                     -> TH.conE name
    CEnum_toCEnum                    -> TH.varE name
    CEnum_fromCEnum                  -> TH.varE name
    CEnum_declaredValues             -> TH.varE name
    CEnum_showsUndeclared            -> TH.varE name
    CEnum_readPrecUndeclared         -> TH.varE name
    CEnum_isDeclared                 -> TH.varE name
    CEnum_mkDeclared                 -> TH.varE name
    SequentialCEnum_class            -> panicPure "class in expression"
    SequentialCEnum_minDeclaredValue -> TH.varE name
    SequentialCEnum_maxDeclaredValue -> TH.varE name
    CEnum_declaredValuesFromList     -> TH.varE name
    CEnum_showsCEnum                 -> TH.varE name
    CEnum_showsWrappedUndeclared     -> TH.varE name
    CEnum_readPrecCEnum              -> TH.varE name
    CEnum_readPrecWrappedUndeclared  -> TH.varE name
    CEnum_seqIsDeclared              -> TH.varE name
    CEnum_seqMkDeclared              -> TH.varE name
    AsCEnum_type                     -> panicPure "type in expression"
    AsSequentialCEnum_type           -> panicPure "type in expression"

    ByteArray_type      -> panicPure "type in expression"
    SizedByteArray_type -> panicPure "type in expression"
    Block_type          -> panicPure "type in expression"
    PrimType{}          -> panicPure "type in expression"
  where
    name :: TH.Name
    name = mkGlobal n

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
                CallConvUserlandCAPI -> (TH.CCall,
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

        fmap singleton $
          withDecDoc foreignImportComment $
            fmap TH.ForeignD $
              TH.ImportF
                <$> pure callconv
                <*> pure safety
                <*> pure impent
                <*> pure (hsNameToTH foreignImportName)
                <*> mkType EmptyEnv foreignImportType

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

      DCSource src -> do
          addCSource src
          return []
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
