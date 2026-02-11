{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.SHs.Global (
    -- * Global symbols
    Global(CustomGlobal)
  , getName
  , getHsImport

    -- ** Specific to @hs-bindgen@
  , BindgenGlobal(..)
  , bindgenGlobal

    -- ** Specifc to C expressions
  , CExprGlobal(..)
  , cExprGlobal
  ) where

import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH

import C.Expr.HostPlatform qualified

import HsBindgen.Runtime.BitfieldPtr qualified as BitfieldPtr
import HsBindgen.Runtime.Block qualified as Block
import HsBindgen.Runtime.CEnum qualified as CEnum
import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.FLAM qualified as FLAM
import HsBindgen.Runtime.HasCBitfield qualified as HasCBitfield
import HsBindgen.Runtime.HasCField qualified as HasCField
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.Internal.Prelude qualified as RIP
import HsBindgen.Runtime.Marshal qualified as Marshal
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.Errors (panicPure)
import HsBindgen.Language.Haskell qualified as Hs

data BindgenImport =
    -- | Unqualified import from "Prelude".
    IHaskellPrelude
    -- | Qualified import from "HsBindgen.Runtime.Internal.Prelude".
  | IRuntimeInternalPrelude
    -- | Qualified import from other modules in @hs-bindgen-runtime@ with a
    --   corresponding abbreviation ("qualified as").
  | IRuntimeModule String
  deriving stock (Eq, Ord, Show)

data Global =
    BindgenGlobal BindgenImport     TH.Name
  | CustomGlobal  (Maybe Hs.Import) TH.Name
  deriving stock (Eq, Ord, Show)

getName :: Global -> TH.Name
getName = \case
    BindgenGlobal _ n -> n
    CustomGlobal  _ n -> n

getHsImport :: Global -> Hs.Import
getHsImport = \case
    BindgenGlobal i n -> toHsImport n i
    CustomGlobal  mi n -> case mi of
      Nothing -> Hs.QualifiedImport (unsafeGetModuleName n) Nothing
      Just i  -> i
  where
    toHsImport :: TH.Name -> BindgenImport -> Hs.Import
    toHsImport n = \case
      IHaskellPrelude ->
        Hs.UnqualifiedImport "Prelude"
      IRuntimeInternalPrelude ->
        Hs.QualifiedImport "HsBindgen.Runtime.Internal.Prelude" (Just "RIP")
      IRuntimeModule as ->
        Hs.QualifiedImport (unsafeGetModuleName n) (Just as)

    unsafeGetModuleName :: TH.Name -> Hs.ModuleName
    unsafeGetModuleName n = case TH.nameModule n of
      Nothing -> panicPure $ "getHsImport: expected name with module: " ++ show n
      Just m  -> Hs.ModuleName $ Text.pack m

data BindgenGlobal =
    Applicative_pure
  | Applicative_seq
  | Maybe_just
  | Maybe_nothing
  | Monad_return
  | Monad_seq

    -- Function pointers
  | ToFunPtr_class
  | ToFunPtr_toFunPtr
  | FromFunPtr_class
  | FromFunPtr_fromFunPtr

    -- Foreign function interface
  | Foreign_Ptr
  | Foreign_Ptr_constructor
  | Foreign_FunPtr
  | Foreign_plusPtr
  | Foreign_StablePtr
  | ConstantArray
  | IncompleteArray
  | IO_type
  | CharValue_tycon
  | CharValue_constructor
  | CharValue_fromAddr
  | ByteArray_setUnionPayload
  | ByteArray_getUnionPayload
  | Capi_with
  | Capi_allocaAndPeek
  | Generic_class

    -- StaticSize
  | StaticSize_class
  | StaticSize_staticSizeOf
  | StaticSize_staticAlignment

    -- ReadRaw
  | ReadRaw_class
  | ReadRaw_readRaw
  | ReadRaw_readRawByteOff

    -- WriteRaw
  | WriteRaw_class
  | WriteRaw_writeRaw
  | WriteRaw_writeRawByteOff

    -- EquivStorable
  | EquivStorable_type

    -- Storable
  | Storable_class
  | Storable_sizeOf
  | Storable_alignment
  | Storable_peekByteOff
  | Storable_pokeByteOff
  | Storable_peek
  | Storable_poke

    -- Flexible array members
  | Flam_Offset_class
  | Flam_Offset_offset
  | Flam_WithFlam_constructor

    -- HasCField
  | HasCField_class
  | HasCField_CFieldType
  | HasCField_offset#
  | HasCField_fromPtr
  | HasCField_peek
  | HasCField_poke
  | HasCField_readRaw
  | HasCField_writeRaw

    -- BitfieldPtr
  | HasCBitfield_BitfieldPtr

    -- HasCBitfield
  | HasCBitfield_class
  | HasCBitfield_CBitfieldType
  | HasCBitfield_bitfieldOffset#
  | HasCBitfield_bitfieldWidth#
  | HasCBitfield_toPtr
  | HasCBitfield_peek
  | HasCBitfield_poke

    -- HasField
  | HasField_class
  | HasField_getField

    -- Proxy
  | Proxy_type
  | Proxy_constructor

    -- HasFFIType
  | HasFFIType_class
  | HasFFIType_fromFFIType
  | HasFFIType_toFFIType
  | HasFFIType_castFunPtrFromFFIType
  | HasFFIType_castFunPtrToFFIType

    -- Functor
  | Functor_fmap

    -- Unsafe
  | IO_unsafePerformIO

    -- PtrConst
  | PtrConst_type
  | PtrConst_unsafeFromPtr
  | PtrConst_unsafeToPtr
  | PtrConst_peek

    -- Primitive
  | Prim_class
  | Prim_sizeOf#
  | Prim_alignment#
  | Prim_indexByteArray#
  | Prim_readByteArray#
  | Prim_writeByteArray#
  | Prim_indexOffAddr#
  | Prim_readOffAddr#
  | Prim_writeOffAddr#
  | Prim_add#
  | Prim_mul#

    -- Other type classes
  | Bitfield_class
  | Bits_class
  | Bounded_class
  | Enum_class
  | Eq_class
  | FiniteBits_class
  | Floating_class
  | Fractional_class
  | Integral_class
  | Ix_class
  | Num_class
  | Ord_class
  | Read_class
  | Read_readPrec
  | Read_readList
  | Read_readListPrec
  | Read_readListDefault
  | Read_readListPrecDefault
  | Real_class
  | RealFloat_class
  | RealFrac_class
  | Show_class
  | Show_showsPrec

    -- Floating point numbers
  | CFloat_constructor
  | CDouble_constructor
  | GHC_Float_castWord32ToFloat
  | GHC_Float_castWord64ToDouble

    -- Non-empty lists
  | NonEmpty_constructor
  | NonEmpty_singleton

    -- C enumerations
  | CEnum_class
  | CEnumZ_tycon
  | CEnum_toCEnum
  | CEnum_fromCEnum
  | CEnum_declaredValues
  | CEnum_showsUndeclared
  | CEnum_readPrecUndeclared
  | CEnum_isDeclared
  | CEnum_mkDeclared
  | SequentialCEnum_class
  | SequentialCEnum_minDeclaredValue
  | SequentialCEnum_maxDeclaredValue
  | CEnum_declaredValuesFromList
  | CEnum_showsCEnum
  | CEnum_showsWrappedUndeclared
  | CEnum_readPrecCEnum
  | CEnum_readPrecWrappedUndeclared
  | CEnum_seqIsDeclared
  | CEnum_seqMkDeclared
  | AsCEnum_type
  | AsSequentialCEnum_type

    -- Arrays
  | ByteArray_type
  | SizedByteArray_type
  | Block_type

    -- Complex numbers
  | ComplexType

    -- C types
  | Void_type
  | Char_type
  | Int_type
  | Double_type
  | Float_type
  | Bool_type
  | Int8_type
  | Int16_type
  | Int32_type
  | Int64_type
  | Word_type
  | Word8_type
  | Word16_type
  | Word32_type
  | Word64_type
  | CChar_type
  | CSChar_type
  | CUChar_type
  | CShort_type
  | CUShort_type
  | CInt_type
  | CUInt_type
  | CLong_type
  | CULong_type
  | CLLong_type
  | CULLong_type
  | CBool_type
  | CFloat_type
  | CDouble_type
  | CStringLen_type
  | CPtrdiff_type
  deriving stock (Eq, Ord, Show)


bindgenGlobal :: BindgenGlobal -> Global
bindgenGlobal x = uncurry BindgenGlobal $ case x of
    -- When adding a new global that resolves to a non-qualified identifier, be
    -- sure to reserve the name in "HsBindgen.Backend.Hs.AST.Name".
    Applicative_pure    -> (IHaskellPrelude, 'pure)
    Applicative_seq     -> (IHaskellPrelude, '(<*>))
    Maybe_just          -> (IHaskellPrelude, 'Just)
    Maybe_nothing       -> (IHaskellPrelude, 'Nothing)
    Monad_return        -> (IHaskellPrelude, 'return)
    Monad_seq           -> (IHaskellPrelude, '(>>))

    -- Function pointers
    ToFunPtr_class        -> (IRuntimeInternalPrelude, ''RIP.ToFunPtr)
    ToFunPtr_toFunPtr     -> (IRuntimeInternalPrelude,  'RIP.toFunPtr)
    FromFunPtr_class      -> (IRuntimeInternalPrelude, ''RIP.FromFunPtr)
    FromFunPtr_fromFunPtr -> (IRuntimeInternalPrelude,  'RIP.fromFunPtr)

    -- Foreign function interface
    Foreign_Ptr               -> (IRuntimeInternalPrelude, ''RIP.Ptr)
    Foreign_Ptr_constructor   -> (IRuntimeInternalPrelude,  'RIP.Ptr)
    Foreign_FunPtr            -> (IRuntimeInternalPrelude, ''RIP.FunPtr)
    Foreign_plusPtr           -> (IRuntimeInternalPrelude,  'RIP.plusPtr)
    Foreign_StablePtr         -> (IRuntimeInternalPrelude, ''RIP.StablePtr)
    ConstantArray             -> (IRuntimeModule "CA",     ''CA.ConstantArray)
    IncompleteArray           -> (IRuntimeModule "IA",     ''IA.IncompleteArray)
    IO_type                   -> (IHaskellPrelude,         ''IO)
    CharValue_tycon           -> (IRuntimeInternalPrelude, ''RIP.CharValue)
    CharValue_constructor     -> (IRuntimeInternalPrelude,  'RIP.CharValue)
    CharValue_fromAddr        -> (IRuntimeInternalPrelude,  'RIP.charValueFromAddr)
    ByteArray_getUnionPayload -> (IRuntimeInternalPrelude,  'RIP.getUnionPayload)
    ByteArray_setUnionPayload -> (IRuntimeInternalPrelude,  'RIP.setUnionPayload)
    Capi_with                 -> (IRuntimeInternalPrelude,  'RIP.with)
    Capi_allocaAndPeek        -> (IRuntimeInternalPrelude,  'RIP.allocaAndPeek)
    Generic_class             -> (IRuntimeInternalPrelude, ''RIP.Generic)

    -- StaticSize
    StaticSize_class           -> (IRuntimeModule "Marshal", ''Marshal.StaticSize)
    StaticSize_staticSizeOf    -> (IRuntimeModule "Marshal",  'Marshal.staticSizeOf)
    StaticSize_staticAlignment -> (IRuntimeModule "Marshal",  'Marshal.staticAlignment)

    -- ReadRaw
    ReadRaw_class          -> (IRuntimeModule "Marshal", ''Marshal.ReadRaw)
    ReadRaw_readRaw        -> (IRuntimeModule "Marshal",  'Marshal.readRaw)
    ReadRaw_readRawByteOff -> (IRuntimeModule "Marshal",  'Marshal.readRawByteOff)

    -- WriteRaw
    WriteRaw_class           -> (IRuntimeModule "Marshal", ''Marshal.WriteRaw)
    WriteRaw_writeRaw        -> (IRuntimeModule "Marshal",  'Marshal.writeRaw)
    WriteRaw_writeRawByteOff -> (IRuntimeModule "Marshal",  'Marshal.writeRawByteOff)

    -- EquivStorable
    EquivStorable_type -> (IRuntimeModule "Marshal", ''Marshal.EquivStorable)

    -- Storable
    Storable_class       -> (IRuntimeInternalPrelude, ''RIP.Storable)
    Storable_sizeOf      -> (IRuntimeInternalPrelude,  'RIP.sizeOf)
    Storable_alignment   -> (IRuntimeInternalPrelude,  'RIP.alignment)
    Storable_peekByteOff -> (IRuntimeInternalPrelude,  'RIP.peekByteOff)
    Storable_pokeByteOff -> (IRuntimeInternalPrelude,  'RIP.pokeByteOff)
    Storable_peek        -> (IRuntimeInternalPrelude,  'RIP.peek)
    Storable_poke        -> (IRuntimeInternalPrelude,  'RIP.poke)

    -- Flexible array members
    Flam_Offset_class         -> (IRuntimeModule "FLAM", ''FLAM.Offset)
    Flam_Offset_offset        -> (IRuntimeModule "FLAM",  'FLAM.offset)
    Flam_WithFlam_constructor -> (IRuntimeModule "FLAM",  'FLAM.WithFlam)

    -- HasCField
    HasCField_class      -> (IRuntimeModule "HasCField", ''HasCField.HasCField)
    HasCField_CFieldType -> (IRuntimeModule "HasCField", ''HasCField.CFieldType)
    HasCField_offset#    -> (IRuntimeModule "HasCField",  'HasCField.offset#)
    HasCField_fromPtr    -> (IRuntimeModule "HasCField",  'HasCField.fromPtr)
    HasCField_peek       -> (IRuntimeModule "HasCField",  'HasCField.peek)
    HasCField_poke       -> (IRuntimeModule "HasCField",  'HasCField.poke)
    HasCField_readRaw    -> (IRuntimeModule "HasCField",  'HasCField.readRaw)
    HasCField_writeRaw   -> (IRuntimeModule "HasCField",  'HasCField.writeRaw)

    -- BitfieldPtr
    HasCBitfield_BitfieldPtr     -> (IRuntimeModule "BitfieldPtr", ''BitfieldPtr.BitfieldPtr)

    -- HasCBitfield
    HasCBitfield_class           -> (IRuntimeModule "HasCBitfield", ''HasCBitfield.HasCBitfield)
    HasCBitfield_CBitfieldType   -> (IRuntimeModule "HasCBitfield", ''HasCBitfield.CBitfieldType)
    HasCBitfield_bitfieldOffset# -> (IRuntimeModule "HasCBitfield",  'HasCBitfield.bitfieldOffset#)
    HasCBitfield_bitfieldWidth#  -> (IRuntimeModule "HasCBitfield",  'HasCBitfield.bitfieldWidth#)
    HasCBitfield_toPtr           -> (IRuntimeModule "HasCBitfield",  'HasCBitfield.toPtr)
    HasCBitfield_peek            -> (IRuntimeModule "HasCBitfield",  'HasCBitfield.peek)
    HasCBitfield_poke            -> (IRuntimeModule "HasCBitfield",  'HasCBitfield.poke)

    -- HasField
    HasField_class    -> (IRuntimeInternalPrelude, ''RIP.HasField)
    HasField_getField -> (IRuntimeInternalPrelude,  'RIP.getField)

    -- Proxy
    Proxy_type        -> (IRuntimeInternalPrelude, ''RIP.Proxy)
    Proxy_constructor -> (IRuntimeInternalPrelude,  'RIP.Proxy)

    -- HasFFIType
    HasFFIType_class                 -> (IRuntimeInternalPrelude, ''RIP.HasFFIType)
    HasFFIType_fromFFIType           -> (IRuntimeInternalPrelude,  'RIP.fromFFIType)
    HasFFIType_toFFIType             -> (IRuntimeInternalPrelude,  'RIP.toFFIType)
    HasFFIType_castFunPtrFromFFIType -> (IRuntimeInternalPrelude,  'RIP.castFunPtrFromFFIType)
    HasFFIType_castFunPtrToFFIType   -> (IRuntimeInternalPrelude,  'RIP.castFunPtrToFFIType)

    -- Functor
    Functor_fmap -> (IHaskellPrelude, 'fmap)

    -- Unsafe
    IO_unsafePerformIO -> (IRuntimeInternalPrelude, 'RIP.unsafePerformIO)

    -- PtrConst
    PtrConst_type          -> (IRuntimeModule "PtrConst", ''PtrConst.PtrConst)
    PtrConst_unsafeFromPtr -> (IRuntimeModule "PtrConst",  'PtrConst.unsafeFromPtr)
    PtrConst_unsafeToPtr   -> (IRuntimeModule "PtrConst",  'PtrConst.unsafeToPtr)
    PtrConst_peek          -> (IRuntimeModule "PtrConst",  'PtrConst.peek)

    -- Primitive
    Prim_class           -> (IRuntimeInternalPrelude, ''RIP.Prim)
    Prim_sizeOf#         -> (IRuntimeInternalPrelude,  'RIP.sizeOf#)
    Prim_alignment#      -> (IRuntimeInternalPrelude,  'RIP.alignment#)
    Prim_indexByteArray# -> (IRuntimeInternalPrelude,  'RIP.indexByteArray#)
    Prim_readByteArray#  -> (IRuntimeInternalPrelude,  'RIP.readByteArray#)
    Prim_writeByteArray# -> (IRuntimeInternalPrelude,  'RIP.writeByteArray#)
    Prim_indexOffAddr#   -> (IRuntimeInternalPrelude,  'RIP.indexOffAddr#)
    Prim_readOffAddr#    -> (IRuntimeInternalPrelude,  'RIP.readOffAddr#)
    Prim_writeOffAddr#   -> (IRuntimeInternalPrelude,  'RIP.writeOffAddr#)
    Prim_add#            -> (IRuntimeInternalPrelude, '(RIP.+#))
    Prim_mul#            -> (IRuntimeInternalPrelude, '(RIP.*#))

    -- Other type classes
    Bitfield_class           -> (IRuntimeInternalPrelude, ''RIP.Bitfield)
    Bits_class               -> (IRuntimeInternalPrelude, ''RIP.Bits)
    Bounded_class            -> (IHaskellPrelude,         ''Bounded)
    Enum_class               -> (IHaskellPrelude,         ''Enum)
    Eq_class                 -> (IHaskellPrelude,         ''Eq)
    FiniteBits_class         -> (IRuntimeInternalPrelude, ''RIP.FiniteBits)
    Floating_class           -> (IHaskellPrelude,         ''Floating)
    Fractional_class         -> (IHaskellPrelude,         ''Fractional)
    Integral_class           -> (IHaskellPrelude,         ''Integral)
    Ix_class                 -> (IRuntimeInternalPrelude, ''RIP.Ix)
    Num_class                -> (IHaskellPrelude,         ''Num)
    Ord_class                -> (IHaskellPrelude,         ''Ord)
    Read_class               -> (IHaskellPrelude,         ''Read)
    Read_readPrec            -> (IRuntimeInternalPrelude,  'RIP.readPrec)
    Read_readList            -> (IHaskellPrelude,          'readList)
    Read_readListPrec        -> (IRuntimeInternalPrelude,  'RIP.readListPrec)
    Read_readListDefault     -> (IRuntimeInternalPrelude,  'RIP.readListDefault)
    Read_readListPrecDefault -> (IRuntimeInternalPrelude,  'RIP.readListPrecDefault)
    Real_class               -> (IHaskellPrelude,         ''Real)
    RealFloat_class          -> (IHaskellPrelude,         ''RealFloat)
    RealFrac_class           -> (IHaskellPrelude,         ''RealFrac)
    Show_class               -> (IHaskellPrelude,         ''Show)
    Show_showsPrec           -> (IHaskellPrelude,          'showsPrec)

    -- Floating point numbers
    GHC_Float_castWord32ToFloat  -> (IRuntimeInternalPrelude,  'RIP.castWord32ToFloat)
    GHC_Float_castWord64ToDouble -> (IRuntimeInternalPrelude,  'RIP.castWord64ToDouble)
    CFloat_constructor           -> (IRuntimeInternalPrelude, ''RIP.CFloat)
    CDouble_constructor          -> (IRuntimeInternalPrelude, ''RIP.CDouble)

    -- Non-empty lists
    NonEmpty_constructor     -> (IRuntimeInternalPrelude, '(RIP.:|))
    NonEmpty_singleton       -> (IRuntimeInternalPrelude,  'RIP.singleton)

    -- C enumerations
    CEnum_class                      -> (IRuntimeModule "CEnum", ''CEnum.CEnum)
    CEnumZ_tycon                     -> (IRuntimeModule "CEnum", ''CEnum.CEnumZ)
    CEnum_toCEnum                    -> (IRuntimeModule "CEnum",  'CEnum.toCEnum)
    CEnum_fromCEnum                  -> (IRuntimeModule "CEnum",  'CEnum.fromCEnum)
    CEnum_declaredValues             -> (IRuntimeModule "CEnum",  'CEnum.declaredValues)
    CEnum_showsUndeclared            -> (IRuntimeModule "CEnum",  'CEnum.showsUndeclared)
    CEnum_readPrecUndeclared         -> (IRuntimeModule "CEnum",  'CEnum.readPrecUndeclared)
    CEnum_isDeclared                 -> (IRuntimeModule "CEnum",  'CEnum.isDeclared)
    CEnum_mkDeclared                 -> (IRuntimeModule "CEnum",  'CEnum.mkDeclared)
    SequentialCEnum_class            -> (IRuntimeModule "CEnum", ''CEnum.SequentialCEnum)
    SequentialCEnum_minDeclaredValue -> (IRuntimeModule "CEnum",  'CEnum.minDeclaredValue)
    SequentialCEnum_maxDeclaredValue -> (IRuntimeModule "CEnum",  'CEnum.maxDeclaredValue)
    CEnum_declaredValuesFromList     -> (IRuntimeModule "CEnum",  'CEnum.declaredValuesFromList)
    CEnum_showsCEnum                 -> (IRuntimeModule "CEnum",  'CEnum.shows)
    CEnum_showsWrappedUndeclared     -> (IRuntimeModule "CEnum",  'CEnum.showsWrappedUndeclared)
    CEnum_readPrecCEnum              -> (IRuntimeModule "CEnum",  'CEnum.readPrec)
    CEnum_readPrecWrappedUndeclared  -> (IRuntimeModule "CEnum",  'CEnum.readPrecWrappedUndeclared)
    CEnum_seqIsDeclared              -> (IRuntimeModule "CEnum",  'CEnum.seqIsDeclared)
    CEnum_seqMkDeclared              -> (IRuntimeModule "CEnum",  'CEnum.seqMkDeclared)
    AsCEnum_type                     -> (IRuntimeModule "CEnum", ''CEnum.AsCEnum)
    AsSequentialCEnum_type           -> (IRuntimeModule "CEnum", ''CEnum.AsSequentialCEnum)

    -- Arrays
    ByteArray_type      -> (IRuntimeInternalPrelude,  ''RIP.ByteArray)
    SizedByteArray_type -> (IRuntimeInternalPrelude,  ''RIP.SizedByteArray)
    Block_type          -> (IRuntimeModule "Block",   ''Block.Block)

    -- Complex numbers
    ComplexType -> (IRuntimeInternalPrelude, ''RIP.Complex)

    -- C types
    Void_type       -> (IRuntimeInternalPrelude, ''RIP.Void)
    Char_type       -> (IHaskellPrelude,         ''Char)
    Int_type        -> (IHaskellPrelude,         ''Int)
    Double_type     -> (IHaskellPrelude,         ''Double)
    Float_type      -> (IHaskellPrelude,         ''Float)
    Bool_type       -> (IHaskellPrelude,         ''Bool)
    Int8_type       -> (IRuntimeInternalPrelude, ''RIP.Int8)
    Int16_type      -> (IRuntimeInternalPrelude, ''RIP.Int16)
    Int32_type      -> (IRuntimeInternalPrelude, ''RIP.Int32)
    Int64_type      -> (IRuntimeInternalPrelude, ''RIP.Int64)
    Word_type       -> (IHaskellPrelude,         ''Word)
    Word8_type      -> (IRuntimeInternalPrelude, ''RIP.Word8)
    Word16_type     -> (IRuntimeInternalPrelude, ''RIP.Word16)
    Word32_type     -> (IRuntimeInternalPrelude, ''RIP.Word32)
    Word64_type     -> (IRuntimeInternalPrelude, ''RIP.Word64)
    CChar_type      -> (IRuntimeInternalPrelude, ''RIP.CChar)
    CSChar_type     -> (IRuntimeInternalPrelude, ''RIP.CSChar)
    CUChar_type     -> (IRuntimeInternalPrelude, ''RIP.CUChar)
    CShort_type     -> (IRuntimeInternalPrelude, ''RIP.CShort)
    CUShort_type    -> (IRuntimeInternalPrelude, ''RIP.CUShort)
    CInt_type       -> (IRuntimeInternalPrelude, ''RIP.CInt)
    CUInt_type      -> (IRuntimeInternalPrelude, ''RIP.CUInt)
    CLong_type      -> (IRuntimeInternalPrelude, ''RIP.CLong)
    CULong_type     -> (IRuntimeInternalPrelude, ''RIP.CULong)
    CLLong_type     -> (IRuntimeInternalPrelude, ''RIP.CLLong)
    CULLong_type    -> (IRuntimeInternalPrelude, ''RIP.CULLong)
    CBool_type      -> (IRuntimeInternalPrelude, ''RIP.CBool)
    CFloat_type     -> (IRuntimeInternalPrelude, ''RIP.CFloat)
    CDouble_type    -> (IRuntimeInternalPrelude, ''RIP.CDouble)
    CStringLen_type -> (IRuntimeInternalPrelude, ''RIP.CStringLen)
    CPtrdiff_type   -> (IRuntimeInternalPrelude, ''RIP.CPtrdiff)

data CExprGlobal =
    Not_class
  | Not_not
  | Logical_class
  | Logical_and
  | Logical_or
  | RelEq_class
  | RelEq_eq
  | RelEq_uneq
  | RelOrd_class
  | RelOrd_lt
  | RelOrd_le
  | RelOrd_gt
  | RelOrd_ge
  | Plus_class
  | Plus_resTyCon
  | Plus_plus
  | Minus_class
  | Minus_resTyCon
  | Minus_negate
  | Add_class
  | Add_resTyCon
  | Add_add
  | Sub_class
  | Sub_resTyCon
  | Sub_minus
  | Mult_class
  | Mult_resTyCon
  | Mult_mult
  | Div_class
  | Div_div
  | Div_resTyCon
  | Rem_class
  | Rem_resTyCon
  | Rem_rem
  | Complement_class
  | Complement_resTyCon
  | Complement_complement
  | Bitwise_class
  | Bitwise_resTyCon
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_class
  | Shift_resTyCon
  | Shift_shiftL
  | Shift_shiftR

cExprGlobal :: CExprGlobal -> Global
cExprGlobal x =
  CustomGlobal (Just $ Hs.QualifiedImport "C.Expr.HostPlatform" Nothing) $ case x of
    Not_class             -> ''C.Expr.HostPlatform.Not
    Not_not               ->  'C.Expr.HostPlatform.not
    Logical_class         -> ''C.Expr.HostPlatform.Logical
    Logical_and           -> '(C.Expr.HostPlatform.&&)
    Logical_or            -> '(C.Expr.HostPlatform.||)
    RelEq_class           -> ''C.Expr.HostPlatform.RelEq
    RelEq_eq              -> '(C.Expr.HostPlatform.==)
    RelEq_uneq            -> '(C.Expr.HostPlatform.!=)
    RelOrd_class          -> ''C.Expr.HostPlatform.RelOrd
    RelOrd_lt             -> '(C.Expr.HostPlatform.<)
    RelOrd_le             -> '(C.Expr.HostPlatform.<=)
    RelOrd_gt             -> '(C.Expr.HostPlatform.>)
    RelOrd_ge             -> '(C.Expr.HostPlatform.>=)
    Plus_class            -> ''C.Expr.HostPlatform.Plus
    Plus_resTyCon         -> ''C.Expr.HostPlatform.PlusRes
    Plus_plus             ->  'C.Expr.HostPlatform.plus
    Minus_class           -> ''C.Expr.HostPlatform.Minus
    Minus_resTyCon        -> ''C.Expr.HostPlatform.MinusRes
    Minus_negate          ->  'C.Expr.HostPlatform.negate
    Add_class             -> ''C.Expr.HostPlatform.Add
    Add_resTyCon          -> ''C.Expr.HostPlatform.AddRes
    Add_add               -> '(C.Expr.HostPlatform.+)
    Sub_class             -> ''C.Expr.HostPlatform.Sub
    Sub_resTyCon          -> ''C.Expr.HostPlatform.SubRes
    Sub_minus             -> '(C.Expr.HostPlatform.-)
    Mult_class            -> ''C.Expr.HostPlatform.Mult
    Mult_resTyCon         -> ''C.Expr.HostPlatform.MultRes
    Mult_mult             -> '(C.Expr.HostPlatform.*)
    Div_class             -> ''C.Expr.HostPlatform.Div
    Div_resTyCon          -> ''C.Expr.HostPlatform.DivRes
    Div_div               -> '(C.Expr.HostPlatform./)
    Rem_class             -> ''C.Expr.HostPlatform.Rem
    Rem_resTyCon          -> ''C.Expr.HostPlatform.RemRes
    Rem_rem               -> '(C.Expr.HostPlatform.%)
    Complement_class      -> ''C.Expr.HostPlatform.Complement
    Complement_resTyCon   -> ''C.Expr.HostPlatform.ComplementRes
    Complement_complement -> '(C.Expr.HostPlatform..~)
    Bitwise_class         -> ''C.Expr.HostPlatform.Bitwise
    Bitwise_resTyCon      -> ''C.Expr.HostPlatform.BitsRes
    Bitwise_and           -> '(C.Expr.HostPlatform..&.)
    Bitwise_or            -> '(C.Expr.HostPlatform..|.)
    Bitwise_xor           -> '(C.Expr.HostPlatform..^.)
    Shift_class           -> ''C.Expr.HostPlatform.Shift
    Shift_resTyCon        -> ''C.Expr.HostPlatform.ShiftRes
    Shift_shiftL          -> '(C.Expr.HostPlatform.<<)
    Shift_shiftR          -> '(C.Expr.HostPlatform.>>)
