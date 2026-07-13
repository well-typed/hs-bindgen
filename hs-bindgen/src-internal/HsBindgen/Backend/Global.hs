{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Intended for unqualified import.
module HsBindgen.Backend.Global (
    -- * Global symbols
    Global(..)
  , GlobalCat(..)

    -- ** Specific to @hs-bindgen@
  , BindgenGlobalType(..)
  , bindgenGlobalType
  , typeClassGlobal
  , BindgenGlobalTerm(..)
  , bindgenGlobalTerm
  ) where

import Data.ByteString qualified as BS
import Language.Haskell.TH qualified as TH

import HsBindgen.Runtime.BitfieldPtr qualified as BitfieldPtr
import HsBindgen.Runtime.Block qualified as Block
import HsBindgen.Runtime.CEnum qualified as CEnum
import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.FLAM qualified as FLAM
import HsBindgen.Runtime.HasCBitfield qualified as HasCBitfield
import HsBindgen.Runtime.HasCField qualified as HasCField
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.Marshal qualified as Marshal
import HsBindgen.Runtime.PtrConst qualified as PtrConst
import HsBindgen.Runtime.Support qualified as BG
import HsBindgen.Runtime.Support.CompatHasField qualified as BG.CompatHasField
import HsBindgen.Runtime.Union qualified as Union

import HsBindgen.Backend.Level
import HsBindgen.Backend.Runtime (RuntimeModule)
import HsBindgen.Backend.Runtime qualified as Runtime
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Globals
-------------------------------------------------------------------------------}

-- | We distinguish between globals on the type (@Global LvlType@) and term
--   (@Global LvlTerm@) level.
data GlobalCat (lvl :: Level) where
  GVar :: GlobalCat LvlTerm
  GCon :: GlobalCat LvlTerm
  GTyp :: GlobalCat LvlType

deriving stock instance Eq   (GlobalCat lvl)
deriving stock instance Ord  (GlobalCat lvl)
deriving stock instance Show (GlobalCat lvl)

-- | Global symbol
--
-- We use the 'Level' to distinguish between globals on the type and the term
-- level, see 'GlobalCat'.
--
-- The constructor name 'CustomGlobal' highlights that users can create custom
-- globals. However, we provide a predefined set of globals specific to
-- @hs-bindgen@ and the standard C expression language @c-expr-runtime@.
data Global (lvl :: Level) = CustomGlobal {
    name  :: TH.Name
  , cat   :: GlobalCat lvl
  , imprt :: Hs.Import
  }
  deriving stock (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  Globals specific to @hs-bindgen@
-------------------------------------------------------------------------------}

-- | Import specific to @hs-bindgen@
--
-- Internal!
data BindgenImport =
    -- | Implicit import from "Prelude".
    IHaskellPrelude
    -- | Qualified import from a module in @hs-bindgen-runtime@; the module path
    --   and alias come from 'RN.RuntimeModule'.
  | IRuntime RuntimeModule
  deriving stock (Eq, Ord, Show)

bindgenToHsImport :: BindgenImport -> Hs.Import
bindgenToHsImport = \case
    IHaskellPrelude -> Hs.ImplicitPrelude
    IRuntime rm     -> Runtime.qualifiedImport rm

globalExpr :: (BindgenImport, GlobalCat LvlTerm, TH.Name) -> Global LvlTerm
globalExpr (i, c, n) = CustomGlobal n c (bindgenToHsImport i)

globalType :: (BindgenImport, TH.Name) -> Global LvlType
globalType (i, n) = CustomGlobal n GTyp (bindgenToHsImport i)

data BindgenGlobalType =
    -- Foreign function interface
    Foreign_Ptr_type
  | Foreign_FunPtr_type
  | Foreign_StablePtr_type
  | IO_type

      -- Arrays
  | ConstantArray_type
  | IncompleteArray_type
  | IsArray_Elem

    -- EquivStorable
  | EquivStorable_type

    -- Flexible array members
  | Flam_WithFlam_type

    -- HasCField
  | HasCField_CFieldType

    -- BitfieldPtr
  | HasCBitfield_BitfieldPtr_type

    -- HasCBitfield
  | HasCBitfield_CBitfieldType

    -- PtrConst
  | PtrConst_type

    -- C enumerations
  | CEnumZ_type

    -- Arrays
  | ByteArray_type
  | SizedByteArray_type
  | Block_type

    -- Complex numbers
  | Complex_type

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
  | CPtrdiff_type

    -- ByteString
  | ByteString_type
  deriving stock (Eq, Ord, Show)

data BindgenGlobalTerm =
    Applicative_pure
  | Applicative_seq
  | Monad_return
  | Monad_seq

    -- Function pointers
  | ToFunPtr_toFunPtr
  | FromFunPtr_fromFunPtr

    -- Foreign function interface
  | ByteArray_setUnionPayload
  | ByteArray_getUnionPayload
  | Capi_with
  | Capi_allocaAndPeek

    -- StaticSize
  | StaticSize_staticSizeOf
  | StaticSize_staticAlignment

    -- ReadRaw
  | ReadRaw_readRaw
  | ReadRaw_readRawByteOff

    -- WriteRaw
  | WriteRaw_writeRaw
  | WriteRaw_writeRawByteOff

    -- Storable
  | Storable_sizeOf
  | Storable_alignment
  | Storable_peekByteOff
  | Storable_pokeByteOff
  | Storable_peek
  | Storable_poke

    -- Flexible array members
  | Flam_Offset_offset

    -- HasCField
  | HasCField_offset#
  | HasCField_fromPtr
  | HasCField_peek
  | HasCField_poke
  | HasCField_readRaw
  | HasCField_writeRaw

    -- HasCBitfield
  | HasCBitfield_bitfieldOffset#
  | HasCBitfield_bitfieldWidth#
  | HasCBitfield_toPtr
  | HasCBitfield_peek
  | HasCBitfield_poke

    -- HasField
  | HasField_getField

    -- Compat.HasField
  | HasFieldCompat_hasField

    -- Proxy
  | Proxy_constructor

    -- HasFFIType
  | HasFFIType_fromFFIType
  | HasFFIType_toFFIType
  | HasFFIType_castFunPtrFromFFIType
  | HasFFIType_castFunPtrToFFIType

    -- Functor
  | Functor_fmap

    -- Unsafe
  | IO_unsafePerformIO

    -- PtrConst
  | PtrConst_unsafeFromPtr
  | PtrConst_peek

    -- Other type classes
  | Read_readPrec
  | Read_readList
  | Read_readListPrec
  | Read_readListDefault
  | Read_readListPrecDefault
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
  | CEnum_toCEnum
  | CEnum_fromCEnum
  | CEnum_declaredValues
  | CEnum_showsUndeclared
  | CEnum_readPrecUndeclared
  | CEnum_isDeclared
  | CEnum_mkDeclared
  | SequentialCEnum_minDeclaredValue
  | SequentialCEnum_maxDeclaredValue
  | CEnum_declaredValuesFromList
  | CEnum_showsCEnum
  | CEnum_showsWrappedUndeclared
  | CEnum_readPrecCEnum
  | CEnum_readPrecWrappedUndeclared
  | CEnum_seqIsDeclared
  | CEnum_seqMkDeclared

    -- ByteString
  | ByteString_pack
  deriving stock (Eq, Ord, Show)

bindgenGlobalType :: BindgenGlobalType -> Global LvlType
bindgenGlobalType = globalType . \case
    -- Foreign function interface
    Foreign_Ptr_type       -> (IRuntime Runtime.Support, ''BG.Ptr)
    Foreign_FunPtr_type    -> (IRuntime Runtime.Support, ''BG.FunPtr)
    Foreign_StablePtr_type -> (IRuntime Runtime.Support, ''BG.StablePtr)
    IO_type                -> (IHaskellPrelude,          ''IO)

      -- Arrays
    ConstantArray_type     -> (IRuntime Runtime.ConstantArray,   ''CA.ConstantArray)
    IncompleteArray_type   -> (IRuntime Runtime.IncompleteArray, ''IA.IncompleteArray)
    IsArray_Elem           -> (IRuntime Runtime.IsArray,         ''IsA.Elem)

    -- EquivStorable
    EquivStorable_type -> (IRuntime Runtime.Marshal, ''Marshal.EquivStorable)

    -- Flexible array members
    Flam_WithFlam_type -> (IRuntime Runtime.Flam, ''FLAM.WithFlam)

    -- HasCField
    HasCField_CFieldType -> (IRuntime Runtime.HasCField, ''HasCField.CFieldType)

    -- BitfieldPtr
    HasCBitfield_BitfieldPtr_type -> (IRuntime Runtime.BitfieldPtr, ''BitfieldPtr.BitfieldPtr)

    -- HasCBitfield
    HasCBitfield_CBitfieldType -> (IRuntime Runtime.HasCBitfield, ''HasCBitfield.CBitfieldType)

    -- PtrConst
    PtrConst_type -> (IRuntime Runtime.PtrConst, ''PtrConst.PtrConst)

    -- C enumerations
    CEnumZ_type            -> (IRuntime Runtime.CEnum, ''CEnum.CEnumZ)

    -- Arrays
    ByteArray_type      -> (IRuntime Runtime.Support, ''BG.ByteArray)
    SizedByteArray_type -> (IRuntime Runtime.Support, ''BG.SizedByteArray)
    Block_type          -> (IRuntime Runtime.Block,   ''Block.Block)

    -- Complex numbers
    Complex_type -> (IRuntime Runtime.Support, ''BG.Complex)

    -- C types
    Void_type       -> (IRuntime Runtime.Support, ''BG.Void)
    Char_type       -> (IHaskellPrelude,          ''Char)
    Int_type        -> (IHaskellPrelude,          ''Int)
    Double_type     -> (IHaskellPrelude,          ''Double)
    Float_type      -> (IHaskellPrelude,          ''Float)
    Bool_type       -> (IHaskellPrelude,          ''Bool)
    Int8_type       -> (IRuntime Runtime.Support, ''BG.Int8)
    Int16_type      -> (IRuntime Runtime.Support, ''BG.Int16)
    Int32_type      -> (IRuntime Runtime.Support, ''BG.Int32)
    Int64_type      -> (IRuntime Runtime.Support, ''BG.Int64)
    Word_type       -> (IHaskellPrelude,          ''Word)
    Word8_type      -> (IRuntime Runtime.Support, ''BG.Word8)
    Word16_type     -> (IRuntime Runtime.Support, ''BG.Word16)
    Word32_type     -> (IRuntime Runtime.Support, ''BG.Word32)
    Word64_type     -> (IRuntime Runtime.Support, ''BG.Word64)
    CChar_type      -> (IRuntime Runtime.Support, ''BG.CChar)
    CSChar_type     -> (IRuntime Runtime.Support, ''BG.CSChar)
    CUChar_type     -> (IRuntime Runtime.Support, ''BG.CUChar)
    CShort_type     -> (IRuntime Runtime.Support, ''BG.CShort)
    CUShort_type    -> (IRuntime Runtime.Support, ''BG.CUShort)
    CInt_type       -> (IRuntime Runtime.Support, ''BG.CInt)
    CUInt_type      -> (IRuntime Runtime.Support, ''BG.CUInt)
    CLong_type      -> (IRuntime Runtime.Support, ''BG.CLong)
    CULong_type     -> (IRuntime Runtime.Support, ''BG.CULong)
    CLLong_type     -> (IRuntime Runtime.Support, ''BG.CLLong)
    CULLong_type    -> (IRuntime Runtime.Support, ''BG.CULLong)
    CBool_type      -> (IRuntime Runtime.Support, ''BG.CBool)
    CFloat_type     -> (IRuntime Runtime.Support, ''BG.CFloat)
    CDouble_type    -> (IRuntime Runtime.Support, ''BG.CDouble)
    CPtrdiff_type   -> (IRuntime Runtime.Support, ''BG.CPtrdiff)

    -- ByteString
    ByteString_type -> (IRuntime Runtime.Support, ''BG.ByteString)

typeClassGlobal :: Inst.TypeClass -> Global LvlType
typeClassGlobal = globalType . \case
    Inst.Bitfield        -> (IRuntime Runtime.Support,        ''BG.Bitfield)
    Inst.Bits            -> (IRuntime Runtime.Support,        ''BG.Bits)
    Inst.Bounded         -> (IHaskellPrelude,                 ''Bounded)
    Inst.CEnum           -> (IRuntime Runtime.CEnum,          ''CEnum.CEnum)
    Inst.Enum            -> (IHaskellPrelude,                 ''Enum)
    Inst.Eq              -> (IHaskellPrelude,                 ''Eq)
    Inst.FiniteBits      -> (IRuntime Runtime.Support,        ''BG.FiniteBits)
    Inst.Floating        -> (IHaskellPrelude,                 ''Floating)
    Inst.Fractional      -> (IHaskellPrelude,                 ''Fractional)
    Inst.FromFunPtr      -> (IRuntime Runtime.Support,        ''BG.FromFunPtr)
    Inst.Generic         -> (IRuntime Runtime.Support,        ''BG.Generic)
    Inst.HasCBitfield    -> (IRuntime Runtime.HasCBitfield,   ''HasCBitfield.HasCBitfield)
    Inst.HasCField       -> (IRuntime Runtime.HasCField,      ''HasCField.HasCField)
    Inst.HasFFIType      -> (IRuntime Runtime.Support,        ''BG.HasFFIType)
    Inst.HasField        -> (IRuntime Runtime.Support,        ''BG.HasField)
    Inst.HasFieldCompat  -> (IRuntime Runtime.CompatHasField, ''BG.CompatHasField.HasField)
    Inst.HasFieldPtr     -> (IRuntime Runtime.Support,        ''BG.HasField)
    Inst.Flam_Offset     -> (IRuntime Runtime.Flam,           ''FLAM.Offset)
    Inst.Integral        -> (IHaskellPrelude,                 ''Integral)
    Inst.IsArray         -> (IRuntime Runtime.IsArray,        ''IsA.IsArray)
    Inst.IsUnion         -> (IRuntime Runtime.Union,          ''Union.IsUnion)
    Inst.Ix              -> (IRuntime Runtime.Support,        ''BG.Ix)
    Inst.Num             -> (IHaskellPrelude,                 ''Num)
    Inst.Ord             -> (IHaskellPrelude,                 ''Ord)
    Inst.Prim            -> (IRuntime Runtime.Support,        ''BG.Prim)
    Inst.Read            -> (IHaskellPrelude,                 ''Read)
    Inst.ReadRaw         -> (IRuntime Runtime.Marshal,        ''Marshal.ReadRaw)
    Inst.Real            -> (IHaskellPrelude,                 ''Real)
    Inst.RealFloat       -> (IHaskellPrelude,                 ''RealFloat)
    Inst.RealFrac        -> (IHaskellPrelude,                 ''RealFrac)
    Inst.SequentialCEnum -> (IRuntime Runtime.CEnum,          ''CEnum.SequentialCEnum)
    Inst.Show            -> (IHaskellPrelude,                 ''Show)
    Inst.StaticSize      -> (IRuntime Runtime.Marshal,        ''Marshal.StaticSize)
    Inst.Storable        -> (IRuntime Runtime.Support,        ''BG.Storable)
    Inst.ToFunPtr        -> (IRuntime Runtime.Support,        ''BG.ToFunPtr)
    Inst.WriteRaw        -> (IRuntime Runtime.Marshal,        ''Marshal.WriteRaw)

bindgenGlobalTerm :: BindgenGlobalTerm -> Global LvlTerm
bindgenGlobalTerm = globalExpr . \case
    -- When adding a new global that resolves to a non-qualified identifier, be
    -- sure to reserve the name in "HsBindgen.Backend.Hs.AST.Name".
    Applicative_pure    -> (IHaskellPrelude, GVar, 'pure)
    Applicative_seq     -> (IHaskellPrelude, GVar, '(<*>))
    Monad_return        -> (IHaskellPrelude, GVar, 'return)
    Monad_seq           -> (IHaskellPrelude, GVar, '(>>))

    -- Function pointers
    ToFunPtr_toFunPtr     -> (IRuntime Runtime.Support, GVar, 'BG.toFunPtr)
    FromFunPtr_fromFunPtr -> (IRuntime Runtime.Support, GVar, 'BG.fromFunPtr)

    -- Foreign function interface
    ByteArray_getUnionPayload -> (IRuntime Runtime.Support, GVar, 'BG.getUnionPayload)
    ByteArray_setUnionPayload -> (IRuntime Runtime.Support, GVar, 'BG.setUnionPayload)
    Capi_with                 -> (IRuntime Runtime.Support, GVar, 'BG.with)
    Capi_allocaAndPeek        -> (IRuntime Runtime.Support, GVar, 'BG.allocaAndPeek)

    -- StaticSize
    StaticSize_staticSizeOf    -> (IRuntime Runtime.Marshal, GVar, 'Marshal.staticSizeOf)
    StaticSize_staticAlignment -> (IRuntime Runtime.Marshal, GVar, 'Marshal.staticAlignment)

    -- ReadRaw
    ReadRaw_readRaw        -> (IRuntime Runtime.Marshal, GVar, 'Marshal.readRaw)
    ReadRaw_readRawByteOff -> (IRuntime Runtime.Marshal, GVar, 'Marshal.readRawByteOff)

    -- WriteRaw
    WriteRaw_writeRaw        -> (IRuntime Runtime.Marshal, GVar, 'Marshal.writeRaw)
    WriteRaw_writeRawByteOff -> (IRuntime Runtime.Marshal, GVar, 'Marshal.writeRawByteOff)

    -- Storable
    Storable_sizeOf      -> (IRuntime Runtime.Support, GVar, 'BG.sizeOf)
    Storable_alignment   -> (IRuntime Runtime.Support, GVar, 'BG.alignment)
    Storable_peekByteOff -> (IRuntime Runtime.Support, GVar, 'BG.peekByteOff)
    Storable_pokeByteOff -> (IRuntime Runtime.Support, GVar, 'BG.pokeByteOff)
    Storable_peek        -> (IRuntime Runtime.Support, GVar, 'BG.peek)
    Storable_poke        -> (IRuntime Runtime.Support, GVar, 'BG.poke)

    -- Flexible array members
    Flam_Offset_offset        -> (IRuntime Runtime.Flam, GVar, 'FLAM.offset)

    -- HasCField
    HasCField_offset#    -> (IRuntime Runtime.HasCField, GVar, 'HasCField.offset#)
    HasCField_fromPtr    -> (IRuntime Runtime.HasCField, GVar, 'HasCField.fromPtr)
    HasCField_peek       -> (IRuntime Runtime.HasCField, GVar, 'HasCField.peek)
    HasCField_poke       -> (IRuntime Runtime.HasCField, GVar, 'HasCField.poke)
    HasCField_readRaw    -> (IRuntime Runtime.HasCField, GVar, 'HasCField.readRaw)
    HasCField_writeRaw   -> (IRuntime Runtime.HasCField, GVar, 'HasCField.writeRaw)

    -- HasCBitfield
    HasCBitfield_bitfieldOffset# -> (IRuntime Runtime.HasCBitfield, GVar, 'HasCBitfield.bitfieldOffset#)
    HasCBitfield_bitfieldWidth#  -> (IRuntime Runtime.HasCBitfield, GVar, 'HasCBitfield.bitfieldWidth#)
    HasCBitfield_toPtr           -> (IRuntime Runtime.HasCBitfield, GVar, 'HasCBitfield.toPtr)
    HasCBitfield_peek            -> (IRuntime Runtime.HasCBitfield, GVar, 'HasCBitfield.peek)
    HasCBitfield_poke            -> (IRuntime Runtime.HasCBitfield, GVar, 'HasCBitfield.poke)

    -- HasField
    HasField_getField -> (IRuntime Runtime.Support, GVar, 'BG.getField)

    -- Compat.HasField
    HasFieldCompat_hasField -> (IRuntime Runtime.CompatHasField, GVar, 'BG.CompatHasField.hasField)

    -- Proxy
    Proxy_constructor -> (IRuntime Runtime.Support, GCon, 'BG.Proxy)

    -- HasFFIType
    HasFFIType_fromFFIType           -> (IRuntime Runtime.Support, GVar, 'BG.fromFFIType)
    HasFFIType_toFFIType             -> (IRuntime Runtime.Support, GVar, 'BG.toFFIType)
    HasFFIType_castFunPtrFromFFIType -> (IRuntime Runtime.Support, GVar, 'BG.castFunPtrFromFFIType)
    HasFFIType_castFunPtrToFFIType   -> (IRuntime Runtime.Support, GVar, 'BG.castFunPtrToFFIType)

    -- Functor
    Functor_fmap -> (IHaskellPrelude, GVar, 'fmap)

    -- Unsafe
    IO_unsafePerformIO -> (IRuntime Runtime.Support, GVar, 'BG.unsafePerformIO)

    -- PtrConst
    PtrConst_unsafeFromPtr -> (IRuntime Runtime.PtrConst, GVar, 'PtrConst.unsafeFromPtr)
    PtrConst_peek          -> (IRuntime Runtime.PtrConst, GVar, 'PtrConst.peek)

    -- Other type classes
    Read_readPrec            -> (IRuntime Runtime.Support, GVar, 'BG.readPrec)
    Read_readList            -> (IHaskellPrelude,                 GVar, 'readList)
    Read_readListPrec        -> (IRuntime Runtime.Support, GVar, 'BG.readListPrec)
    Read_readListDefault     -> (IRuntime Runtime.Support, GVar, 'BG.readListDefault)
    Read_readListPrecDefault -> (IRuntime Runtime.Support, GVar, 'BG.readListPrecDefault)
    Show_showsPrec           -> (IHaskellPrelude,                 GVar, 'showsPrec)

    -- Floating point numbers
    CFloat_constructor           -> (IRuntime Runtime.Support, GCon, ''BG.CFloat)
    CDouble_constructor          -> (IRuntime Runtime.Support, GCon, ''BG.CDouble)
    GHC_Float_castWord32ToFloat  -> (IRuntime Runtime.Support, GVar, 'BG.castWord32ToFloat)
    GHC_Float_castWord64ToDouble -> (IRuntime Runtime.Support, GVar, 'BG.castWord64ToDouble)

    -- Non-empty lists
    NonEmpty_constructor     -> (IRuntime Runtime.Support, GCon, '(BG.:|))
    NonEmpty_singleton       -> (IRuntime Runtime.Support, GVar, 'BG.singleton)

    -- C enumerations
    CEnum_toCEnum                    -> (IRuntime Runtime.CEnum, GVar, 'CEnum.toCEnum)
    CEnum_fromCEnum                  -> (IRuntime Runtime.CEnum, GVar, 'CEnum.fromCEnum)
    CEnum_declaredValues             -> (IRuntime Runtime.CEnum, GVar, 'CEnum.declaredValues)
    CEnum_showsUndeclared            -> (IRuntime Runtime.CEnum, GVar, 'CEnum.showsUndeclared)
    CEnum_readPrecUndeclared         -> (IRuntime Runtime.CEnum, GVar, 'CEnum.readPrecUndeclared)
    CEnum_isDeclared                 -> (IRuntime Runtime.CEnum, GVar, 'CEnum.isDeclared)
    CEnum_mkDeclared                 -> (IRuntime Runtime.CEnum, GVar, 'CEnum.mkDeclared)
    SequentialCEnum_minDeclaredValue -> (IRuntime Runtime.CEnum, GVar, 'CEnum.minDeclaredValue)
    SequentialCEnum_maxDeclaredValue -> (IRuntime Runtime.CEnum, GVar, 'CEnum.maxDeclaredValue)
    CEnum_declaredValuesFromList     -> (IRuntime Runtime.CEnum, GVar, 'CEnum.declaredValuesFromList)
    CEnum_showsCEnum                 -> (IRuntime Runtime.CEnum, GVar, 'CEnum.shows)
    CEnum_showsWrappedUndeclared     -> (IRuntime Runtime.CEnum, GVar, 'CEnum.showsWrappedUndeclared)
    CEnum_readPrecCEnum              -> (IRuntime Runtime.CEnum, GVar, 'CEnum.readPrec)
    CEnum_readPrecWrappedUndeclared  -> (IRuntime Runtime.CEnum, GVar, 'CEnum.readPrecWrappedUndeclared)
    CEnum_seqIsDeclared              -> (IRuntime Runtime.CEnum, GVar, 'CEnum.seqIsDeclared)
    CEnum_seqMkDeclared              -> (IRuntime Runtime.CEnum, GVar, 'CEnum.seqMkDeclared)

    -- ByteString
    ByteString_pack -> (IRuntime Runtime.Support, GVar, 'BS.pack)
