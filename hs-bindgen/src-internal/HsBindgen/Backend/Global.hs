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

    -- ** Specifc to C expressions
  , CExprGlobalType(..)
  , cExprGlobalType
  , CExprGlobalTerm(..)
  , cExprGlobalTerm
  ) where

import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH

import C.Char qualified
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
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.Marshal qualified as Marshal
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.Backend.Level
import HsBindgen.Errors (panicPure)
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
-- @hs-bindgen@ and the standard C expression lanauge @c-expr-runtime@.
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
    -- | Qualified import from "HsBindgen.Runtime.Internal.Prelude".
  | IRuntimeInternalPrelude
    -- | Qualified import from other modules in @hs-bindgen-runtime@ with a
    --   corresponding abbreviation ("qualified as").
  | IRuntimeModule String
  deriving stock (Eq, Ord, Show)

-- We avoid full Template Haskell name resolution, because we want to depend on
-- the intermediate runtime modules.
bindgenToHsImport :: TH.Name -> BindgenImport -> Hs.Import
bindgenToHsImport n = \case
    IHaskellPrelude ->
      Hs.ImplicitPrelude
    IRuntimeInternalPrelude ->
      Hs.QualifiedImport "HsBindgen.Runtime.Internal.Prelude" (Just "RIP")
    IRuntimeModule as ->
      Hs.QualifiedImport unsafeModuleName (Just as)
  where
    unsafeModuleName :: Hs.ModuleName
    unsafeModuleName = case TH.nameModule n of
      Nothing ->
        panicPure $ "unsafeModuleName: expected name with module: " ++ show n
      Just m  ->
        Hs.ModuleName $ Text.pack m

globalExpr :: (BindgenImport, GlobalCat LvlTerm, TH.Name) -> Global LvlTerm
globalExpr (i, c, n) = CustomGlobal n c (bindgenToHsImport n i)

globalType :: (BindgenImport, TH.Name) -> Global LvlType
globalType (i, n) = CustomGlobal n GTyp (bindgenToHsImport n i)

data BindgenGlobalType =
    -- Foreign function interface
    Foreign_Ptr_type
  | Foreign_FunPtr_type
  | Foreign_StablePtr_type
  | IO_type

      -- Arrays
  | ConstantArray_type
  | IncompleteArray_type
  | IsArray_class

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

    -- Proxy
  | Proxy_type

    -- PtrConst
  | PtrConst_type

    -- C enumerations
  | CEnumZ_type
  | AsCEnum_type
  | AsSequentialCEnum_type

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
  | CStringLen_type
  | CPtrdiff_type
  deriving stock (Eq, Ord, Show)

data BindgenGlobalTerm =
    Applicative_pure
  | Applicative_seq
  | Maybe_just
  | Maybe_nothing
  | Monad_return
  | Monad_seq

    -- Function pointers
  | ToFunPtr_toFunPtr
  | FromFunPtr_fromFunPtr

    -- Foreign function interface
  | Foreign_Ptr_constructor
  | Foreign_plusPtr
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
  | PtrConst_unsafeToPtr
  | PtrConst_peek

    -- Primitive
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
  deriving stock (Eq, Ord, Show)

bindgenGlobalType :: BindgenGlobalType -> Global LvlType
bindgenGlobalType = globalType . \case
    -- Foreign function interface
    Foreign_Ptr_type       -> (IRuntimeInternalPrelude, ''RIP.Ptr)
    Foreign_FunPtr_type    -> (IRuntimeInternalPrelude, ''RIP.FunPtr)
    Foreign_StablePtr_type -> (IRuntimeInternalPrelude, ''RIP.StablePtr)
    IO_type                -> (IHaskellPrelude,         ''IO)

      -- Arrays
    ConstantArray_type     -> (IRuntimeModule "CA",     ''CA.ConstantArray)
    IncompleteArray_type   -> (IRuntimeModule "IA",     ''IA.IncompleteArray)
    IsArray_class          -> (IRuntimeModule "IsA",    ''IsA.IsArray)

    -- EquivStorable
    EquivStorable_type -> (IRuntimeModule "Marshal", ''Marshal.EquivStorable)

    -- Flexible array members
    Flam_WithFlam_type -> (IRuntimeModule "FLAM", ''FLAM.WithFlam)

    -- HasCField
    HasCField_CFieldType -> (IRuntimeModule "HasCField", ''HasCField.CFieldType)

    -- BitfieldPtr
    HasCBitfield_BitfieldPtr_type -> (IRuntimeModule "BitfieldPtr", ''BitfieldPtr.BitfieldPtr)

    -- HasCBitfield
    HasCBitfield_CBitfieldType -> (IRuntimeModule "HasCBitfield", ''HasCBitfield.CBitfieldType)

    -- Proxy
    Proxy_type -> (IRuntimeInternalPrelude, ''RIP.Proxy)

    -- PtrConst
    PtrConst_type -> (IRuntimeModule "PtrConst", ''PtrConst.PtrConst)

    -- C enumerations
    CEnumZ_type            -> (IRuntimeModule "CEnum", ''CEnum.CEnumZ)
    AsCEnum_type           -> (IRuntimeModule "CEnum", ''CEnum.AsCEnum)
    AsSequentialCEnum_type -> (IRuntimeModule "CEnum", ''CEnum.AsSequentialCEnum)

    -- Arrays
    ByteArray_type      -> (IRuntimeInternalPrelude,  ''RIP.ByteArray)
    SizedByteArray_type -> (IRuntimeInternalPrelude,  ''RIP.SizedByteArray)
    Block_type          -> (IRuntimeModule "Block",   ''Block.Block)

    -- Complex numbers
    Complex_type -> (IRuntimeInternalPrelude, ''RIP.Complex)

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

typeClassGlobal :: Inst.TypeClass -> Global LvlType
typeClassGlobal = globalType . \case
    Inst.Bitfield        -> (IRuntimeInternalPrelude,       ''RIP.Bitfield)
    Inst.Bits            -> (IRuntimeInternalPrelude,       ''RIP.Bits)
    Inst.Bounded         -> (IHaskellPrelude,               ''Bounded)
    Inst.CEnum           -> (IRuntimeModule "CEnum",        ''CEnum.CEnum)
    Inst.Enum            -> (IHaskellPrelude,               ''Enum)
    Inst.Eq              -> (IHaskellPrelude,               ''Eq)
    Inst.FiniteBits      -> (IRuntimeInternalPrelude,       ''RIP.FiniteBits)
    Inst.Floating        -> (IHaskellPrelude,               ''Floating)
    Inst.Fractional      -> (IHaskellPrelude,               ''Fractional)
    Inst.FromFunPtr      -> (IRuntimeInternalPrelude,       ''RIP.FromFunPtr)
    Inst.Generic         -> (IRuntimeInternalPrelude,       ''RIP.Generic)
    Inst.HasCBitfield    -> (IRuntimeModule "HasCBitfield", ''HasCBitfield.HasCBitfield)
    Inst.HasCField       -> (IRuntimeModule "HasCField",    ''HasCField.HasCField)
    Inst.HasFFIType      -> (IRuntimeInternalPrelude,       ''RIP.HasFFIType)
    Inst.HasField        -> (IRuntimeInternalPrelude,       ''RIP.HasField)
    Inst.Flam_Offset     -> (IRuntimeModule "FLAM",         ''FLAM.Offset)
    Inst.Integral        -> (IHaskellPrelude,               ''Integral)
    Inst.IsArray         -> (IRuntimeModule "IsA",          ''IsA.IsArray)
    Inst.Ix              -> (IRuntimeInternalPrelude,       ''RIP.Ix)
    Inst.Num             -> (IHaskellPrelude,               ''Num)
    Inst.Ord             -> (IHaskellPrelude,               ''Ord)
    Inst.Prim            -> (IRuntimeInternalPrelude,       ''RIP.Prim)
    Inst.Read            -> (IHaskellPrelude,               ''Read)
    Inst.ReadRaw         -> (IRuntimeModule "Marshal",      ''Marshal.ReadRaw)
    Inst.Real            -> (IHaskellPrelude,               ''Real)
    Inst.RealFloat       -> (IHaskellPrelude,               ''RealFloat)
    Inst.RealFrac        -> (IHaskellPrelude,               ''RealFrac)
    Inst.SequentialCEnum -> (IRuntimeModule "CEnum",        ''CEnum.SequentialCEnum)
    Inst.Show            -> (IHaskellPrelude,               ''Show)
    Inst.StaticSize      -> (IRuntimeModule "Marshal",      ''Marshal.StaticSize)
    Inst.Storable        -> (IRuntimeInternalPrelude,       ''RIP.Storable)
    Inst.ToFunPtr        -> (IRuntimeInternalPrelude,       ''RIP.ToFunPtr)
    Inst.WriteRaw        -> (IRuntimeModule "Marshal",      ''Marshal.WriteRaw)

bindgenGlobalTerm :: BindgenGlobalTerm -> Global LvlTerm
bindgenGlobalTerm = globalExpr . \case
    -- When adding a new global that resolves to a non-qualified identifier, be
    -- sure to reserve the name in "HsBindgen.Backend.Hs.AST.Name".
    Applicative_pure    -> (IHaskellPrelude, GVar, 'pure)
    Applicative_seq     -> (IHaskellPrelude, GVar, '(<*>))
    Maybe_just          -> (IHaskellPrelude, GVar, 'Just)
    Maybe_nothing       -> (IHaskellPrelude, GVar, 'Nothing)
    Monad_return        -> (IHaskellPrelude, GVar, 'return)
    Monad_seq           -> (IHaskellPrelude, GVar, '(>>))

    -- Function pointers
    ToFunPtr_toFunPtr     -> (IRuntimeInternalPrelude, GVar, 'RIP.toFunPtr)
    FromFunPtr_fromFunPtr -> (IRuntimeInternalPrelude, GVar, 'RIP.fromFunPtr)

    -- Foreign function interface
    Foreign_Ptr_constructor   -> (IRuntimeInternalPrelude, GCon, 'RIP.Ptr)
    Foreign_plusPtr           -> (IRuntimeInternalPrelude, GVar, 'RIP.plusPtr)
    ByteArray_getUnionPayload -> (IRuntimeInternalPrelude, GVar, 'RIP.getUnionPayload)
    ByteArray_setUnionPayload -> (IRuntimeInternalPrelude, GVar, 'RIP.setUnionPayload)
    Capi_with                 -> (IRuntimeInternalPrelude, GVar, 'RIP.with)
    Capi_allocaAndPeek        -> (IRuntimeInternalPrelude, GVar, 'RIP.allocaAndPeek)

    -- StaticSize
    StaticSize_staticSizeOf    -> (IRuntimeModule "Marshal", GVar, 'Marshal.staticSizeOf)
    StaticSize_staticAlignment -> (IRuntimeModule "Marshal", GVar, 'Marshal.staticAlignment)

    -- ReadRaw
    ReadRaw_readRaw        -> (IRuntimeModule "Marshal", GVar, 'Marshal.readRaw)
    ReadRaw_readRawByteOff -> (IRuntimeModule "Marshal", GVar, 'Marshal.readRawByteOff)

    -- WriteRaw
    WriteRaw_writeRaw        -> (IRuntimeModule "Marshal", GVar, 'Marshal.writeRaw)
    WriteRaw_writeRawByteOff -> (IRuntimeModule "Marshal", GVar, 'Marshal.writeRawByteOff)

    -- Storable
    Storable_sizeOf      -> (IRuntimeInternalPrelude, GVar, 'RIP.sizeOf)
    Storable_alignment   -> (IRuntimeInternalPrelude, GVar, 'RIP.alignment)
    Storable_peekByteOff -> (IRuntimeInternalPrelude, GVar, 'RIP.peekByteOff)
    Storable_pokeByteOff -> (IRuntimeInternalPrelude, GVar, 'RIP.pokeByteOff)
    Storable_peek        -> (IRuntimeInternalPrelude, GVar, 'RIP.peek)
    Storable_poke        -> (IRuntimeInternalPrelude, GVar, 'RIP.poke)

    -- Flexible array members
    Flam_Offset_offset        -> (IRuntimeModule "FLAM", GVar, 'FLAM.offset)

    -- HasCField
    HasCField_offset#    -> (IRuntimeModule "HasCField", GVar, 'HasCField.offset#)
    HasCField_fromPtr    -> (IRuntimeModule "HasCField", GVar, 'HasCField.fromPtr)
    HasCField_peek       -> (IRuntimeModule "HasCField", GVar, 'HasCField.peek)
    HasCField_poke       -> (IRuntimeModule "HasCField", GVar, 'HasCField.poke)
    HasCField_readRaw    -> (IRuntimeModule "HasCField", GVar, 'HasCField.readRaw)
    HasCField_writeRaw   -> (IRuntimeModule "HasCField", GVar, 'HasCField.writeRaw)

    -- HasCBitfield
    HasCBitfield_bitfieldOffset# -> (IRuntimeModule "HasCBitfield", GVar, 'HasCBitfield.bitfieldOffset#)
    HasCBitfield_bitfieldWidth#  -> (IRuntimeModule "HasCBitfield", GVar, 'HasCBitfield.bitfieldWidth#)
    HasCBitfield_toPtr           -> (IRuntimeModule "HasCBitfield", GVar, 'HasCBitfield.toPtr)
    HasCBitfield_peek            -> (IRuntimeModule "HasCBitfield", GVar, 'HasCBitfield.peek)
    HasCBitfield_poke            -> (IRuntimeModule "HasCBitfield", GVar, 'HasCBitfield.poke)

    -- HasField
    HasField_getField -> (IRuntimeInternalPrelude, GVar, 'RIP.getField)

    -- Proxy
    Proxy_constructor -> (IRuntimeInternalPrelude, GCon, 'RIP.Proxy)

    -- HasFFIType
    HasFFIType_fromFFIType           -> (IRuntimeInternalPrelude, GVar, 'RIP.fromFFIType)
    HasFFIType_toFFIType             -> (IRuntimeInternalPrelude, GVar, 'RIP.toFFIType)
    HasFFIType_castFunPtrFromFFIType -> (IRuntimeInternalPrelude, GVar, 'RIP.castFunPtrFromFFIType)
    HasFFIType_castFunPtrToFFIType   -> (IRuntimeInternalPrelude, GVar, 'RIP.castFunPtrToFFIType)

    -- Functor
    Functor_fmap -> (IHaskellPrelude, GVar, 'fmap)

    -- Unsafe
    IO_unsafePerformIO -> (IRuntimeInternalPrelude, GVar, 'RIP.unsafePerformIO)

    -- PtrConst
    PtrConst_unsafeFromPtr -> (IRuntimeModule "PtrConst", GCon, 'PtrConst.unsafeFromPtr)
    PtrConst_unsafeToPtr   -> (IRuntimeModule "PtrConst", GVar, 'PtrConst.unsafeToPtr)
    PtrConst_peek          -> (IRuntimeModule "PtrConst", GVar, 'PtrConst.peek)

    -- Primitive
    Prim_sizeOf#         -> (IRuntimeInternalPrelude, GVar,  'RIP.sizeOf#)
    Prim_alignment#      -> (IRuntimeInternalPrelude, GVar,  'RIP.alignment#)
    Prim_indexByteArray# -> (IRuntimeInternalPrelude, GVar,  'RIP.indexByteArray#)
    Prim_readByteArray#  -> (IRuntimeInternalPrelude, GVar,  'RIP.readByteArray#)
    Prim_writeByteArray# -> (IRuntimeInternalPrelude, GVar,  'RIP.writeByteArray#)
    Prim_indexOffAddr#   -> (IRuntimeInternalPrelude, GVar,  'RIP.indexOffAddr#)
    Prim_readOffAddr#    -> (IRuntimeInternalPrelude, GVar,  'RIP.readOffAddr#)
    Prim_writeOffAddr#   -> (IRuntimeInternalPrelude, GVar,  'RIP.writeOffAddr#)
    Prim_add#            -> (IRuntimeInternalPrelude, GVar, '(RIP.+#))
    Prim_mul#            -> (IRuntimeInternalPrelude, GVar, '(RIP.*#))

    -- Other type classes
    Read_readPrec            -> (IRuntimeInternalPrelude, GVar,  'RIP.readPrec)
    Read_readList            -> (IHaskellPrelude,         GVar, 'readList)
    Read_readListPrec        -> (IRuntimeInternalPrelude, GVar, 'RIP.readListPrec)
    Read_readListDefault     -> (IRuntimeInternalPrelude, GVar, 'RIP.readListDefault)
    Read_readListPrecDefault -> (IRuntimeInternalPrelude, GVar, 'RIP.readListPrecDefault)
    Show_showsPrec           -> (IHaskellPrelude,         GVar, 'showsPrec)

    -- Floating point numbers
    CFloat_constructor           -> (IRuntimeInternalPrelude, GCon, ''RIP.CFloat)
    CDouble_constructor          -> (IRuntimeInternalPrelude, GCon, ''RIP.CDouble)
    GHC_Float_castWord32ToFloat  -> (IRuntimeInternalPrelude, GVar, 'RIP.castWord32ToFloat)
    GHC_Float_castWord64ToDouble -> (IRuntimeInternalPrelude, GVar, 'RIP.castWord64ToDouble)

    -- Non-empty lists
    NonEmpty_constructor     -> (IRuntimeInternalPrelude, GCon, '(RIP.:|))
    NonEmpty_singleton       -> (IRuntimeInternalPrelude, GVar, 'RIP.singleton)

    -- C enumerations
    CEnum_toCEnum                    -> (IRuntimeModule "CEnum", GVar, 'CEnum.toCEnum)
    CEnum_fromCEnum                  -> (IRuntimeModule "CEnum", GVar, 'CEnum.fromCEnum)
    CEnum_declaredValues             -> (IRuntimeModule "CEnum", GVar, 'CEnum.declaredValues)
    CEnum_showsUndeclared            -> (IRuntimeModule "CEnum", GVar, 'CEnum.showsUndeclared)
    CEnum_readPrecUndeclared         -> (IRuntimeModule "CEnum", GVar, 'CEnum.readPrecUndeclared)
    CEnum_isDeclared                 -> (IRuntimeModule "CEnum", GVar, 'CEnum.isDeclared)
    CEnum_mkDeclared                 -> (IRuntimeModule "CEnum", GVar, 'CEnum.mkDeclared)
    SequentialCEnum_minDeclaredValue -> (IRuntimeModule "CEnum", GVar, 'CEnum.minDeclaredValue)
    SequentialCEnum_maxDeclaredValue -> (IRuntimeModule "CEnum", GVar, 'CEnum.maxDeclaredValue)
    CEnum_declaredValuesFromList     -> (IRuntimeModule "CEnum", GVar, 'CEnum.declaredValuesFromList)
    CEnum_showsCEnum                 -> (IRuntimeModule "CEnum", GVar, 'CEnum.shows)
    CEnum_showsWrappedUndeclared     -> (IRuntimeModule "CEnum", GVar, 'CEnum.showsWrappedUndeclared)
    CEnum_readPrecCEnum              -> (IRuntimeModule "CEnum", GVar, 'CEnum.readPrec)
    CEnum_readPrecWrappedUndeclared  -> (IRuntimeModule "CEnum", GVar, 'CEnum.readPrecWrappedUndeclared)
    CEnum_seqIsDeclared              -> (IRuntimeModule "CEnum", GVar, 'CEnum.seqIsDeclared)
    CEnum_seqMkDeclared              -> (IRuntimeModule "CEnum", GVar, 'CEnum.seqMkDeclared)

{-------------------------------------------------------------------------------
  Globals specific to C expressions
-------------------------------------------------------------------------------}

-- | Import specific to @c-expr-runtime@
--
-- Internal!
data CExprImport =
    -- | Qualified import from "C.Char".
    ICChar
    -- | Qualified import from "C.Expr.HostPlatform".
  | ICExprHostPlatform
  deriving stock (Eq, Ord, Show)

-- We avoid full Template Haskell name resolution, because we want to depend on
-- the intermediate runtime modules.
cExprToHsImport :: CExprImport -> Hs.Import
cExprToHsImport = \case
    ICChar ->
      Hs.QualifiedImport "C.Char" Nothing
    ICExprHostPlatform ->
      Hs.QualifiedImport "C.Expr.HostPlatform" Nothing

data CExprGlobalType =
    CharValue_type
  | Not_class
  | Logical_class
  | RelEq_class
  | RelOrd_class
  | Plus_class
  | Plus_resTyCon
  | Minus_class
  | Minus_resTyCon
  | Add_class
  | Add_resTyCon
  | Sub_class
  | Sub_resTyCon
  | Mult_class
  | Mult_resTyCon
  | Div_class
  | Div_resTyCon
  | Rem_class
  | Rem_resTyCon
  | Complement_class
  | Complement_resTyCon
  | Bitwise_class
  | Bitwise_resTyCon
  | Shift_class
  | Shift_resTyCon

data CExprGlobalTerm =
    CharValue_constructor
  | CharValue_fromAddr
  | Not_not
  | Logical_and
  | Logical_or
  | RelEq_eq
  | RelEq_uneq
  | RelOrd_lt
  | RelOrd_le
  | RelOrd_gt
  | RelOrd_ge
  | Plus_plus
  | Minus_negate
  | Add_add
  | Sub_minus
  | Mult_mult
  | Div_div
  | Rem_rem
  | Complement_complement
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_shiftL
  | Shift_shiftR

cExprGlobalType :: CExprGlobalType -> Global LvlType
cExprGlobalType = aux . \case
    CharValue_type      -> (ICChar,             ''C.Char.CharValue)
    Not_class           -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Not)
    Logical_class       -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Logical)
    RelEq_class         -> (ICExprHostPlatform, ''C.Expr.HostPlatform.RelEq)
    RelOrd_class        -> (ICExprHostPlatform, ''C.Expr.HostPlatform.RelOrd)
    Plus_class          -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Plus)
    Plus_resTyCon       -> (ICExprHostPlatform, ''C.Expr.HostPlatform.PlusRes)
    Minus_class         -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Minus)
    Minus_resTyCon      -> (ICExprHostPlatform, ''C.Expr.HostPlatform.MinusRes)
    Add_class           -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Add)
    Add_resTyCon        -> (ICExprHostPlatform, ''C.Expr.HostPlatform.AddRes)
    Sub_class           -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Sub)
    Sub_resTyCon        -> (ICExprHostPlatform, ''C.Expr.HostPlatform.SubRes)
    Mult_class          -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Mult)
    Mult_resTyCon       -> (ICExprHostPlatform, ''C.Expr.HostPlatform.MultRes)
    Div_class           -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Div)
    Div_resTyCon        -> (ICExprHostPlatform, ''C.Expr.HostPlatform.DivRes)
    Rem_class           -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Rem)
    Rem_resTyCon        -> (ICExprHostPlatform, ''C.Expr.HostPlatform.RemRes)
    Complement_class    -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Complement)
    Complement_resTyCon -> (ICExprHostPlatform, ''C.Expr.HostPlatform.ComplementRes)
    Bitwise_class       -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Bitwise)
    Bitwise_resTyCon    -> (ICExprHostPlatform, ''C.Expr.HostPlatform.BitsRes)
    Shift_class         -> (ICExprHostPlatform, ''C.Expr.HostPlatform.Shift)
    Shift_resTyCon      -> (ICExprHostPlatform, ''C.Expr.HostPlatform.ShiftRes)
  where
    aux :: (CExprImport, TH.Name) -> Global LvlType
    aux (i, n) = CustomGlobal n GTyp (cExprToHsImport i)

cExprGlobalTerm :: CExprGlobalTerm -> Global LvlTerm
cExprGlobalTerm = aux . \case
    CharValue_constructor -> (ICChar,             GCon,  'C.Char.CharValue)
    CharValue_fromAddr    -> (ICChar,             GVar,  'C.Char.charValueFromAddr)
    Not_not               -> (ICExprHostPlatform, GVar,  'C.Expr.HostPlatform.not)
    Logical_and           -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.&&))
    Logical_or            -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.||))
    RelEq_eq              -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.==))
    RelEq_uneq            -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.!=))
    RelOrd_lt             -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.<))
    RelOrd_le             -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.<=))
    RelOrd_gt             -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.>))
    RelOrd_ge             -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.>=))
    Plus_plus             -> (ICExprHostPlatform, GVar,  'C.Expr.HostPlatform.plus)
    Minus_negate          -> (ICExprHostPlatform, GVar,  'C.Expr.HostPlatform.negate)
    Add_add               -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.+))
    Sub_minus             -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.-))
    Mult_mult             -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.*))
    Div_div               -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform./))
    Rem_rem               -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.%))
    Complement_complement -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform..~))
    Bitwise_and           -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform..&.))
    Bitwise_or            -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform..|.))
    Bitwise_xor           -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform..^.))
    Shift_shiftL          -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.<<))
    Shift_shiftR          -> (ICExprHostPlatform, GVar, '(C.Expr.HostPlatform.>>))
  where
    aux :: (CExprImport, GlobalCat LvlTerm, TH.Name) -> Global LvlTerm
    aux (i, c, n) =
      CustomGlobal n c (cExprToHsImport i)
