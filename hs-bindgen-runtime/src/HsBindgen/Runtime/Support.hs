{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MagicHash #-}

-- | Support prelude of generated bindings
--
-- Re-exports the definitions that generated code needs from modules meant for
-- unqualified import, be they in @base@, in other libraries, or in
-- @hs-bindgen-runtime@. Generated code imports modules meant for qualified
-- import, such as "HsBindgen.Runtime.Marshal", directly instead, and imports a
-- curated set of "Prelude" names unqualified; see @dev\/generated-code.md@.
--
-- This module also bridges differences between GHC and @base@ versions.
--
-- We maintain minimal lists of explicit imports and exports. Exports are
-- grouped like the constructors of @BindgenGlobalType@ and
-- @BindgenGlobalTerm@ in @HsBindgen.Backend.Global@ (package @hs-bindgen@).
--
-- Intended for qualified import.
--
-- @
-- import HsBindgen.Runtime.Support qualified as BG
-- @
module HsBindgen.Runtime.Support (
    -- * Function pointers
    ToFunPtr(toFunPtr)
  , FromFunPtr(fromFunPtr)

    -- * Foreign function interface
  , Ptr(Ptr)
  , FunPtr
  , StablePtr
  , plusPtr
  , castFunPtr
  , getUnionPayload
  , setUnionPayload
  , getUnionPayloadBits
  , setUnionPayloadBits
  , with
  , allocaAndPeek
  , Generic

    -- * 'Storable'
  , Storable(sizeOf, alignment, peekByteOff, pokeByteOff, peek, poke)

    -- * 'HasField'
  , HasField(getField)

    -- * Proxy
  , Proxy(Proxy)

    -- * 'HasFFIType'
  , HasFFIType(fromFFIType, toFFIType)
  , PtrVoid
  , FunPtrVoid

    -- * Unsafe
  , unsafePerformIO

    -- * Primitive
  , Prim(sizeOf#, alignment#, indexByteArray#, readByteArray#, writeByteArray#, indexOffAddr#, readOffAddr#, writeOffAddr#)
  , (+#)
  , (*#)

    -- * Other type classes
  , Bitfield
  , Bits
  , FiniteBits
  , Ix
  , readPrec
  , readList
  , readListPrec
  , readListDefault
  , readListPrecDefault
  , showsPrec

    -- Floating point numbers
  , castWord32ToFloat
  , castWord64ToDouble
    -- The CFloat and CDouble constructors are exported below, together with
    -- their types.

    -- Non-empty lists
  , NonEmpty((:|))
  , singleton

    -- Arrays
  , ByteArray
  , SizedByteArray(SizedByteArray)

    -- ByteString
  , BS.ByteString
  , BS.pack

    -- Complex numbers
  , Complex

    -- C types
  , Void
  , Int8,  Int16,  Int32,  Int64
  , Word8, Word16, Word32, Word64
  , CChar(CChar), CSChar(CSChar), CUChar(CUChar)
  , CShort(CShort), CUShort(CUShort)
  , CInt(CInt), CUInt(CUInt)
  , CLong(CLong), CULong(CULong)
  , CLLong(CLLong), CULLong(CULLong)
  , CBool(CBool)
  , CFloat(CFloat), CDouble(CDouble)
  , CStringLen
  , CPtrdiff
  ) where

import Data.Array.Byte (ByteArray)
import Data.Bits (Bits, FiniteBits)
import Data.ByteString qualified as BS (ByteString, pack)
import Data.Complex (Complex)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ix (Ix)
import Data.List.NonEmpty (NonEmpty ((:|)), singleton)
import Data.Primitive.Types (Prim (alignment#, indexByteArray#, indexOffAddr#, readByteArray#, readOffAddr#, sizeOf#, writeByteArray#, writeOffAddr#))
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
                castFunPtr, with)
import Foreign.C (CBool (CBool), CChar (CChar), CDouble (CDouble),
                  CFloat (CFloat), CInt (CInt), CLLong (CLLong), CLong (CLong),
                  CPtrdiff, CSChar (CSChar), CShort (CShort), CUChar (CUChar),
                  CUInt (CUInt), CULLong (CULLong), CULong (CULong),
                  CUShort (CUShort))
import Foreign.C.String (CStringLen)
import Foreign.StablePtr (StablePtr)
import GHC.Base ((*#), (+#))
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.Generics (Generic)
import GHC.Ptr (FunPtr, Ptr (Ptr), plusPtr)
import GHC.Records (HasField (getField))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readListDefault, readListPrec, readListPrecDefault, readPrec)

import HsBindgen.Runtime.HasFFIType (FunPtrVoid,
                                     HasFFIType (fromFFIType, toFFIType),
                                     PtrVoid)
import HsBindgen.Runtime.Support.Bitfield (Bitfield)
import HsBindgen.Runtime.Support.ByteArray (getUnionPayload,
                                            getUnionPayloadBits,
                                            setUnionPayload,
                                            setUnionPayloadBits)
import HsBindgen.Runtime.Support.CAPI (allocaAndPeek)
import HsBindgen.Runtime.Support.FunPtr (FromFunPtr (fromFunPtr),
                                         ToFunPtr (toFunPtr))
import HsBindgen.Runtime.Support.SizedByteArray (SizedByteArray (SizedByteArray))
