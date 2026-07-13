{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MagicHash #-}

-- | Definitions required by generated bindings.
--
-- This module ensures compatibility across GHC versions and @base@ library
-- versions. It re-exports most Haskell definitions that are used by the
-- generated bindings. In particular, the order of exports matches the order of
-- constructors of 'HsBindgen.Backend.SHs.Global.BindgenGlobal'.
--
-- See https://github.com/well-typed/hs-bindgen/issues/1627.
--
-- We maintain minimal lists of explicit imports and exports.
--
-- The "HsBindgen.Runtime.Support" module should only re-export definitions
-- intended for unqualified import defined in @hs-bindgen-runtime@ or from other
-- libraries such as @base@ or @containers@.
--
-- For definitions in @hs-bindgen-runtime@ which are intended for qualified
-- import, the generated code will directly import those modules in a qualified
-- way, as intended.
--
-- So far, we have not come across definitions intended for unqualified import
-- defined in other libraries.
--
-- For the full rule on what generated code may import, see the "Imports in
-- generated code" section of @dev\/code-structure.md@.
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
  , plusPtr
  , StablePtr
  , getUnionPayload
  , setUnionPayload
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
  , castFunPtrFromFFIType
  , castFunPtrToFFIType

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
  , readListPrec
  , readListDefault
  , readListPrecDefault

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
  , CChar, CSChar, CUChar, CShort, CUShort
  , CInt, CUInt, CLong, CULong, CLLong, CULLong
  , CBool
  , CFloat(CFloat)
  , CDouble(CDouble)
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
                with)
import Foreign.C (CBool, CChar, CDouble (CDouble), CFloat (CFloat), CInt,
                  CLLong, CLong, CPtrdiff, CSChar, CShort, CUChar, CUInt,
                  CULLong, CULong, CUShort)
import Foreign.C.String (CStringLen)
import GHC.Base ((*#), (+#))
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.Generics (Generic)
import GHC.Ptr (FunPtr, Ptr (Ptr), plusPtr)
import GHC.Records (HasField (getField))
import GHC.Stable (StablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readListDefault, readListPrec, readListPrecDefault, readPrec)

import HsBindgen.Runtime.Support.Bitfield (Bitfield)
import HsBindgen.Runtime.Support.ByteArray (getUnionPayload, setUnionPayload)
import HsBindgen.Runtime.Support.CAPI (allocaAndPeek)
import HsBindgen.Runtime.Support.FunPtr (FromFunPtr (fromFunPtr),
                                         ToFunPtr (toFunPtr))
import HsBindgen.Runtime.Support.HasFFIType (HasFFIType (fromFFIType, toFFIType),
                                             castFunPtrFromFFIType,
                                             castFunPtrToFFIType)
import HsBindgen.Runtime.Support.SizedByteArray (SizedByteArray (SizedByteArray))
