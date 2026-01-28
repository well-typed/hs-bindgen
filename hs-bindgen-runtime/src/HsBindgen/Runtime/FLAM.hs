{-# LANGUAGE MagicHash #-}

-- We capitalize module names, but use camelCase/PascalCase in code:
--
-- - in types names:    FlamFoo, FooFlamBar
-- - in variable names: flamFoo, fooFlamBar

-- | Intended for qualified import.
--
-- @
-- import HsBindgen.Runtime.FLAM (WithFlam)
-- import HsBindgen.Runtime.FLAM qualified as FLAM
-- @
module HsBindgen.Runtime.FLAM (
    -- * Definitions
    Offset (..),
    NumElems (..),
    WithFlam (..),
    -- * Exceptions
    FlamLengthMismatch (..),
) where

import Control.Exception (Exception, throwIO)
import Data.Kind (Type)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign (Ptr, Storable)
import Foreign qualified
import GHC.Exts (Proxy#, proxy#)

import HsBindgen.Runtime.Marshal

{-------------------------------------------------------------------------------
  Definitions
-------------------------------------------------------------------------------}

class Offset elem aux | aux -> elem where
  offset :: Proxy# aux -> Int

class Offset elem aux => NumElems elem aux | aux -> elem where
  numElems :: aux -> Int

-- | Data structure with flexible array member
data WithFlam elem aux = WithFlam
    { -- Underlying data structure without
      aux  :: !aux
      -- We use the word "flam" for the flexible array member of the struct.
      -- We use the word "vector" to refer to its Haskell representation (as a
      -- vector).
    , flam :: {-# UNPACK #-} !(VS.Vector elem)
    }
  deriving stock Show

instance
       (Storable aux, Storable elem, NumElems elem aux)
    => ReadRaw (WithFlam elem aux) where
  readRaw = peek

instance
       (Storable aux, Storable elem, NumElems elem aux )
    => WriteRaw (WithFlam elem aux) where
  writeRaw = poke

{-------------------------------------------------------------------------------
  Peek and poke
-------------------------------------------------------------------------------}

-- | Peek structure with flexible array member.
peek :: forall aux elem.
     (Storable aux , Storable elem, NumElems elem aux)
  => Ptr (WithFlam elem aux) -> IO (WithFlam elem aux)
peek ptrStruct = do
    aux <- Foreign.peek (ptrToAux ptrStruct)
    let Size{sizeNumElems, sizeNumBytes} = flamSize aux
    vector <- VSM.unsafeNew sizeNumElems
    Foreign.withForeignPtr (fst (VSM.unsafeToForeignPtr0 vector)) $ \ptrVectorElems -> do
        Foreign.copyBytes ptrVectorElems (ptrToFlam ptrStruct) sizeNumBytes
    vector' <- VS.unsafeFreeze vector
    return (WithFlam aux vector')

-- | Poke structure with flexible array member.
poke :: forall aux elem.
     (Storable aux, Storable elem, NumElems elem aux)
  => Ptr (WithFlam elem aux) -> WithFlam elem aux -> IO ()
poke ptrStruct (WithFlam aux vector)
  | sizeNumElems /= VS.length vector =
      throwIO $ FlamLengthMismatch sizeNumElems (VS.length vector)
  | otherwise = do
      Foreign.poke (ptrToAux ptrStruct) aux
      VS.unsafeWith vector $ \ptrVectorElems -> do
        Foreign.copyBytes (ptrToFlam ptrStruct) ptrVectorElems sizeNumBytes
  where
    Size{sizeNumElems, sizeNumBytes} = flamSize aux

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data FlamLengthMismatch = FlamLengthMismatch {
      flamLengthStruct   :: Int
    , flamLengthProvided :: Int
    }
  deriving stock (Show)

instance Exception FlamLengthMismatch

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

ptrToAux :: Ptr (WithFlam elem aux) -> Ptr aux
ptrToAux = Foreign.castPtr

ptrToFlam :: forall elem aux.
     Offset elem aux
  => Ptr (WithFlam elem aux) -> Ptr elem
ptrToFlam ptrStruct = Foreign.plusPtr ptrStruct (offset (proxy# @aux))

-- Internal.
data Size = Size{
      sizeNumElems :: Int
    , sizeNumBytes    :: Int
    }

flamSize :: forall (elem :: Type) aux.
     (NumElems elem aux, Storable elem)
  => aux -> Size
flamSize aux = Size{
      sizeNumElems
    , sizeNumBytes = sizeNumElems * Foreign.sizeOf (undefined :: elem)
    }
  where sizeNumElems = numElems aux
