-- | Union infrastructure
--
-- This would be part of hs-bindgen-runtime
module UnionInfrastructure (
    -- * Features of 'Storable'
    StaticSize(..)
  , staticAlloca
  , WriteRaw(..)
  , writeRawOff
  , withWritten
    -- * 'StorableInContext'
  , ReadRawWithCtxt(..)
  , readRawWithCtxtOff
    -- * 'StructField'
  , StructField(..)
  ) where

import Foreign
import GHC.TypeLits
import Data.Proxy

{-------------------------------------------------------------------------------
  Subsets of 'Storable'
-------------------------------------------------------------------------------}

class StaticSize a where
  staticSizeOf    :: Proxy a -> Int
  staticAlignment :: Proxy a -> Int

staticAlloca :: forall a b. StaticSize a => (Ptr a -> IO b) -> IO b
staticAlloca =
    allocaBytesAligned
      (staticSizeOf    (Proxy @a))
      (staticAlignment (Proxy @a))

class WriteRaw a where
  writeRaw :: Ptr a -> a -> IO ()

writeRawOff :: WriteRaw a => Ptr b -> Int -> a -> IO ()
writeRawOff addr off = writeRaw (addr `plusPtr` off)

withWritten :: (WriteRaw a, StaticSize a) => a -> (Ptr a -> IO b) -> IO b
withWritten a k = staticAlloca $ \ptr -> writeRaw ptr a >> k ptr

{-------------------------------------------------------------------------------
  'ReadRawWithCtxt'

  One instance of this class would be derived by @hs-bindgen@ (where we are
  given the tag directly). Further instances can be used-defined.
-------------------------------------------------------------------------------}

class ReadRawWithCtxt ctxt a where
  readRawWithCtxt :: ctxt -> Ptr a -> IO a

readRawWithCtxtOff ::
     ReadRawWithCtxt ctxt a
  => ctxt -> Ptr b -> Int -> IO a
readRawWithCtxtOff ctxt addr off = readRawWithCtxt ctxt (addr `plusPtr` off)

{-------------------------------------------------------------------------------
  'StructField' allows to distinguish multiple unions in the (very rare?) case
  where the same union type occurs more than once in the same struct type.
-------------------------------------------------------------------------------}

newtype StructField (field :: Symbol) a = StructField a
