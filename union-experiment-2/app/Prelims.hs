module Prelims (
    -- 'StaticSize'
    StaticSize(..)
  , staticAlloca
    -- 'WriteRaw'
  , WriteRaw(..)
  , writeRawOff
  , withWritten
    -- 'ReadRaw'
  , ReadRaw(..)
  , readRawOff
    -- * Deriving-via support
  , FromStorable(..)
  ) where

import Data.Proxy
import Foreign

{-------------------------------------------------------------------------------
  'StaticSize'
-------------------------------------------------------------------------------}

class StaticSize a where
  staticSizeOf    :: Proxy a -> Int
  staticAlignment :: Proxy a -> Int

staticAlloca :: forall a b. StaticSize a => (Ptr a -> IO b) -> IO b
staticAlloca =
    allocaBytesAligned
      (staticSizeOf    (Proxy @a))
      (staticAlignment (Proxy @a))

{-------------------------------------------------------------------------------
  'WriteRaw'
-------------------------------------------------------------------------------}

class WriteRaw a where
  writeRaw :: Ptr a -> a -> IO ()

writeRawOff :: WriteRaw a => Ptr b -> Int -> a -> IO ()
writeRawOff addr off = writeRaw (addr `plusPtr` off)

withWritten :: (WriteRaw a, StaticSize a) => a -> (Ptr a -> IO b) -> IO b
withWritten a k = staticAlloca $ \ptr -> writeRaw ptr a >> k ptr

{-------------------------------------------------------------------------------
  'ReadRaw'
-------------------------------------------------------------------------------}

class ReadRaw a where
  readRaw :: Ptr a -> IO a

readRawOff :: ReadRaw a => Ptr a -> Int -> IO a
readRawOff addr off = readRaw (addr `plusPtr` off)

{-------------------------------------------------------------------------------
  Deriving-via

  Mostly just to check signatures, but also potentially useful.
-------------------------------------------------------------------------------}

newtype FromStorable a = WrapFromStorable { unwrapFromStorable :: a }
newtype ToStorable   a = WrapToStorable   { unwrapToStorable   :: a }

instance Storable a => StaticSize (FromStorable a) where
  staticSizeOf    _ = sizeOf    (undefined :: a)
  staticAlignment _ = alignment (undefined :: a)

instance Storable a => WriteRaw (FromStorable a) where
  writeRaw ptr = poke (castPtr ptr) . unwrapFromStorable

instance Storable a => ReadRaw (FromStorable a) where
  readRaw ptr = WrapFromStorable <$> peek (castPtr ptr)

instance (StaticSize a, WriteRaw a, ReadRaw a) => Storable (ToStorable a) where
  sizeOf    _ = staticSizeOf    (Proxy @a)
  alignment _ = staticAlignment (Proxy @a)

  poke ptr = writeRaw (castPtr ptr) . unwrapToStorable
  peek ptr = WrapToStorable <$> readRaw (castPtr ptr)
