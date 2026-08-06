{-# LANGUAGE DerivingStrategies #-}

-- | High-level wrappers around the generated libpcap bindings.
--
module HighLevel (
    PcapError (..)
  , findAllDevNames
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Foreign.C.String qualified as C
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import HsBindgen.HighLevel (fixed, output, resultIO, toHighLevel)
import HsBindgen.HighLevel.Marshaller (Unmarshaller, unmarshalOutWith)

import Generated.Pcap qualified as Pcap
import Generated.Pcap.Safe qualified as Pcap

-- | Thrown by 'findAllDevNames' when @pcap_findalldevs@ reports failure.
--
data PcapError = PcapError { msg :: String, code :: Int }
  deriving stock (Show)
instance Exception PcapError

-- | Allocate a @pcap_if_t **@, walk the linked list after the call, free it.
--
peekPcapDeviceNames
  :: Unmarshaller (Ptr (Ptr Pcap.Pcap_if_t)) [String]
peekPcapDeviceNames = unmarshalOutWith alloca $ \pp -> do
  headPtr <- peek pp
  names   <- collect [] headPtr
  Pcap.pcap_freealldevs headPtr
  pure names
  where
    collect acc ptr
      | ptr == nullPtr = pure (reverse acc)
      | otherwise = do
          dev  <- peek ptr
          name <- C.peekCString (Pcap.pcap_if_t_name dev)
          collect (name : acc) (Pcap.pcap_if_t_next dev)

-- | Collect the names of all devices visible to libpcap. @pcap_findalldevs@ signals
-- failure with a non-zero status and writes a message into a separate error buffer.
-- We allocate that buffer ourselves and pin it into the spec with 'fixed', which
-- keeps it out of the wrapper's type while leaving it in scope for the closer. The
-- 'resultIO' assembler then sees the device names and the status together, so the
-- check and the value it guards are one function and the result is exactly
-- @IO [String]@.
--
findAllDevNames :: IO [String]
findAllDevNames =
  allocaBytes (fromIntegral Pcap.pCAP_ERRBUF_SIZE) $ \errbuf ->
    toHighLevel
      ( output peekPcapDeviceNames  -- pcap_if_t ** : device names (kept)
      $ fixed errbuf                -- char *       : caller-allocated errbuf
      $ resultIO (keepNames errbuf) -- int          : throw on failure
      ) Pcap.pcap_findalldevs
  where
    keepNames errbuf names status = do
      when (status /= 0) $ do
        errMsg <- C.peekCString errbuf
        throwIO (PcapError errMsg (fromIntegral status))
      pure names
