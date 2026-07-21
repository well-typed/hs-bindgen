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
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import HsBindgen.HighLevel (output, resultIO, toHighLevel)
import HsBindgen.HighLevel.Marshaller (Unmarshaller, unmarshalOutWith)
import HsBindgen.HighLevel.Marshaller.Utils (zeroedCStringOut)

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
-- failure with a non-zero status and writes a message into a separate @char[]@ error
-- buffer.
--
-- Both of the call's pointer arguments are 'output' combinators, so the whole binding
-- is one spec and nothing is allocated around it. The error buffer is the interesting
-- one: libpcap writes it /only/ on failure, so 'zeroedCStringOut' hands C a zeroed
-- buffer and the successful calls read it back as @\"\"@ rather than as whatever was
-- on the stack.
--
-- The 'resultIO' assembler then sees the device names, the message and the status
-- together, so the check and the value it guards are one function and the result is
-- exactly @IO [String]@.
--
findAllDevNames :: IO [String]
findAllDevNames = toHighLevel Pcap.pcap_findalldevs
                $ output peekPcapDeviceNames -- pcap_if_t ** : device names (kept)
                $ output errbufOut           -- char *       : the message, empty on success
                $ resultIO keepNames         -- int          : throw on failure
  where
    errbufOut = zeroedCStringOut (fromIntegral Pcap.pCAP_ERRBUF_SIZE)

    keepNames names errMsg status = do
      when (status /= 0) $ throwIO (PcapError errMsg (fromIntegral status))
      pure names
