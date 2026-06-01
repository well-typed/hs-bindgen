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
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import HsBindgen.Runtime.HighLevel.ToHighLevel (OutMarshaller, mkOut, output,
                                                resultPure, toHighLevel)
import HsBindgen.Runtime.HighLevel.ToHighLevel.Marshallers (peekCStringOut)

import Generated.Pcap qualified as Pcap
import Generated.Pcap.Safe qualified as Pcap

-- | Thrown by 'findAllDevNames' when @pcap_findalldevs@ reports failure.
--
data PcapError = PcapError { msg :: String, code :: CInt }
  deriving stock (Show)
instance Exception PcapError

-- | Allocate a @pcap_if_t **@, walk the linked list after the call, free it.
--
peekPcapDeviceNames
  :: OutMarshaller (Ptr (Ptr Pcap.Pcap_if_t)) [String]
peekPcapDeviceNames = mkOut alloca $ \pp -> do
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

-- | Collect the names of all devices visible to libpcap. The status check
-- runs after the spec rather than inside a 'throwOn' because the error
-- buffer lives in a separate output position.
--
findAllDevNames :: IO [String]
findAllDevNames = do
  (names, errMsg, status) <- toHighLevel
    ( output peekPcapDeviceNames
    $ output (peekCStringOut (fromIntegral Pcap.pCAP_ERRBUF_SIZE))
    $ resultPure id
    ) Pcap.pcap_findalldevs
  when (status /= 0) $ throwIO (PcapError errMsg status)
  pure names
