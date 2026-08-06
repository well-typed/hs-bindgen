module Main where

import Control.Exception (AsyncException (UserInterrupt), IOException, catch,
                          finally, throwIO, try)
import Control.Monad (when)
import Data.List (intercalate)
import Data.Word (Word8)
import Foreign qualified
import Foreign.C qualified as C
import Network.Socket (Family (AF_INET), Socket, SocketType (Raw), close,
                       socket)
import Text.Printf (printf)

import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Pcap qualified as Pcap
import Generated.Pcap.Safe qualified as Pcap
import HighLevel (findAllDevNames)

main :: IO ()
main = do
  putStrLn "List of network devices found:"
  devNames <- findAllDevNames
  mapM_ (putStrLn . ("  - " ++)) devNames
  canCapture <- canCapturePackets
  case devNames of
    [] -> putStrLn "No network devices found; not capturing."
    (devName : _)
      | not canCapture ->
          putStrLn "\nNo permission to open raw sockets; skipping packet capture (rerun with sudo)."
      | otherwise -> do
          putStrLn ""
          putStrLn $ "Capturing packets on " ++ devName ++ " (press Ctrl-C to stop)..."
          putStrLn ""
          capturePackets devName

-- | Checks whether the process has permission to open raw network sockets,
-- which live packet capture also requires. This is a cross-platform stand-in
-- for a superuser/capability check.
canCapturePackets :: IO Bool
canCapturePackets = do
  -- Protocol 255 is IPPROTO_RAW; protocol 0 is not a valid raw-socket
  -- protocol and fails with "protocol not supported" regardless of
  -- privilege, which would make this check useless.
  result <- try (socket AF_INET Raw 255) :: IO (Either IOException Socket)
  case result of
    Left _     -> pure False
    Right sock -> do
      close sock
      pure True

-- | Open a device for live capture and print a human-readable summary of
-- every packet until interrupted with Ctrl-C.
capturePackets :: String -> IO ()
capturePackets devName =
  Foreign.allocaBytes (fromIntegral Pcap.pCAP_ERRBUF_SIZE) $ \errBuf -> do
    pcapPtr <-
      C.withCString devName $ \devNameCStr ->
        Pcap.pcap_create (PtrConst.unsafeFromPtr devNameCStr) errBuf
    when (pcapPtr == Foreign.nullPtr) $
      fail . ("pcap_create failed: " ++) =<< C.peekCString errBuf
    (`finally` Pcap.pcap_close pcapPtr) $ do
      _ <- Pcap.pcap_set_snaplen pcapPtr 65535
      _ <- Pcap.pcap_set_promisc pcapPtr 1
      -- Read timeout: how often pcap_next_ex returns control to us even
      -- when no packet has arrived, which is also how often we notice a
      -- Ctrl-C while blocked in the underlying (safe) foreign call.
      _ <- Pcap.pcap_set_timeout pcapPtr 1000

      activateResult <- Pcap.pcap_activate pcapPtr
      when (activateResult < 0) $
        fail . ("pcap_activate failed: " ++) =<< getPcapErr pcapPtr
      when (activateResult > 0) $
        putStrLn . ("Warning: " ++) =<< getPcapErr pcapPtr

      datalink <- Pcap.pcap_datalink pcapPtr

      putStrLn packetTableHeader
      captureLoop pcapPtr datalink 0
        `catch` \e -> case e of
          UserInterrupt -> putStrLn "\nStopping capture..."
          _             -> throwIO e

      printStats pcapPtr

getPcapErr :: Foreign.Ptr Pcap.Pcap_t -> IO String
getPcapErr pcapPtr = C.peekCString =<< Pcap.pcap_geterr pcapPtr

captureLoop :: Foreign.Ptr Pcap.Pcap_t -> C.CInt -> Int -> IO ()
captureLoop pcapPtr datalink count =
  Foreign.alloca $ \headerPtrPtr ->
  Foreign.alloca $ \dataPtrPtr -> do
    result <- Pcap.pcap_next_ex pcapPtr headerPtrPtr dataPtrPtr
    case result of
      1 -> do
        header  <- Foreign.peek =<< Foreign.peek headerPtrPtr
        dataPtr <- Foreign.peek dataPtrPtr
        printPacket (count + 1) datalink header dataPtr
        captureLoop pcapPtr datalink (count + 1)
      0 -> captureLoop pcapPtr datalink count -- read timeout, no packet
      _ -> fail . ("pcap_next_ex failed: " ++) =<< getPcapErr pcapPtr

-- | libpcap's @DLT_EN10MB@, i.e. Ethernet; from @pcap/dlt.h@ (not otherwise
-- exposed by the generated bindings).
ethernetDataLinkType :: C.CInt
ethernetDataLinkType = 1

-- | Column titles for 'printPacket', aligned with its @#%-4d %s  %5d bytes
-- %s@ format.
packetTableHeader :: String
packetTableHeader =
  printf "%-5s %-18s %11s  %s"
    ("#" :: String) ("Timestamp" :: String) ("Bytes" :: String)
    ("Description" :: String)

printPacket ::
     Int
  -> C.CInt
  -> Pcap.Pcap_pkthdr
  -> PtrConst.PtrConst Pcap.U_char
  -> IO ()
printPacket packetNo datalink header dataPtr = do
  let caplen = fromIntegral (Pcap.pcap_pkthdr_caplen header) :: Int
      len    = fromIntegral (Pcap.pcap_pkthdr_len header) :: Int
  bytes <-
    Foreign.peekArray caplen
      (Foreign.castPtr (PtrConst.unsafeToPtr dataPtr) :: Foreign.Ptr Word8)
  printf "#%-4d %s  %5d bytes  %s\n"
    packetNo
    (formatTimestamp (Pcap.pcap_pkthdr_ts header))
    len
    (describePacket datalink bytes)

formatTimestamp :: Pcap.Timeval -> String
formatTimestamp ts =
  printf "%d.%06d"
    (fromIntegral (Pcap.timeval_tv_sec ts)  :: Integer)
    (fromIntegral (Pcap.timeval_tv_usec ts) :: Integer)

-- | A short, human-readable description of a packet. For Ethernet frames
-- this decodes the source/destination MAC addresses and the ethertype; for
-- anything else (or a truncated capture) it falls back to a hex preview, so
-- the output stays readable instead of dumping the full payload.
describePacket :: C.CInt -> [Word8] -> String
describePacket datalink bytes
  | datalink == ethernetDataLinkType, length bytes >= 14 =
      let (dst, rest1) = splitAt 6 bytes
          (src, rest2) = splitAt 6 rest1
          (etype, _)   = splitAt 2 rest2
      in printf "%s -> %s  %s" (formatMac src) (formatMac dst) (etherType etype)
  | otherwise =
      "raw: " ++ hexPreview bytes

formatMac :: [Word8] -> String
formatMac = intercalate ":" . map (printf "%02x")

etherType :: [Word8] -> String
etherType [0x08, 0x00] = "IPv4"
etherType [0x08, 0x06] = "ARP"
etherType [0x86, 0xdd] = "IPv6"
etherType [b0, b1]     = printf "0x%02x%02x" b0 b1
etherType _            = "?"

hexPreview :: [Word8] -> String
hexPreview bytes =
  intercalate " " (map (printf "%02x") (take 16 bytes))
    ++ (if length bytes > 16 then " ..." else "")

printStats :: Foreign.Ptr Pcap.Pcap_t -> IO ()
printStats pcapPtr =
  Foreign.alloca $ \statPtr -> do
    result <- Pcap.pcap_stats pcapPtr statPtr
    when (result == 0) $ do
      stat <- Foreign.peek statPtr
      printf "\n%d packets captured, %d dropped by the kernel\n"
        (fromIntegral (Pcap.pcap_stat_ps_recv stat) :: Integer)
        (fromIntegral (Pcap.pcap_stat_ps_drop stat) :: Integer)
