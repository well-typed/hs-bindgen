module Main where

import Control.Monad (unless)
import Foreign qualified
import Foreign.C qualified as C

import Generated.Pcap qualified as Pcap
import Generated.Pcap.Safe qualified as Pcap

main :: IO ()
main = do
  putStrLn "List of network devices found:"
  mapM_ (putStrLn . ("  - " ++)) =<< findAllDevNames

findAllDevNames :: IO [String]
findAllDevNames = Foreign.alloca $ \pcapIfTPtrPtr -> do
    Foreign.allocaBytes (fromIntegral Pcap.pCAP_ERRBUF_SIZE) $ \errBuf -> do
      success <- Pcap.pcap_findalldevs pcapIfTPtrPtr errBuf
      unless (success == 0) $ fail "find all devices failed"
    pcapIfTPtr <- Foreign.peek pcapIfTPtrPtr
    devNames <- aux [] pcapIfTPtr
    Pcap.pcap_freealldevs pcapIfTPtr
    return devNames
  where
    aux :: [String] -> Foreign.Ptr Pcap.Pcap_if_t -> IO [String]
    aux acc ptr
      | ptr == Foreign.nullPtr = return $ reverse acc
      | otherwise = do
          pcapIfT <- Foreign.peek ptr
          devName <- C.peekCString $ Pcap.pcap_if_t_name pcapIfT
          aux (devName : acc) (Pcap.pcap_if_t_next pcapIfT)
