module Main where

import HighLevel qualified as Pcap

main :: IO ()
main = do
  putStrLn "List of network devices found:"
  mapM_ (putStrLn . ("  - " ++)) =<< Pcap.findAllDevNames
