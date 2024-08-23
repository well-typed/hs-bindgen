-- | Internal utilities for working with the C FFI
module HsBindgen.Clang.Util.FFI (
    withCStrings
  ) where

import Foreign
import Foreign.C

-- | Extension of 'withCString' for multiple CStrings
withCStrings :: [String] -> (Ptr CString -> CInt -> IO r) -> IO r
withCStrings = \args k ->
    allocaArray (length args) $ \arr ->
      go args $ \args' -> do
        pokeArray arr args'
        k arr (fromIntegral $ length args)
  where
    go :: [String] -> ([CString] -> IO r) -> IO r
    go []     k = k []
    go (x:xs) k = withCString x  $ \x'  ->
                  go          xs $ \xs' ->
                  k (x' : xs')

