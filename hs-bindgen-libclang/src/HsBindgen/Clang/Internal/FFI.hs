-- | Internal utilities for working with the C FFI
module HsBindgen.Clang.Internal.FFI (
    withArrayOrNull
  , withCStrings
  ) where

import Foreign
import Foreign.C

-- | Alternative to 'withArrayLen' that returns a null pointer when the passed
-- array is empty
withArrayOrNull :: Storable a => [a] -> (Ptr a -> Int -> IO r) -> IO r
withArrayOrNull xs k
    | null xs   = k nullPtr 0
    | otherwise =
        let arrayLength = length xs
        in  allocaArray arrayLength $ \ptr -> do
              pokeArray ptr xs
              k ptr arrayLength

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
