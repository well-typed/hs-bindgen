module SomeOtherModule where

import Runtime
import C

foreign import capi "exporttest.h foo"
  foo_hash1234 :: Int -> IO ()

foo :: C -> IO ()
foo c = foo_hash1234 (coerceFFI c)


