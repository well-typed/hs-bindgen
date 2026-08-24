{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.hash_defines_feature
    , Example.Unsafe.hash_defines_empty
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#define MY_FEATURE 1"
  , "#define MY_SIZE 8"
  , "#define MY_EMPTY"
  , "#include <functions/hash_defines.h>"
  , "signed int hs_bindgen_2d6dbb74d32d186a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (hash_defines_feature)(arg1);"
  , "}"
  , "signed int hs_bindgen_ba61b6fc716692e4 (void)"
  , "{"
  , "  return (hash_defines_empty)();"
  , "}"
  ]))

-- __unique:__ @test_functionshash_defines_Example_Unsafe_hash_defines_feature@
foreign import ccall unsafe "hs_bindgen_2d6dbb74d32d186a" hs_bindgen_2d6dbb74d32d186a_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_functionshash_defines_Example_Unsafe_hash_defines_feature@
hs_bindgen_2d6dbb74d32d186a ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_2d6dbb74d32d186a =
  BG.fromFFIType hs_bindgen_2d6dbb74d32d186a_base

{-| __C declaration:__ @hash_defines_feature@

    __defined at:__ @functions\/hash_defines.h 9:5@

    __exported by:__ @functions\/hash_defines.h@
-}
hash_defines_feature ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
hash_defines_feature = hs_bindgen_2d6dbb74d32d186a

-- __unique:__ @test_functionshash_defines_Example_Unsafe_hash_defines_empty@
foreign import ccall unsafe "hs_bindgen_ba61b6fc716692e4" hs_bindgen_ba61b6fc716692e4_base ::
     IO BG.Int32

-- __unique:__ @test_functionshash_defines_Example_Unsafe_hash_defines_empty@
hs_bindgen_ba61b6fc716692e4 :: IO BG.CInt
hs_bindgen_ba61b6fc716692e4 =
  BG.fromFFIType hs_bindgen_ba61b6fc716692e4_base

{-| __C declaration:__ @hash_defines_empty@

    __defined at:__ @functions\/hash_defines.h 20:5@

    __exported by:__ @functions\/hash_defines.h@
-}
hash_defines_empty :: IO BG.CInt
hash_defines_empty = hs_bindgen_ba61b6fc716692e4
