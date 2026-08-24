{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.hash_defines_feature
    , Example.FunPtr.hash_defines_empty
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#define MY_FEATURE 1"
  , "#define MY_SIZE 8"
  , "#define MY_EMPTY"
  , "#include <functions/hash_defines.h>"
  , "/* test_functionshash_defines_1_selec_Example_get_hash_defines_feature */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_50e4783814da01a4 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &hash_defines_feature;"
  , "}"
  , "/* test_functionshash_defines_1_selec_Example_get_hash_defines_empty */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b21d3bcdf79e0d85 (void)) (void)"
  , "{"
  , "  return &hash_defines_empty;"
  , "}"
  ]))

-- __unique:__ @test_functionshash_defines_1_selec_Example_get_hash_defines_feature@
foreign import ccall unsafe "hs_bindgen_50e4783814da01a4" hs_bindgen_50e4783814da01a4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionshash_defines_1_selec_Example_get_hash_defines_feature@
hs_bindgen_50e4783814da01a4 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_50e4783814da01a4 =
  BG.fromFFIType hs_bindgen_50e4783814da01a4_base

{-# NOINLINE hash_defines_feature #-}
{-| __C declaration:__ @hash_defines_feature@

    __defined at:__ @functions\/hash_defines.h 9:5@

    __exported by:__ @functions\/hash_defines.h@
-}
hash_defines_feature :: BG.FunPtr (BG.CInt -> IO BG.CInt)
hash_defines_feature =
  BG.unsafePerformIO hs_bindgen_50e4783814da01a4

-- __unique:__ @test_functionshash_defines_1_selec_Example_get_hash_defines_empty@
foreign import ccall unsafe "hs_bindgen_b21d3bcdf79e0d85" hs_bindgen_b21d3bcdf79e0d85_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionshash_defines_1_selec_Example_get_hash_defines_empty@
hs_bindgen_b21d3bcdf79e0d85 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_b21d3bcdf79e0d85 =
  BG.fromFFIType hs_bindgen_b21d3bcdf79e0d85_base

{-# NOINLINE hash_defines_empty #-}
{-| __C declaration:__ @hash_defines_empty@

    __defined at:__ @functions\/hash_defines.h 20:5@

    __exported by:__ @functions\/hash_defines.h@
-}
hash_defines_empty :: BG.FunPtr (IO BG.CInt)
hash_defines_empty =
  BG.unsafePerformIO hs_bindgen_b21d3bcdf79e0d85
