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
  , "signed int hs_bindgen_9c64b65744839a8b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (hash_defines_feature)(arg1);"
  , "}"
  , "signed int hs_bindgen_f811fa46f514d285 (void)"
  , "{"
  , "  return (hash_defines_empty)();"
  , "}"
  ]))

-- __unique:__ @test_functionshash_defines_1_selec_Example_Unsafe_hash_defines_feature@
foreign import ccall unsafe "hs_bindgen_9c64b65744839a8b" hs_bindgen_9c64b65744839a8b_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_functionshash_defines_1_selec_Example_Unsafe_hash_defines_feature@
hs_bindgen_9c64b65744839a8b ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_9c64b65744839a8b =
  BG.fromFFIType hs_bindgen_9c64b65744839a8b_base

{-| __C declaration:__ @hash_defines_feature@

    __defined at:__ @functions\/hash_defines.h 8:5@

    __exported by:__ @functions\/hash_defines.h@
-}
hash_defines_feature ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
hash_defines_feature = hs_bindgen_9c64b65744839a8b

-- __unique:__ @test_functionshash_defines_1_selec_Example_Unsafe_hash_defines_empty@
foreign import ccall unsafe "hs_bindgen_f811fa46f514d285" hs_bindgen_f811fa46f514d285_base ::
     IO BG.Int32

-- __unique:__ @test_functionshash_defines_1_selec_Example_Unsafe_hash_defines_empty@
hs_bindgen_f811fa46f514d285 :: IO BG.CInt
hs_bindgen_f811fa46f514d285 =
  BG.fromFFIType hs_bindgen_f811fa46f514d285_base

{-| __C declaration:__ @hash_defines_empty@

    __defined at:__ @functions\/hash_defines.h 19:5@

    __exported by:__ @functions\/hash_defines.h@
-}
hash_defines_empty :: IO BG.CInt
hash_defines_empty = hs_bindgen_f811fa46f514d285
