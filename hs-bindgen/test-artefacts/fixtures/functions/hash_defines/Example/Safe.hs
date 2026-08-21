{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.hash_defines_feature
    , Example.Safe.hash_defines_empty
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#define MY_FEATURE 1"
  , "#define MY_SIZE 8"
  , "#define MY_EMPTY"
  , "#include <functions/hash_defines.h>"
  , "signed int hs_bindgen_8b3d8c537a5a6361 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (hash_defines_feature)(arg1);"
  , "}"
  , "signed int hs_bindgen_60d52d7fed356c54 (void)"
  , "{"
  , "  return (hash_defines_empty)();"
  , "}"
  ]))

-- __unique:__ @test_functionshash_defines_Example_Safe_hash_defines_feature@
foreign import ccall safe "hs_bindgen_8b3d8c537a5a6361" hs_bindgen_8b3d8c537a5a6361_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_functionshash_defines_Example_Safe_hash_defines_feature@
hs_bindgen_8b3d8c537a5a6361 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_8b3d8c537a5a6361 =
  BG.fromFFIType hs_bindgen_8b3d8c537a5a6361_base

{-| __C declaration:__ @hash_defines_feature@

    __defined at:__ @functions\/hash_defines.h 9:5@

    __exported by:__ @functions\/hash_defines.h@
-}
hash_defines_feature ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
hash_defines_feature = hs_bindgen_8b3d8c537a5a6361

-- __unique:__ @test_functionshash_defines_Example_Safe_hash_defines_empty@
foreign import ccall safe "hs_bindgen_60d52d7fed356c54" hs_bindgen_60d52d7fed356c54_base ::
     IO BG.Int32

-- __unique:__ @test_functionshash_defines_Example_Safe_hash_defines_empty@
hs_bindgen_60d52d7fed356c54 :: IO BG.CInt
hs_bindgen_60d52d7fed356c54 =
  BG.fromFFIType hs_bindgen_60d52d7fed356c54_base

{-| __C declaration:__ @hash_defines_empty@

    __defined at:__ @functions\/hash_defines.h 20:5@

    __exported by:__ @functions\/hash_defines.h@
-}
hash_defines_empty :: IO BG.CInt
hash_defines_empty = hs_bindgen_60d52d7fed356c54
