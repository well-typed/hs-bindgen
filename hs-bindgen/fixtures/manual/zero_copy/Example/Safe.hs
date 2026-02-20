{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/zero_copy.h>"
  , "signed int hs_bindgen_350cceac1101d344 ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return (reverse)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2ff371c815d92b04 ("
  , "  matrix const *arg1,"
  , "  matrix *arg2"
  , ")"
  , "{"
  , "  (transpose)(*arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_manualzero_copy_Example_Safe_reverse@
foreign import ccall safe "hs_bindgen_350cceac1101d344" hs_bindgen_350cceac1101d344_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_manualzero_copy_Example_Safe_reverse@
hs_bindgen_350cceac1101d344 ::
     PtrConst.PtrConst Vector
  -> RIP.Ptr Vector
  -> IO RIP.CInt
hs_bindgen_350cceac1101d344 =
  RIP.fromFFIType hs_bindgen_350cceac1101d344_base

{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h 77:5@

    __exported by:__ @manual\/zero_copy.h@
-}
reverse ::
     PtrConst.PtrConst Vector
     -- ^ __C declaration:__ @input@
  -> RIP.Ptr Vector
     -- ^ __C declaration:__ @output@
  -> IO RIP.CInt
reverse = hs_bindgen_350cceac1101d344

-- __unique:__ @test_manualzero_copy_Example_Safe_transpose@
foreign import ccall safe "hs_bindgen_2ff371c815d92b04" hs_bindgen_2ff371c815d92b04_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_manualzero_copy_Example_Safe_transpose@
hs_bindgen_2ff371c815d92b04 ::
     PtrConst.PtrConst Matrix
  -> RIP.Ptr Matrix
  -> IO ()
hs_bindgen_2ff371c815d92b04 =
  RIP.fromFFIType hs_bindgen_2ff371c815d92b04_base

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h 85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose ::
     PtrConst.PtrConst Matrix
     -- ^ __C declaration:__ @input@
  -> RIP.Ptr Matrix
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose = hs_bindgen_2ff371c815d92b04
