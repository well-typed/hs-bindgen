{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/zero_copy.h>"
  , "signed int hs_bindgen_f9655173d51bbaac ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return (reverse)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ea25667627dd5ed2 ("
  , "  triplet const *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  (transpose)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_manualzero_copy_Example_Unsafe_reverse@
foreign import ccall unsafe "hs_bindgen_f9655173d51bbaac" hs_bindgen_f9655173d51bbaac_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_manualzero_copy_Example_Unsafe_reverse@
hs_bindgen_f9655173d51bbaac ::
     PtrConst.PtrConst Vector
  -> RIP.Ptr Vector
  -> IO RIP.CInt
hs_bindgen_f9655173d51bbaac =
  RIP.fromFFIType hs_bindgen_f9655173d51bbaac_base

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
reverse = hs_bindgen_f9655173d51bbaac

-- __unique:__ @test_manualzero_copy_Example_Unsafe_transpose@
foreign import ccall unsafe "hs_bindgen_ea25667627dd5ed2" hs_bindgen_ea25667627dd5ed2_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_manualzero_copy_Example_Unsafe_transpose@
hs_bindgen_ea25667627dd5ed2 ::
     PtrConst.PtrConst (IsA.Elem Matrix)
  -> RIP.Ptr (IsA.Elem Matrix)
  -> IO ()
hs_bindgen_ea25667627dd5ed2 =
  RIP.fromFFIType hs_bindgen_ea25667627dd5ed2_base

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h 85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose ::
     PtrConst.PtrConst (IsA.Elem Matrix)
     -- ^ __C declaration:__ @input@
  -> RIP.Ptr (IsA.Elem Matrix)
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose = hs_bindgen_ea25667627dd5ed2
