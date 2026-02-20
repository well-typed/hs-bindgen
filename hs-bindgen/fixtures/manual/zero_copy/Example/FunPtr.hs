{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/zero_copy.h>"
  , "/* test_manualzero_copy_Example_get_reverse */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_44cb8687c63f0086 (void)) ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return &reverse;"
  , "}"
  , "/* test_manualzero_copy_Example_get_transpose */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1bf30b28aefee5bf (void)) ("
  , "  triplet const *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  return &transpose;"
  , "}"
  ]))

-- __unique:__ @test_manualzero_copy_Example_get_reverse@
foreign import ccall unsafe "hs_bindgen_44cb8687c63f0086" hs_bindgen_44cb8687c63f0086_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_manualzero_copy_Example_get_reverse@
hs_bindgen_44cb8687c63f0086 :: IO (RIP.FunPtr ((PtrConst.PtrConst Vector) -> (RIP.Ptr Vector) -> IO RIP.CInt))
hs_bindgen_44cb8687c63f0086 =
  RIP.fromFFIType hs_bindgen_44cb8687c63f0086_base

{-# NOINLINE reverse #-}
{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h 77:5@

    __exported by:__ @manual\/zero_copy.h@
-}
reverse :: RIP.FunPtr ((PtrConst.PtrConst Vector) -> (RIP.Ptr Vector) -> IO RIP.CInt)
reverse =
  RIP.unsafePerformIO hs_bindgen_44cb8687c63f0086

-- __unique:__ @test_manualzero_copy_Example_get_transpose@
foreign import ccall unsafe "hs_bindgen_1bf30b28aefee5bf" hs_bindgen_1bf30b28aefee5bf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_manualzero_copy_Example_get_transpose@
hs_bindgen_1bf30b28aefee5bf :: IO (RIP.FunPtr ((PtrConst.PtrConst (IsA.Elem Matrix)) -> (RIP.Ptr (IsA.Elem Matrix)) -> IO ()))
hs_bindgen_1bf30b28aefee5bf =
  RIP.fromFFIType hs_bindgen_1bf30b28aefee5bf_base

{-# NOINLINE transpose #-}
{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h 85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose :: RIP.FunPtr ((PtrConst.PtrConst (IsA.Elem Matrix)) -> (RIP.Ptr (IsA.Elem Matrix)) -> IO ())
transpose =
  RIP.unsafePerformIO hs_bindgen_1bf30b28aefee5bf
