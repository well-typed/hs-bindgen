{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
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
  , "  matrix const arg1,"
  , "  matrix arg2"
  , ")"
  , "{"
  , "  return &transpose;"
  , "}"
  ]))

-- __unique:__ @test_manualzero_copy_Example_get_reverse@
foreign import ccall unsafe "hs_bindgen_44cb8687c63f0086" hs_bindgen_44cb8687c63f0086_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualzero_copy_Example_get_reverse@
hs_bindgen_44cb8687c63f0086 :: IO (Ptr.FunPtr ((HsBindgen.Runtime.ConstPtr.ConstPtr Vector) -> (Ptr.Ptr Vector) -> IO FC.CInt))
hs_bindgen_44cb8687c63f0086 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_44cb8687c63f0086_base

{-# NOINLINE reverse #-}
{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h 77:5@

    __exported by:__ @manual\/zero_copy.h@
-}
reverse :: Ptr.FunPtr ((HsBindgen.Runtime.ConstPtr.ConstPtr Vector) -> (Ptr.Ptr Vector) -> IO FC.CInt)
reverse =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_44cb8687c63f0086

-- __unique:__ @test_manualzero_copy_Example_get_transpose@
foreign import ccall unsafe "hs_bindgen_1bf30b28aefee5bf" hs_bindgen_1bf30b28aefee5bf_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualzero_copy_Example_get_transpose@
hs_bindgen_1bf30b28aefee5bf :: IO (Ptr.FunPtr (Matrix -> Matrix -> IO ()))
hs_bindgen_1bf30b28aefee5bf =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1bf30b28aefee5bf_base

{-# NOINLINE transpose #-}
{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h 85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose :: Ptr.FunPtr (Matrix -> Matrix -> IO ())
transpose =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1bf30b28aefee5bf
