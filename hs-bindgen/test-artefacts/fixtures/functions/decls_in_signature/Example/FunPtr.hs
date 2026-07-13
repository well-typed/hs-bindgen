{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.normal
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/decls_in_signature.h>"
  , "/* test_functionsdecls_in_signature_Example_get_normal */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f3036965ea57b87f (void)) ("
  , "  struct opaque *arg1,"
  , "  struct outside *arg2,"
  , "  struct outside arg3"
  , ")"
  , "{"
  , "  return &normal;"
  , "}"
  ]))

-- __unique:__ @test_functionsdecls_in_signature_Example_get_normal@
foreign import ccall unsafe "hs_bindgen_f3036965ea57b87f" hs_bindgen_f3036965ea57b87f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsdecls_in_signature_Example_get_normal@
hs_bindgen_f3036965ea57b87f :: IO (BG.FunPtr (BG.Ptr Opaque -> BG.Ptr Outside -> Outside -> IO ()))
hs_bindgen_f3036965ea57b87f =
  BG.fromFFIType hs_bindgen_f3036965ea57b87f_base

{-# NOINLINE normal #-}
{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h 7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal :: BG.FunPtr (BG.Ptr Opaque -> BG.Ptr Outside -> Outside -> IO ())
normal =
  BG.unsafePerformIO hs_bindgen_f3036965ea57b87f
