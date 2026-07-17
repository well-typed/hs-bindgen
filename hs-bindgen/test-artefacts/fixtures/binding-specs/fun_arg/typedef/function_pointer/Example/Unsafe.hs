{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.fooA
    , Example.Unsafe.fooB
    , Example.Unsafe.fooC
    , Example.Unsafe.fooD
    , Example.Unsafe.fooE
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/function_pointer.h>"
  , "void hs_bindgen_51195acecf6b880e ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_eb4a794c10ca94f9 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_a11eb4471a953b64 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_1dea92b3f3346311 ("
  , "  C arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_dba6d56e1a316e2d ("
  , "  D arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_23b6f403a534aeae ("
  , "  E arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_51195acecf6b880e" hs_bindgen_51195acecf6b880e_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_foo@
hs_bindgen_51195acecf6b880e ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO ()
hs_bindgen_51195acecf6b880e =
  BG.fromFFIType hs_bindgen_51195acecf6b880e_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
foo ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_51195acecf6b880e

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_eb4a794c10ca94f9" hs_bindgen_eb4a794c10ca94f9_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooA@
hs_bindgen_eb4a794c10ca94f9 ::
     A
  -> IO ()
hs_bindgen_eb4a794c10ca94f9 =
  BG.fromFFIType hs_bindgen_eb4a794c10ca94f9_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_eb4a794c10ca94f9

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_a11eb4471a953b64" hs_bindgen_a11eb4471a953b64_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooB@
hs_bindgen_a11eb4471a953b64 ::
     B
  -> IO ()
hs_bindgen_a11eb4471a953b64 =
  BG.fromFFIType hs_bindgen_a11eb4471a953b64_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_a11eb4471a953b64

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_1dea92b3f3346311" hs_bindgen_1dea92b3f3346311_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooC@
hs_bindgen_1dea92b3f3346311 ::
     M.C
  -> IO ()
hs_bindgen_1dea92b3f3346311 =
  BG.fromFFIType hs_bindgen_1dea92b3f3346311_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_1dea92b3f3346311

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_dba6d56e1a316e2d" hs_bindgen_dba6d56e1a316e2d_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooD@
hs_bindgen_dba6d56e1a316e2d ::
     M.D
  -> IO ()
hs_bindgen_dba6d56e1a316e2d =
  BG.fromFFIType hs_bindgen_dba6d56e1a316e2d_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_dba6d56e1a316e2d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_23b6f403a534aeae" hs_bindgen_23b6f403a534aeae_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooE@
hs_bindgen_23b6f403a534aeae ::
     E
  -> IO ()
hs_bindgen_23b6f403a534aeae =
  BG.fromFFIType hs_bindgen_23b6f403a534aeae_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_23b6f403a534aeae
