{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/array_known_size.h>"
  , "void hs_bindgen_51195acecf6b880e ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_eb4a794c10ca94f9 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_a11eb4471a953b64 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  , "void hs_bindgen_1dea92b3f3346311 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(*arg1);"
  , "}"
  , "void hs_bindgen_dba6d56e1a316e2d ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(*arg1);"
  , "}"
  , "void hs_bindgen_23b6f403a534aeae ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_51195acecf6b880e" hs_bindgen_51195acecf6b880e_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_foo@
hs_bindgen_51195acecf6b880e ::
     RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
  -> IO ()
hs_bindgen_51195acecf6b880e =
  RIP.fromFFIType hs_bindgen_51195acecf6b880e_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
foo ::
     RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_51195acecf6b880e

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_eb4a794c10ca94f9" hs_bindgen_eb4a794c10ca94f9_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooA@
hs_bindgen_eb4a794c10ca94f9 ::
     RIP.Ptr A
  -> IO ()
hs_bindgen_eb4a794c10ca94f9 =
  RIP.fromFFIType hs_bindgen_eb4a794c10ca94f9_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooA ::
     RIP.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_eb4a794c10ca94f9

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_a11eb4471a953b64" hs_bindgen_a11eb4471a953b64_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooB@
hs_bindgen_a11eb4471a953b64 ::
     RIP.Ptr B
  -> IO ()
hs_bindgen_a11eb4471a953b64 =
  RIP.fromFFIType hs_bindgen_a11eb4471a953b64_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooB ::
     RIP.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_a11eb4471a953b64

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_1dea92b3f3346311" hs_bindgen_1dea92b3f3346311_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooC@
hs_bindgen_1dea92b3f3346311 ::
     RIP.Ptr M.C
  -> IO ()
hs_bindgen_1dea92b3f3346311 =
  RIP.fromFFIType hs_bindgen_1dea92b3f3346311_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooC ::
     RIP.Ptr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_1dea92b3f3346311

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_dba6d56e1a316e2d" hs_bindgen_dba6d56e1a316e2d_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooD@
hs_bindgen_dba6d56e1a316e2d ::
     RIP.Ptr M.D
  -> IO ()
hs_bindgen_dba6d56e1a316e2d =
  RIP.fromFFIType hs_bindgen_dba6d56e1a316e2d_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooD ::
     RIP.Ptr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_dba6d56e1a316e2d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_23b6f403a534aeae" hs_bindgen_23b6f403a534aeae_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooE@
hs_bindgen_23b6f403a534aeae ::
     RIP.Ptr E
  -> IO ()
hs_bindgen_23b6f403a534aeae =
  RIP.fromFFIType hs_bindgen_23b6f403a534aeae_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooE ::
     RIP.Ptr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_23b6f403a534aeae
