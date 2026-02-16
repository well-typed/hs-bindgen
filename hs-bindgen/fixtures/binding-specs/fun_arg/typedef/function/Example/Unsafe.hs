{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/function.h>"
  , "void hs_bindgen_51195acecf6b880e ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_eb4a794c10ca94f9 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_a11eb4471a953b64 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_1dea92b3f3346311 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_dba6d56e1a316e2d ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_23b6f403a534aeae ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  , "void hs_bindgen_44994e62548d5196 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_012af7936cfc5b63 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  barA(arg1);"
  , "}"
  , "void hs_bindgen_67ecffff9d788c07 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  barB(arg1);"
  , "}"
  , "void hs_bindgen_e8b5ec0471fff356 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  barC(arg1);"
  , "}"
  , "void hs_bindgen_98ea214fd1feaaca ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  barD(arg1);"
  , "}"
  , "void hs_bindgen_f145043f481aed3c ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  barE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_51195acecf6b880e" hs_bindgen_51195acecf6b880e_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_foo@
hs_bindgen_51195acecf6b880e ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO ()
hs_bindgen_51195acecf6b880e =
  RIP.fromFFIType hs_bindgen_51195acecf6b880e_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
foo ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_51195acecf6b880e

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_eb4a794c10ca94f9" hs_bindgen_eb4a794c10ca94f9_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooA@
hs_bindgen_eb4a794c10ca94f9 ::
     RIP.FunPtr A
  -> IO ()
hs_bindgen_eb4a794c10ca94f9 =
  RIP.fromFFIType hs_bindgen_eb4a794c10ca94f9_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooA ::
     RIP.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_eb4a794c10ca94f9

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_a11eb4471a953b64" hs_bindgen_a11eb4471a953b64_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooB@
hs_bindgen_a11eb4471a953b64 ::
     RIP.FunPtr B
  -> IO ()
hs_bindgen_a11eb4471a953b64 =
  RIP.fromFFIType hs_bindgen_a11eb4471a953b64_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooB ::
     RIP.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_a11eb4471a953b64

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_1dea92b3f3346311" hs_bindgen_1dea92b3f3346311_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooC@
hs_bindgen_1dea92b3f3346311 ::
     RIP.FunPtr M.C
  -> IO ()
hs_bindgen_1dea92b3f3346311 =
  RIP.fromFFIType hs_bindgen_1dea92b3f3346311_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooC ::
     RIP.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_1dea92b3f3346311

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_dba6d56e1a316e2d" hs_bindgen_dba6d56e1a316e2d_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooD@
hs_bindgen_dba6d56e1a316e2d ::
     RIP.FunPtr M.D
  -> IO ()
hs_bindgen_dba6d56e1a316e2d =
  RIP.fromFFIType hs_bindgen_dba6d56e1a316e2d_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooD ::
     RIP.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_dba6d56e1a316e2d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_23b6f403a534aeae" hs_bindgen_23b6f403a534aeae_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooE@
hs_bindgen_23b6f403a534aeae ::
     RIP.FunPtr E
  -> IO ()
hs_bindgen_23b6f403a534aeae =
  RIP.fromFFIType hs_bindgen_23b6f403a534aeae_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooE ::
     RIP.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_23b6f403a534aeae

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_44994e62548d5196" hs_bindgen_44994e62548d5196_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_bar@
hs_bindgen_44994e62548d5196 ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO ()
hs_bindgen_44994e62548d5196 =
  RIP.fromFFIType hs_bindgen_44994e62548d5196_base

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 28:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
bar ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_44994e62548d5196

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barA@
foreign import ccall unsafe "hs_bindgen_012af7936cfc5b63" hs_bindgen_012af7936cfc5b63_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barA@
hs_bindgen_012af7936cfc5b63 ::
     RIP.FunPtr A
  -> IO ()
hs_bindgen_012af7936cfc5b63 =
  RIP.fromFFIType hs_bindgen_012af7936cfc5b63_base

{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 30:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barA ::
     RIP.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_012af7936cfc5b63

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barB@
foreign import ccall unsafe "hs_bindgen_67ecffff9d788c07" hs_bindgen_67ecffff9d788c07_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barB@
hs_bindgen_67ecffff9d788c07 ::
     RIP.FunPtr B
  -> IO ()
hs_bindgen_67ecffff9d788c07 =
  RIP.fromFFIType hs_bindgen_67ecffff9d788c07_base

{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 31:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barB ::
     RIP.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_67ecffff9d788c07

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barC@
foreign import ccall unsafe "hs_bindgen_e8b5ec0471fff356" hs_bindgen_e8b5ec0471fff356_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barC@
hs_bindgen_e8b5ec0471fff356 ::
     RIP.FunPtr M.C
  -> IO ()
hs_bindgen_e8b5ec0471fff356 =
  RIP.fromFFIType hs_bindgen_e8b5ec0471fff356_base

{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barC ::
     RIP.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_e8b5ec0471fff356

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barD@
foreign import ccall unsafe "hs_bindgen_98ea214fd1feaaca" hs_bindgen_98ea214fd1feaaca_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barD@
hs_bindgen_98ea214fd1feaaca ::
     RIP.FunPtr M.D
  -> IO ()
hs_bindgen_98ea214fd1feaaca =
  RIP.fromFFIType hs_bindgen_98ea214fd1feaaca_base

{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barD ::
     RIP.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
barD = hs_bindgen_98ea214fd1feaaca

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barE@
foreign import ccall unsafe "hs_bindgen_f145043f481aed3c" hs_bindgen_f145043f481aed3c_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barE@
hs_bindgen_f145043f481aed3c ::
     RIP.FunPtr E
  -> IO ()
hs_bindgen_f145043f481aed3c =
  RIP.fromFFIType hs_bindgen_f145043f481aed3c_base

{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barE ::
     RIP.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
barE = hs_bindgen_f145043f481aed3c
