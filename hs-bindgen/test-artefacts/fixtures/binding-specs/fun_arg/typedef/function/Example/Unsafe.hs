{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.fooA
    , Example.Unsafe.fooB
    , Example.Unsafe.fooC
    , Example.Unsafe.fooD
    , Example.Unsafe.fooE
    , Example.Unsafe.bar
    , Example.Unsafe.barA
    , Example.Unsafe.barB
    , Example.Unsafe.barC
    , Example.Unsafe.barD
    , Example.Unsafe.barE
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/function.h>"
  , "void hs_bindgen_51195acecf6b880e ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_eb4a794c10ca94f9 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_a11eb4471a953b64 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_1dea92b3f3346311 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_dba6d56e1a316e2d ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_23b6f403a534aeae ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  , "void hs_bindgen_44994e62548d5196 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_012af7936cfc5b63 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (barA)(arg1);"
  , "}"
  , "void hs_bindgen_67ecffff9d788c07 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (barB)(arg1);"
  , "}"
  , "void hs_bindgen_e8b5ec0471fff356 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (barC)(arg1);"
  , "}"
  , "void hs_bindgen_98ea214fd1feaaca ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (barD)(arg1);"
  , "}"
  , "void hs_bindgen_f145043f481aed3c ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (barE)(arg1);"
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

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
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
     BG.FunPtr A
  -> IO ()
hs_bindgen_eb4a794c10ca94f9 =
  BG.fromFFIType hs_bindgen_eb4a794c10ca94f9_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooA ::
     BG.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_eb4a794c10ca94f9

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_a11eb4471a953b64" hs_bindgen_a11eb4471a953b64_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooB@
hs_bindgen_a11eb4471a953b64 ::
     BG.FunPtr B
  -> IO ()
hs_bindgen_a11eb4471a953b64 =
  BG.fromFFIType hs_bindgen_a11eb4471a953b64_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooB ::
     BG.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_a11eb4471a953b64

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_1dea92b3f3346311" hs_bindgen_1dea92b3f3346311_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooC@
hs_bindgen_1dea92b3f3346311 ::
     BG.FunPtr M.C
  -> IO ()
hs_bindgen_1dea92b3f3346311 =
  BG.fromFFIType hs_bindgen_1dea92b3f3346311_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooC ::
     BG.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_1dea92b3f3346311

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_dba6d56e1a316e2d" hs_bindgen_dba6d56e1a316e2d_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooD@
hs_bindgen_dba6d56e1a316e2d ::
     BG.FunPtr M.D
  -> IO ()
hs_bindgen_dba6d56e1a316e2d =
  BG.fromFFIType hs_bindgen_dba6d56e1a316e2d_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooD ::
     BG.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_dba6d56e1a316e2d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_23b6f403a534aeae" hs_bindgen_23b6f403a534aeae_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooE@
hs_bindgen_23b6f403a534aeae ::
     BG.FunPtr E
  -> IO ()
hs_bindgen_23b6f403a534aeae =
  BG.fromFFIType hs_bindgen_23b6f403a534aeae_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooE ::
     BG.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_23b6f403a534aeae

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_44994e62548d5196" hs_bindgen_44994e62548d5196_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_bar@
hs_bindgen_44994e62548d5196 ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO ()
hs_bindgen_44994e62548d5196 =
  BG.fromFFIType hs_bindgen_44994e62548d5196_base

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 28:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
bar ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_44994e62548d5196

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barA@
foreign import ccall unsafe "hs_bindgen_012af7936cfc5b63" hs_bindgen_012af7936cfc5b63_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barA@
hs_bindgen_012af7936cfc5b63 ::
     BG.FunPtr A
  -> IO ()
hs_bindgen_012af7936cfc5b63 =
  BG.fromFFIType hs_bindgen_012af7936cfc5b63_base

{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 30:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barA ::
     BG.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_012af7936cfc5b63

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barB@
foreign import ccall unsafe "hs_bindgen_67ecffff9d788c07" hs_bindgen_67ecffff9d788c07_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barB@
hs_bindgen_67ecffff9d788c07 ::
     BG.FunPtr B
  -> IO ()
hs_bindgen_67ecffff9d788c07 =
  BG.fromFFIType hs_bindgen_67ecffff9d788c07_base

{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 31:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barB ::
     BG.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_67ecffff9d788c07

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barC@
foreign import ccall unsafe "hs_bindgen_e8b5ec0471fff356" hs_bindgen_e8b5ec0471fff356_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barC@
hs_bindgen_e8b5ec0471fff356 ::
     BG.FunPtr M.C
  -> IO ()
hs_bindgen_e8b5ec0471fff356 =
  BG.fromFFIType hs_bindgen_e8b5ec0471fff356_base

{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barC ::
     BG.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_e8b5ec0471fff356

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barD@
foreign import ccall unsafe "hs_bindgen_98ea214fd1feaaca" hs_bindgen_98ea214fd1feaaca_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barD@
hs_bindgen_98ea214fd1feaaca ::
     BG.FunPtr M.D
  -> IO ()
hs_bindgen_98ea214fd1feaaca =
  BG.fromFFIType hs_bindgen_98ea214fd1feaaca_base

{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barD ::
     BG.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
barD = hs_bindgen_98ea214fd1feaaca

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barE@
foreign import ccall unsafe "hs_bindgen_f145043f481aed3c" hs_bindgen_f145043f481aed3c_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_barE@
hs_bindgen_f145043f481aed3c ::
     BG.FunPtr E
  -> IO ()
hs_bindgen_f145043f481aed3c =
  BG.fromFFIType hs_bindgen_f145043f481aed3c_base

{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barE ::
     BG.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
barE = hs_bindgen_f145043f481aed3c
