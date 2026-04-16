{-# LANGUAGE CApiFFI #-}
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

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/function.h>"
  , "void hs_bindgen_fbc2ec26cd297034 ("
  , "  MyFunction *arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_cf67e2fc00fd28d8 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_269a46f9680e33ed ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_f9bc9d37a12171dd ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_ca21c7e4aaa33a81 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_e71a09bea0aef335 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  , "void hs_bindgen_0fdddb4fac9b77d1 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_bd59a5d308c55504 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (barA)(arg1);"
  , "}"
  , "void hs_bindgen_b016bfc7a4cc0734 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (barB)(arg1);"
  , "}"
  , "void hs_bindgen_5e40e29a3ba3186e ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (barC)(arg1);"
  , "}"
  , "void hs_bindgen_cb058763607fee19 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (barD)(arg1);"
  , "}"
  , "void hs_bindgen_2f3d5b8e2b6dd66f ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (barE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_fbc2ec26cd297034" hs_bindgen_fbc2ec26cd297034_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_foo@
hs_bindgen_fbc2ec26cd297034 ::
     RIP.FunPtr MyFunction
  -> IO ()
hs_bindgen_fbc2ec26cd297034 =
  RIP.fromFFIType hs_bindgen_fbc2ec26cd297034_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
foo ::
     RIP.FunPtr MyFunction
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_fbc2ec26cd297034

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_cf67e2fc00fd28d8" hs_bindgen_cf67e2fc00fd28d8_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooA@
hs_bindgen_cf67e2fc00fd28d8 ::
     RIP.FunPtr A
  -> IO ()
hs_bindgen_cf67e2fc00fd28d8 =
  RIP.fromFFIType hs_bindgen_cf67e2fc00fd28d8_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooA ::
     RIP.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_cf67e2fc00fd28d8

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_269a46f9680e33ed" hs_bindgen_269a46f9680e33ed_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooB@
hs_bindgen_269a46f9680e33ed ::
     RIP.FunPtr B
  -> IO ()
hs_bindgen_269a46f9680e33ed =
  RIP.fromFFIType hs_bindgen_269a46f9680e33ed_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooB ::
     RIP.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_269a46f9680e33ed

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_f9bc9d37a12171dd" hs_bindgen_f9bc9d37a12171dd_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooC@
hs_bindgen_f9bc9d37a12171dd ::
     RIP.FunPtr M.C
  -> IO ()
hs_bindgen_f9bc9d37a12171dd =
  RIP.fromFFIType hs_bindgen_f9bc9d37a12171dd_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooC ::
     RIP.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_f9bc9d37a12171dd

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_ca21c7e4aaa33a81" hs_bindgen_ca21c7e4aaa33a81_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooD@
hs_bindgen_ca21c7e4aaa33a81 ::
     RIP.FunPtr M.D
  -> IO ()
hs_bindgen_ca21c7e4aaa33a81 =
  RIP.fromFFIType hs_bindgen_ca21c7e4aaa33a81_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooD ::
     RIP.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_ca21c7e4aaa33a81

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_e71a09bea0aef335" hs_bindgen_e71a09bea0aef335_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooE@
hs_bindgen_e71a09bea0aef335 ::
     RIP.FunPtr E
  -> IO ()
hs_bindgen_e71a09bea0aef335 =
  RIP.fromFFIType hs_bindgen_e71a09bea0aef335_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooE ::
     RIP.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_e71a09bea0aef335

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_0fdddb4fac9b77d1" hs_bindgen_0fdddb4fac9b77d1_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_bar@
hs_bindgen_0fdddb4fac9b77d1 ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO ()
hs_bindgen_0fdddb4fac9b77d1 =
  RIP.fromFFIType hs_bindgen_0fdddb4fac9b77d1_base

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 40:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
bar ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_0fdddb4fac9b77d1

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barA@
foreign import ccall unsafe "hs_bindgen_bd59a5d308c55504" hs_bindgen_bd59a5d308c55504_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barA@
hs_bindgen_bd59a5d308c55504 ::
     RIP.FunPtr A
  -> IO ()
hs_bindgen_bd59a5d308c55504 =
  RIP.fromFFIType hs_bindgen_bd59a5d308c55504_base

{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 42:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barA ::
     RIP.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_bd59a5d308c55504

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barB@
foreign import ccall unsafe "hs_bindgen_b016bfc7a4cc0734" hs_bindgen_b016bfc7a4cc0734_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barB@
hs_bindgen_b016bfc7a4cc0734 ::
     RIP.FunPtr B
  -> IO ()
hs_bindgen_b016bfc7a4cc0734 =
  RIP.fromFFIType hs_bindgen_b016bfc7a4cc0734_base

{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 43:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barB ::
     RIP.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_b016bfc7a4cc0734

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barC@
foreign import ccall unsafe "hs_bindgen_5e40e29a3ba3186e" hs_bindgen_5e40e29a3ba3186e_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barC@
hs_bindgen_5e40e29a3ba3186e ::
     RIP.FunPtr M.C
  -> IO ()
hs_bindgen_5e40e29a3ba3186e =
  RIP.fromFFIType hs_bindgen_5e40e29a3ba3186e_base

{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 45:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barC ::
     RIP.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_5e40e29a3ba3186e

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barD@
foreign import ccall unsafe "hs_bindgen_cb058763607fee19" hs_bindgen_cb058763607fee19_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barD@
hs_bindgen_cb058763607fee19 ::
     RIP.FunPtr M.D
  -> IO ()
hs_bindgen_cb058763607fee19 =
  RIP.fromFFIType hs_bindgen_cb058763607fee19_base

{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 46:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barD ::
     RIP.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
barD = hs_bindgen_cb058763607fee19

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barE@
foreign import ccall unsafe "hs_bindgen_2f3d5b8e2b6dd66f" hs_bindgen_2f3d5b8e2b6dd66f_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barE@
hs_bindgen_2f3d5b8e2b6dd66f ::
     RIP.FunPtr E
  -> IO ()
hs_bindgen_2f3d5b8e2b6dd66f =
  RIP.fromFFIType hs_bindgen_2f3d5b8e2b6dd66f_base

{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 47:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barE ::
     RIP.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
barE = hs_bindgen_2f3d5b8e2b6dd66f
