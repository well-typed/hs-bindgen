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

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/function_pointer.h>"
  , "void hs_bindgen_fbc2ec26cd297034 ("
  , "  MyFunctionPointer arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_cf67e2fc00fd28d8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_269a46f9680e33ed ("
  , "  B arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_f9bc9d37a12171dd ("
  , "  C arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_ca21c7e4aaa33a81 ("
  , "  D arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_e71a09bea0aef335 ("
  , "  E arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_fbc2ec26cd297034" hs_bindgen_fbc2ec26cd297034_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_foo@
hs_bindgen_fbc2ec26cd297034 ::
     MyFunctionPointer
  -> IO ()
hs_bindgen_fbc2ec26cd297034 =
  RIP.fromFFIType hs_bindgen_fbc2ec26cd297034_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
foo ::
     MyFunctionPointer
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_fbc2ec26cd297034

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_cf67e2fc00fd28d8" hs_bindgen_cf67e2fc00fd28d8_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooA@
hs_bindgen_cf67e2fc00fd28d8 ::
     A
  -> IO ()
hs_bindgen_cf67e2fc00fd28d8 =
  RIP.fromFFIType hs_bindgen_cf67e2fc00fd28d8_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_cf67e2fc00fd28d8

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_269a46f9680e33ed" hs_bindgen_269a46f9680e33ed_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooB@
hs_bindgen_269a46f9680e33ed ::
     B
  -> IO ()
hs_bindgen_269a46f9680e33ed =
  RIP.fromFFIType hs_bindgen_269a46f9680e33ed_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_269a46f9680e33ed

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_f9bc9d37a12171dd" hs_bindgen_f9bc9d37a12171dd_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooC@
hs_bindgen_f9bc9d37a12171dd ::
     M.C
  -> IO ()
hs_bindgen_f9bc9d37a12171dd =
  RIP.fromFFIType hs_bindgen_f9bc9d37a12171dd_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_f9bc9d37a12171dd

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_ca21c7e4aaa33a81" hs_bindgen_ca21c7e4aaa33a81_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooD@
hs_bindgen_ca21c7e4aaa33a81 ::
     M.D
  -> IO ()
hs_bindgen_ca21c7e4aaa33a81 =
  RIP.fromFFIType hs_bindgen_ca21c7e4aaa33a81_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_ca21c7e4aaa33a81

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_e71a09bea0aef335" hs_bindgen_e71a09bea0aef335_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooE@
hs_bindgen_e71a09bea0aef335 ::
     E
  -> IO ()
hs_bindgen_e71a09bea0aef335 =
  RIP.fromFFIType hs_bindgen_e71a09bea0aef335_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_e71a09bea0aef335
