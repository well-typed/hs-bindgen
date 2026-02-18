{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/function.h>"
  , "void hs_bindgen_fbc2ec26cd297034 ("
  , "  MyFunction *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_cf67e2fc00fd28d8 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_269a46f9680e33ed ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_0fdddb4fac9b77d1 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_bd59a5d308c55504 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  barA(arg1);"
  , "}"
  , "void hs_bindgen_b016bfc7a4cc0734 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  barB(arg1);"
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

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 5:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 10:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooB ::
     RIP.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_269a46f9680e33ed

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 34:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 36:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 37:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barB ::
     RIP.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_b016bfc7a4cc0734
