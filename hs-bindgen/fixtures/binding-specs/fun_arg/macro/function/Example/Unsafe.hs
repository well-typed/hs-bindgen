{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/macro/function.h>"
  , "void hs_bindgen_fbc2ec26cd297034 ("
  , "  MyFunction *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_cf67e2fc00fd28d8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_269a46f9680e33ed ("
  , "  B arg1"
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
foreign import ccall unsafe "hs_bindgen_fbc2ec26cd297034" hs_bindgen_fbc2ec26cd297034 ::
     Ptr.FunPtr MyFunction
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
foo ::
     Ptr.FunPtr MyFunction
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_fbc2ec26cd297034

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_cf67e2fc00fd28d8" hs_bindgen_cf67e2fc00fd28d8 ::
     A
  -> IO ()

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_cf67e2fc00fd28d8

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_269a46f9680e33ed" hs_bindgen_269a46f9680e33ed ::
     B
  -> IO ()

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_269a46f9680e33ed

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_0fdddb4fac9b77d1" hs_bindgen_0fdddb4fac9b77d1 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 32:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
bar ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_0fdddb4fac9b77d1

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barA@
foreign import ccall unsafe "hs_bindgen_bd59a5d308c55504" hs_bindgen_bd59a5d308c55504 ::
     Ptr.FunPtr A
  -> IO ()

{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barA ::
     Ptr.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_bd59a5d308c55504

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_barB@
foreign import ccall unsafe "hs_bindgen_b016bfc7a4cc0734" hs_bindgen_b016bfc7a4cc0734 ::
     Ptr.FunPtr B
  -> IO ()

{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barB ::
     Ptr.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_b016bfc7a4cc0734
