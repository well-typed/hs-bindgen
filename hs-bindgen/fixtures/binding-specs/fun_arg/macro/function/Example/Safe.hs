{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/macro/function.h>"
  , "void hs_bindgen_40e15e86e5db36ce ("
  , "  MyFunction *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_e13a57fd1d27f6e6 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_fd490df5087893ae ("
  , "  B arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_a708e95f35bff290 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_2991dd76f4337b78 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  barA(arg1);"
  , "}"
  , "void hs_bindgen_104de3f97206bd1d ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  barB(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_40e15e86e5db36ce" hs_bindgen_40e15e86e5db36ce ::
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
foo = hs_bindgen_40e15e86e5db36ce

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_e13a57fd1d27f6e6" hs_bindgen_e13a57fd1d27f6e6 ::
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
fooA = hs_bindgen_e13a57fd1d27f6e6

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_fd490df5087893ae" hs_bindgen_fd490df5087893ae ::
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
fooB = hs_bindgen_fd490df5087893ae

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_a708e95f35bff290" hs_bindgen_a708e95f35bff290 ::
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
bar = hs_bindgen_a708e95f35bff290

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barA@
foreign import ccall safe "hs_bindgen_2991dd76f4337b78" hs_bindgen_2991dd76f4337b78 ::
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
barA = hs_bindgen_2991dd76f4337b78

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barB@
foreign import ccall safe "hs_bindgen_104de3f97206bd1d" hs_bindgen_104de3f97206bd1d ::
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
barB = hs_bindgen_104de3f97206bd1d
