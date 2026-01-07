{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/function_pointer.h>"
  , "void hs_bindgen_bdd2915ebf9193f3 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_9f3ef1a2ad90e46e ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_a3a04a5f27c73a37 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_bdd2915ebf9193f3" hs_bindgen_bdd2915ebf9193f3 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h:3:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
foo ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_bdd2915ebf9193f3

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_9f3ef1a2ad90e46e" hs_bindgen_9f3ef1a2ad90e46e ::
     Ptr.FunPtr M1.A
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
bar ::
     Ptr.FunPtr M1.A
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_9f3ef1a2ad90e46e

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_a3a04a5f27c73a37" hs_bindgen_a3a04a5f27c73a37 ::
     Ptr.FunPtr M2.B
  -> IO ()

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
baz ::
     Ptr.FunPtr M2.B
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_a3a04a5f27c73a37
