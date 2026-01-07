{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array.h>"
  , "void hs_bindgen_3d4f2b2a378bc14d ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_5a731c9fa1e2195e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_e4b3faa747f973a7 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_3d4f2b2a378bc14d" hs_bindgen_3d4f2b2a378bc14d ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array.h:3:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
foo ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_3d4f2b2a378bc14d

-- __unique:__ @test_bindingspecsfun_argarray_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_5a731c9fa1e2195e" hs_bindgen_5a731c9fa1e2195e ::
     M1.A
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/array.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
bar ::
     M1.A
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_5a731c9fa1e2195e

-- __unique:__ @test_bindingspecsfun_argarray_Example_Safe_baz@
foreign import ccall safe "hs_bindgen_e4b3faa747f973a7" hs_bindgen_e4b3faa747f973a7 ::
     M2.B
  -> IO ()

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/array.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
baz ::
     M2.B
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_e4b3faa747f973a7
