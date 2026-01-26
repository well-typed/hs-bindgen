{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/macro/struct.h>"
  , "void hs_bindgen_f2a9c7d0ba1aaa3b ("
  , "  struct MyStruct *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_d7efef1db7e6b005 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_e49c2e985e471c99 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_f2a9c7d0ba1aaa3b" hs_bindgen_f2a9c7d0ba1aaa3b_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_foo@
hs_bindgen_f2a9c7d0ba1aaa3b ::
     Ptr.Ptr MyStruct
  -> IO ()
hs_bindgen_f2a9c7d0ba1aaa3b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f2a9c7d0ba1aaa3b_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
foo ::
     MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    F.with x0 (\x1 -> hs_bindgen_f2a9c7d0ba1aaa3b x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_d7efef1db7e6b005" hs_bindgen_d7efef1db7e6b005_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooA@
hs_bindgen_d7efef1db7e6b005 ::
     Ptr.Ptr A
  -> IO ()
hs_bindgen_d7efef1db7e6b005 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d7efef1db7e6b005_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    F.with x0 (\x1 -> hs_bindgen_d7efef1db7e6b005 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_e49c2e985e471c99" hs_bindgen_e49c2e985e471c99_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooB@
hs_bindgen_e49c2e985e471c99 ::
     Ptr.Ptr B
  -> IO ()
hs_bindgen_e49c2e985e471c99 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e49c2e985e471c99_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    F.with x0 (\x1 -> hs_bindgen_e49c2e985e471c99 x1)
