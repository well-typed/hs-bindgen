{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.fooA
    , Example.Safe.fooB
    , Example.Safe.fooC
    , Example.Safe.fooD
    , Example.Safe.fooE
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/struct.h>"
  , "void hs_bindgen_f2a9c7d0ba1aaa3b ("
  , "  struct MyStruct *arg1"
  , ")"
  , "{"
  , "  (foo)(*arg1);"
  , "}"
  , "void hs_bindgen_d7efef1db7e6b005 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (fooA)(*arg1);"
  , "}"
  , "void hs_bindgen_e49c2e985e471c99 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (fooB)(*arg1);"
  , "}"
  , "void hs_bindgen_584b4871ddfb93d1 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (fooC)(*arg1);"
  , "}"
  , "void hs_bindgen_6ff72b6fa11fd038 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (fooD)(*arg1);"
  , "}"
  , "void hs_bindgen_9c23f536b3698c65 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (fooE)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_f2a9c7d0ba1aaa3b" hs_bindgen_f2a9c7d0ba1aaa3b_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_foo@
hs_bindgen_f2a9c7d0ba1aaa3b ::
     RIP.Ptr MyStruct
  -> IO ()
hs_bindgen_f2a9c7d0ba1aaa3b =
  RIP.fromFFIType hs_bindgen_f2a9c7d0ba1aaa3b_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 8:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
foo ::
     MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_f2a9c7d0ba1aaa3b x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_d7efef1db7e6b005" hs_bindgen_d7efef1db7e6b005_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooA@
hs_bindgen_d7efef1db7e6b005 ::
     RIP.Ptr A
  -> IO ()
hs_bindgen_d7efef1db7e6b005 =
  RIP.fromFFIType hs_bindgen_d7efef1db7e6b005_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 14:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_d7efef1db7e6b005 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_e49c2e985e471c99" hs_bindgen_e49c2e985e471c99_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooB@
hs_bindgen_e49c2e985e471c99 ::
     RIP.Ptr B
  -> IO ()
hs_bindgen_e49c2e985e471c99 =
  RIP.fromFFIType hs_bindgen_e49c2e985e471c99_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 15:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_e49c2e985e471c99 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_584b4871ddfb93d1" hs_bindgen_584b4871ddfb93d1_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooC@
hs_bindgen_584b4871ddfb93d1 ::
     RIP.Ptr M.C
  -> IO ()
hs_bindgen_584b4871ddfb93d1 =
  RIP.fromFFIType hs_bindgen_584b4871ddfb93d1_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 37:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_584b4871ddfb93d1 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_6ff72b6fa11fd038" hs_bindgen_6ff72b6fa11fd038_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooD@
hs_bindgen_6ff72b6fa11fd038 ::
     RIP.Ptr M.D
  -> IO ()
hs_bindgen_6ff72b6fa11fd038 =
  RIP.fromFFIType hs_bindgen_6ff72b6fa11fd038_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 38:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_6ff72b6fa11fd038 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_9c23f536b3698c65" hs_bindgen_9c23f536b3698c65_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooE@
hs_bindgen_9c23f536b3698c65 ::
     RIP.Ptr E
  -> IO ()
hs_bindgen_9c23f536b3698c65 =
  RIP.fromFFIType hs_bindgen_9c23f536b3698c65_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 39:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_9c23f536b3698c65 x1)
