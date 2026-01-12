{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/struct.h>"
  , "void hs_bindgen_b39843a9b8c0691a ("
  , "  struct MyStruct *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_5efc6ead467b8950 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_67f3043ea0af6206 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  , "void hs_bindgen_954d846ba0029844 ("
  , "  C arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_04e48f8233948be7 ("
  , "  D arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_cc24f9f925f95d5e ("
  , "  E arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_b39843a9b8c0691a" hs_bindgen_b39843a9b8c0691a ::
     Ptr.Ptr MyStruct
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_b39843a9b8c0691a

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
foo ::
     MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_b39843a9b8c0691a y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_5efc6ead467b8950" hs_bindgen_5efc6ead467b8950 ::
     Ptr.Ptr A
  -> IO ()

{-| Pointer-based API for 'fooA'
-}
fooA_wrapper ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA_wrapper = hs_bindgen_5efc6ead467b8950

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_5efc6ead467b8950 y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_67f3043ea0af6206" hs_bindgen_67f3043ea0af6206 ::
     Ptr.Ptr B
  -> IO ()

{-| Pointer-based API for 'fooB'
-}
fooB_wrapper ::
     Ptr.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB_wrapper = hs_bindgen_67f3043ea0af6206

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_67f3043ea0af6206 y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_954d846ba0029844" hs_bindgen_954d846ba0029844 ::
     M.C
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_954d846ba0029844

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_04e48f8233948be7" hs_bindgen_04e48f8233948be7 ::
     M.D
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_04e48f8233948be7

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_cc24f9f925f95d5e" hs_bindgen_cc24f9f925f95d5e ::
     E
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_cc24f9f925f95d5e
