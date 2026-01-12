{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/struct.h>"
  , "void hs_bindgen_164f74ca36f10edc ("
  , "  struct MyStruct *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_2e2a097cbf8781e4 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_f342a16fe13c03ae ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  , "void hs_bindgen_409d970719708dd0 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(*arg1);"
  , "}"
  , "void hs_bindgen_5a49923661ef790d ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(*arg1);"
  , "}"
  , "void hs_bindgen_dc794a694a72bc04 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_164f74ca36f10edc" hs_bindgen_164f74ca36f10edc ::
     Ptr.Ptr MyStruct
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_164f74ca36f10edc

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
    F.with x0 (\y1 -> hs_bindgen_164f74ca36f10edc y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_2e2a097cbf8781e4" hs_bindgen_2e2a097cbf8781e4 ::
     Ptr.Ptr A
  -> IO ()

{-| Pointer-based API for 'fooA'
-}
fooA_wrapper ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA_wrapper = hs_bindgen_2e2a097cbf8781e4

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
    F.with x0 (\y1 -> hs_bindgen_2e2a097cbf8781e4 y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_f342a16fe13c03ae" hs_bindgen_f342a16fe13c03ae ::
     Ptr.Ptr B
  -> IO ()

{-| Pointer-based API for 'fooB'
-}
fooB_wrapper ::
     Ptr.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB_wrapper = hs_bindgen_f342a16fe13c03ae

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
    F.with x0 (\y1 -> hs_bindgen_f342a16fe13c03ae y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_409d970719708dd0" hs_bindgen_409d970719708dd0 ::
     Ptr.Ptr M.C
  -> IO ()

{-| Pointer-based API for 'fooC'
-}
fooC_wrapper ::
     Ptr.Ptr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC_wrapper = hs_bindgen_409d970719708dd0

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_409d970719708dd0 y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_5a49923661ef790d" hs_bindgen_5a49923661ef790d ::
     Ptr.Ptr M.D
  -> IO ()

{-| Pointer-based API for 'fooD'
-}
fooD_wrapper ::
     Ptr.Ptr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD_wrapper = hs_bindgen_5a49923661ef790d

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_5a49923661ef790d y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_dc794a694a72bc04" hs_bindgen_dc794a694a72bc04 ::
     Ptr.Ptr E
  -> IO ()

{-| Pointer-based API for 'fooE'
-}
fooE_wrapper ::
     Ptr.Ptr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE_wrapper = hs_bindgen_dc794a694a72bc04

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_dc794a694a72bc04 y1)
