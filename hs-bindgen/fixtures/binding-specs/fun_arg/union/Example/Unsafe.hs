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
  [ "#include <binding-specs/fun_arg/union.h>"
  , "void hs_bindgen_82b12ac06ad69760 ("
  , "  union MyUnion *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_0def96c22ae9d7cf ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_48eb50df07de18b3 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  , "void hs_bindgen_e4a50b687bf19159 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(*arg1);"
  , "}"
  , "void hs_bindgen_aaefd1f85cd999b4 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(*arg1);"
  , "}"
  , "void hs_bindgen_959e2a8963627adb ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argunion_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_82b12ac06ad69760" hs_bindgen_82b12ac06ad69760 ::
     Ptr.Ptr MyUnion
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr MyUnion
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_82b12ac06ad69760

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/union.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
foo ::
     MyUnion
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_82b12ac06ad69760 y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_0def96c22ae9d7cf" hs_bindgen_0def96c22ae9d7cf ::
     Ptr.Ptr A
  -> IO ()

{-| Pointer-based API for 'fooA'
-}
fooA_wrapper ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA_wrapper = hs_bindgen_0def96c22ae9d7cf

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/union.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_0def96c22ae9d7cf y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_48eb50df07de18b3" hs_bindgen_48eb50df07de18b3 ::
     Ptr.Ptr B
  -> IO ()

{-| Pointer-based API for 'fooB'
-}
fooB_wrapper ::
     Ptr.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB_wrapper = hs_bindgen_48eb50df07de18b3

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/union.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_48eb50df07de18b3 y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_e4a50b687bf19159" hs_bindgen_e4a50b687bf19159 ::
     Ptr.Ptr M.C
  -> IO ()

{-| Pointer-based API for 'fooC'
-}
fooC_wrapper ::
     Ptr.Ptr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC_wrapper = hs_bindgen_e4a50b687bf19159

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/union.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_e4a50b687bf19159 y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_aaefd1f85cd999b4" hs_bindgen_aaefd1f85cd999b4 ::
     Ptr.Ptr M.D
  -> IO ()

{-| Pointer-based API for 'fooD'
-}
fooD_wrapper ::
     Ptr.Ptr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD_wrapper = hs_bindgen_aaefd1f85cd999b4

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/union.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_aaefd1f85cd999b4 y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_959e2a8963627adb" hs_bindgen_959e2a8963627adb ::
     Ptr.Ptr E
  -> IO ()

{-| Pointer-based API for 'fooE'
-}
fooE_wrapper ::
     Ptr.Ptr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE_wrapper = hs_bindgen_959e2a8963627adb

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/union.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_959e2a8963627adb y1)
