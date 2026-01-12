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
  [ "#include <binding-specs/fun_arg/typedef/union.h>"
  , "void hs_bindgen_51195acecf6b880e ("
  , "  union MyUnion *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_eb4a794c10ca94f9 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_a11eb4471a953b64 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  , "void hs_bindgen_1dea92b3f3346311 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(*arg1);"
  , "}"
  , "void hs_bindgen_dba6d56e1a316e2d ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(*arg1);"
  , "}"
  , "void hs_bindgen_23b6f403a534aeae ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_51195acecf6b880e" hs_bindgen_51195acecf6b880e ::
     Ptr.Ptr MyUnion
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr MyUnion
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_51195acecf6b880e

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
foo ::
     MyUnion
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_51195acecf6b880e y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_eb4a794c10ca94f9" hs_bindgen_eb4a794c10ca94f9 ::
     Ptr.Ptr A
  -> IO ()

{-| Pointer-based API for 'fooA'
-}
fooA_wrapper ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA_wrapper = hs_bindgen_eb4a794c10ca94f9

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_eb4a794c10ca94f9 y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_a11eb4471a953b64" hs_bindgen_a11eb4471a953b64 ::
     Ptr.Ptr B
  -> IO ()

{-| Pointer-based API for 'fooB'
-}
fooB_wrapper ::
     Ptr.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB_wrapper = hs_bindgen_a11eb4471a953b64

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_a11eb4471a953b64 y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_1dea92b3f3346311" hs_bindgen_1dea92b3f3346311 ::
     Ptr.Ptr M.C
  -> IO ()

{-| Pointer-based API for 'fooC'
-}
fooC_wrapper ::
     Ptr.Ptr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC_wrapper = hs_bindgen_1dea92b3f3346311

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_1dea92b3f3346311 y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_dba6d56e1a316e2d" hs_bindgen_dba6d56e1a316e2d ::
     Ptr.Ptr M.D
  -> IO ()

{-| Pointer-based API for 'fooD'
-}
fooD_wrapper ::
     Ptr.Ptr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD_wrapper = hs_bindgen_dba6d56e1a316e2d

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_dba6d56e1a316e2d y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_23b6f403a534aeae" hs_bindgen_23b6f403a534aeae ::
     Ptr.Ptr E
  -> IO ()

{-| Pointer-based API for 'fooE'
-}
fooE_wrapper ::
     Ptr.Ptr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE_wrapper = hs_bindgen_23b6f403a534aeae

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_23b6f403a534aeae y1)
