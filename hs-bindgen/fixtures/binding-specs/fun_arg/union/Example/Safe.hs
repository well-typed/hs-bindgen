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
  [ "#include <binding-specs/fun_arg/union.h>"
  , "void hs_bindgen_e7a126c05b549270 ("
  , "  union MyUnion *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_a50b49b0997ba61e ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_b9a066dd332d47b6 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  , "void hs_bindgen_f533f768fa6c3ba1 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(*arg1);"
  , "}"
  , "void hs_bindgen_52743854a5f698bf ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(*arg1);"
  , "}"
  , "void hs_bindgen_ba9326fbfa620bcc ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argunion_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_e7a126c05b549270" hs_bindgen_e7a126c05b549270 ::
     Ptr.Ptr MyUnion
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr MyUnion
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_e7a126c05b549270

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
    F.with x0 (\y1 -> hs_bindgen_e7a126c05b549270 y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_a50b49b0997ba61e" hs_bindgen_a50b49b0997ba61e ::
     Ptr.Ptr A
  -> IO ()

{-| Pointer-based API for 'fooA'
-}
fooA_wrapper ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA_wrapper = hs_bindgen_a50b49b0997ba61e

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
    F.with x0 (\y1 -> hs_bindgen_a50b49b0997ba61e y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_b9a066dd332d47b6" hs_bindgen_b9a066dd332d47b6 ::
     Ptr.Ptr B
  -> IO ()

{-| Pointer-based API for 'fooB'
-}
fooB_wrapper ::
     Ptr.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB_wrapper = hs_bindgen_b9a066dd332d47b6

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
    F.with x0 (\y1 -> hs_bindgen_b9a066dd332d47b6 y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_f533f768fa6c3ba1" hs_bindgen_f533f768fa6c3ba1 ::
     Ptr.Ptr M.C
  -> IO ()

{-| Pointer-based API for 'fooC'
-}
fooC_wrapper ::
     Ptr.Ptr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC_wrapper = hs_bindgen_f533f768fa6c3ba1

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
    F.with x0 (\y1 -> hs_bindgen_f533f768fa6c3ba1 y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_52743854a5f698bf" hs_bindgen_52743854a5f698bf ::
     Ptr.Ptr M.D
  -> IO ()

{-| Pointer-based API for 'fooD'
-}
fooD_wrapper ::
     Ptr.Ptr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD_wrapper = hs_bindgen_52743854a5f698bf

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
    F.with x0 (\y1 -> hs_bindgen_52743854a5f698bf y1)

-- __unique:__ @test_bindingspecsfun_argunion_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_ba9326fbfa620bcc" hs_bindgen_ba9326fbfa620bcc ::
     Ptr.Ptr E
  -> IO ()

{-| Pointer-based API for 'fooE'
-}
fooE_wrapper ::
     Ptr.Ptr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE_wrapper = hs_bindgen_ba9326fbfa620bcc

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
    F.with x0 (\y1 -> hs_bindgen_ba9326fbfa620bcc y1)
