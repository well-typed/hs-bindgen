{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array_known_size.h>"
  , "void hs_bindgen_a17ea38966af88ee ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_c203ac5ff8a65aae ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_b2a6967de99725fb ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_e4e4582ed78cd39f ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_201329b4800c3316 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_f83af73ac77658df ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_a17ea38966af88ee" hs_bindgen_a17ea38966af88ee ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
foo ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_a17ea38966af88ee

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_c203ac5ff8a65aae" hs_bindgen_c203ac5ff8a65aae ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooA ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_c203ac5ff8a65aae

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_b2a6967de99725fb" hs_bindgen_b2a6967de99725fb ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooB ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_b2a6967de99725fb

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_e4e4582ed78cd39f" hs_bindgen_e4e4582ed78cd39f ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooC ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_e4e4582ed78cd39f

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_201329b4800c3316" hs_bindgen_201329b4800c3316 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooD ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_201329b4800c3316

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_f83af73ac77658df" hs_bindgen_f83af73ac77658df ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooE ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_f83af73ac77658df
