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
  [ "#include <binding-specs/fun_arg/macro/array.h>"
  , "void hs_bindgen_2a6ef3a515232132 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_4449a68917cbc499 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_05766199d1b077bb ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_2a6ef3a515232132" hs_bindgen_2a6ef3a515232132 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
foo ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_2a6ef3a515232132

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_4449a68917cbc499" hs_bindgen_4449a68917cbc499 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooA ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_4449a68917cbc499

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_05766199d1b077bb" hs_bindgen_05766199d1b077bb ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooB ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_05766199d1b077bb
