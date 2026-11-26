{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Acyclic_callback.FunPtr
    ( AllOutputKinds.Acyclic_callback.FunPtr.acyclic_callback_register
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import AllOutputKinds.Acyclic_callback
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <all_output_kinds.h>"
  , "/* test_librarylayoutall_output_kind_AllOutputKinds.Acyclic_callback_get_acyclic_callback_register */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7e25ad53fd16701e (void)) ("
  , "  acyclic_callback arg1"
  , ")"
  , "{"
  , "  return &acyclic_callback_register;"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Acyclic_callback_get_acyclic_callback_register@
foreign import ccall unsafe "hs_bindgen_7e25ad53fd16701e" hs_bindgen_7e25ad53fd16701e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Acyclic_callback_get_acyclic_callback_register@
hs_bindgen_7e25ad53fd16701e :: IO (BG.FunPtr (Acyclic_callback -> IO ()))
hs_bindgen_7e25ad53fd16701e =
  fmap BG.fromFFIType hs_bindgen_7e25ad53fd16701e_base

{-# NOINLINE acyclic_callback_register #-}
{-| __C declaration:__ @acyclic_callback_register@

    __defined at:__ @all_output_kinds\/acyclic_callback.h 11:6@

    __exported by:__ @all_output_kinds.h@
-}
acyclic_callback_register :: BG.FunPtr (Acyclic_callback -> IO ())
acyclic_callback_register =
  BG.unsafePerformIO hs_bindgen_7e25ad53fd16701e
