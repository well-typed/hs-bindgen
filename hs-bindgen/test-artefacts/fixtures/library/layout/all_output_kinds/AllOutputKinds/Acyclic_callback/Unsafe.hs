{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Acyclic_callback.Unsafe
    ( AllOutputKinds.Acyclic_callback.Unsafe.acyclic_callback_register
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import AllOutputKinds.Acyclic_callback
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <all_output_kinds.h>"
  , "void hs_bindgen_37ccc7b4af2ff57d ("
  , "  acyclic_callback arg1"
  , ")"
  , "{"
  , "  (acyclic_callback_register)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Acyclic_callback_Unsafe_acyclic_callback_register@
foreign import ccall unsafe "hs_bindgen_37ccc7b4af2ff57d" hs_bindgen_37ccc7b4af2ff57d_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Acyclic_callback_Unsafe_acyclic_callback_register@
hs_bindgen_37ccc7b4af2ff57d ::
     Acyclic_callback
  -> IO ()
hs_bindgen_37ccc7b4af2ff57d =
  \x0 ->
    hs_bindgen_37ccc7b4af2ff57d_base (BG.toFFIType x0)

{-| __C declaration:__ @acyclic_callback_register@

    __defined at:__ @all_output_kinds\/acyclic_callback.h 11:6@

    __exported by:__ @all_output_kinds.h@
-}
acyclic_callback_register ::
     Acyclic_callback
     -- ^ __C declaration:__ @callback@
  -> IO ()
acyclic_callback_register =
  hs_bindgen_37ccc7b4af2ff57d
