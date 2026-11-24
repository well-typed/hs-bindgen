{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Acyclic_callback.Safe
    ( AllOutputKinds.Acyclic_callback.Safe.acyclic_callback_register
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import AllOutputKinds.Acyclic_callback
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <all_output_kinds.h>"
  , "void hs_bindgen_0038bd779d46aea0 ("
  , "  acyclic_callback arg1"
  , ")"
  , "{"
  , "  (acyclic_callback_register)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Acyclic_callback_Safe_acyclic_callback_register@
foreign import ccall safe "hs_bindgen_0038bd779d46aea0" hs_bindgen_0038bd779d46aea0_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Acyclic_callback_Safe_acyclic_callback_register@
hs_bindgen_0038bd779d46aea0 ::
     Acyclic_callback
  -> IO ()
hs_bindgen_0038bd779d46aea0 =
  \x0 ->
    hs_bindgen_0038bd779d46aea0_base (BG.toFFIType x0)

{-| __C declaration:__ @acyclic_callback_register@

    __defined at:__ @all_output_kinds\/acyclic_callback.h 11:6@

    __exported by:__ @all_output_kinds.h@
-}
acyclic_callback_register ::
     Acyclic_callback
     -- ^ __C declaration:__ @callback@
  -> IO ()
acyclic_callback_register =
  hs_bindgen_0038bd779d46aea0
