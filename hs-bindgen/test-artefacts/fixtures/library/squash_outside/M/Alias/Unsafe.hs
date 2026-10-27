{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Alias.Unsafe
    ( M.Alias.Unsafe.alias_show
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Alias
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "void hs_bindgen_6d6dd40a7771a237 ("
  , "  window *arg1"
  , ")"
  , "{"
  , "  (alias_show)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_squash_outside_M.Alias_Unsafe_alias_show@
foreign import ccall unsafe "hs_bindgen_6d6dd40a7771a237" hs_bindgen_6d6dd40a7771a237_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_squash_outside_M.Alias_Unsafe_alias_show@
hs_bindgen_6d6dd40a7771a237 ::
     BG.Ptr Window
  -> IO ()
hs_bindgen_6d6dd40a7771a237 =
  \x0 ->
    hs_bindgen_6d6dd40a7771a237_base (BG.toFFIType x0)

{-| __C declaration:__ @alias_show@

    __defined at:__ @lib\/alias.h 6:6@

    __exported by:__ @root.h@
-}
alias_show ::
     BG.Ptr Window
     -- ^ __C declaration:__ @w@
  -> IO ()
alias_show = hs_bindgen_6d6dd40a7771a237
