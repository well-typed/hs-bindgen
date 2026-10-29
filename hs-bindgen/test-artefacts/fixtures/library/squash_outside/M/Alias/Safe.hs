{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Alias.Safe
    ( M.Alias.Safe.alias_show
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Alias
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "void hs_bindgen_ac7890414a84ed68 ("
  , "  window *arg1"
  , ")"
  , "{"
  , "  (alias_show)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_squash_outside_M.Alias_Safe_alias_show@
foreign import ccall safe "hs_bindgen_ac7890414a84ed68" hs_bindgen_ac7890414a84ed68_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_squash_outside_M.Alias_Safe_alias_show@
hs_bindgen_ac7890414a84ed68 ::
     BG.Ptr Window
  -> IO ()
hs_bindgen_ac7890414a84ed68 =
  \x0 ->
    hs_bindgen_ac7890414a84ed68_base (BG.toFFIType x0)

{-| __C declaration:__ @alias_show@

    __defined at:__ @lib\/alias.h 6:6@

    __exported by:__ @root.h@
-}
alias_show ::
     BG.Ptr Window
     -- ^ __C declaration:__ @w@
  -> IO ()
alias_show = hs_bindgen_ac7890414a84ed68
