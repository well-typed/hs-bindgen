{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Early.Unsafe
    ( M.Early.Unsafe.early_draw
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Alias
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "void hs_bindgen_b06be90ceb1f5cea ("
  , "  struct window *arg1"
  , ")"
  , "{"
  , "  (early_draw)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_squash_outside_M.Early_Unsafe_early_draw@
foreign import ccall unsafe "hs_bindgen_b06be90ceb1f5cea" hs_bindgen_b06be90ceb1f5cea_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_squash_outside_M.Early_Unsafe_early_draw@
hs_bindgen_b06be90ceb1f5cea ::
     BG.Ptr M.Alias.Window
  -> IO ()
hs_bindgen_b06be90ceb1f5cea =
  \x0 ->
    hs_bindgen_b06be90ceb1f5cea_base (BG.toFFIType x0)

{-| __C declaration:__ @early_draw@

    __defined at:__ @lib\/early.h 5:6@

    __exported by:__ @root.h@
-}
early_draw ::
     BG.Ptr M.Alias.Window
     -- ^ __C declaration:__ @w@
  -> IO ()
early_draw = hs_bindgen_b06be90ceb1f5cea
