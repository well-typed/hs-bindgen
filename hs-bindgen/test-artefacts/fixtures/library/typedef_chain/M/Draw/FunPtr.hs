{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Draw.FunPtr
    ( M.Draw.FunPtr.draw_check
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Draw
import M.Point ()
import M.Shape ()
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <draw.h>"
  , "/* test_typedef_chain_M.Draw_get_draw_check */"
  , "__attribute__ ((const))"
  , "draw_status (*hs_bindgen_e74f52b6256187dd (void)) (void)"
  , "{"
  , "  return &draw_check;"
  , "}"
  ]))

-- __unique:__ @test_typedef_chain_M.Draw_get_draw_check@
foreign import ccall unsafe "hs_bindgen_e74f52b6256187dd" hs_bindgen_e74f52b6256187dd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typedef_chain_M.Draw_get_draw_check@
hs_bindgen_e74f52b6256187dd :: IO (BG.FunPtr (IO Draw_status))
hs_bindgen_e74f52b6256187dd =
  fmap BG.fromFFIType hs_bindgen_e74f52b6256187dd_base

{-# NOINLINE draw_check #-}
{-| __C declaration:__ @draw_check@

    __defined at:__ @draw.h 6:13@

    __exported by:__ @draw.h@
-}
draw_check :: BG.FunPtr (IO Draw_status)
draw_check =
  BG.unsafePerformIO hs_bindgen_e74f52b6256187dd
