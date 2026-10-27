{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Draw.Unsafe
    ( M.Draw.Unsafe.draw_check
    )
  where

import qualified HsBindgen.Runtime.Support
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Draw
import M.Point ()
import M.Shape ()
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <draw.h>"
  , "draw_status hs_bindgen_11fc6be0221cbc49 (void)"
  , "{"
  , "  return (draw_check)();"
  , "}"
  ]))

-- __unique:__ @test_typedef_chain_M.Draw_Unsafe_draw_check@
foreign import ccall unsafe "hs_bindgen_11fc6be0221cbc49" hs_bindgen_11fc6be0221cbc49_base ::
     IO HsBindgen.Runtime.Support.CInt

-- __unique:__ @test_typedef_chain_M.Draw_Unsafe_draw_check@
hs_bindgen_11fc6be0221cbc49 :: IO Draw_status
hs_bindgen_11fc6be0221cbc49 =
  fmap BG.fromFFIType hs_bindgen_11fc6be0221cbc49_base

{-| __C declaration:__ @draw_check@

    __defined at:__ @draw.h 6:13@

    __exported by:__ @draw.h@
-}
draw_check :: IO Draw_status
draw_check = hs_bindgen_11fc6be0221cbc49
