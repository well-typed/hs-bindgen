{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Draw.Safe
    ( M.Draw.Safe.draw_check
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
  , "draw_status hs_bindgen_24180c63ee2c5721 (void)"
  , "{"
  , "  return (draw_check)();"
  , "}"
  ]))

-- __unique:__ @test_typedef_chain_M.Draw_Safe_draw_check@
foreign import ccall safe "hs_bindgen_24180c63ee2c5721" hs_bindgen_24180c63ee2c5721_base ::
     IO HsBindgen.Runtime.Support.CInt

-- __unique:__ @test_typedef_chain_M.Draw_Safe_draw_check@
hs_bindgen_24180c63ee2c5721 :: IO Draw_status
hs_bindgen_24180c63ee2c5721 =
  fmap BG.fromFFIType hs_bindgen_24180c63ee2c5721_base

{-| __C declaration:__ @draw_check@

    __defined at:__ @draw.h 6:13@

    __exported by:__ @draw.h@
-}
draw_check :: IO Draw_status
draw_check = hs_bindgen_24180c63ee2c5721
