{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Scene.FunPtr
    ( Geometry.Scene.FunPtr.scene_new
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Scene
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <import_cycle.h>"
  , "/* test_librarywalkthroughimport_cyc_Geometry.Scene_get_scene_new */"
  , "__attribute__ ((const))"
  , "struct scene *(*hs_bindgen_bb63cf0415721cce (void)) (void)"
  , "{"
  , "  return &scene_new;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Scene_get_scene_new@
foreign import ccall unsafe "hs_bindgen_bb63cf0415721cce" hs_bindgen_bb63cf0415721cce_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Scene_get_scene_new@
hs_bindgen_bb63cf0415721cce :: IO (BG.FunPtr (IO (BG.Ptr Scene)))
hs_bindgen_bb63cf0415721cce =
  fmap BG.fromFFIType hs_bindgen_bb63cf0415721cce_base

{-# NOINLINE scene_new #-}
{-| __C declaration:__ @scene_new@

    __defined at:__ @geometry\/scene.h 14:15@

    __exported by:__ @import_cycle.h@
-}
scene_new :: BG.FunPtr (IO (BG.Ptr Scene))
scene_new =
  BG.unsafePerformIO hs_bindgen_bb63cf0415721cce
