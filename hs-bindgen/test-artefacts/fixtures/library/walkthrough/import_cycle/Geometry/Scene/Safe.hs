{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Scene.Safe
    ( Geometry.Scene.Safe.scene_new
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Scene
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <import_cycle.h>"
  , "struct scene *hs_bindgen_46869375d3526096 (void)"
  , "{"
  , "  return (scene_new)();"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Scene_Safe_scene_new@
foreign import ccall safe "hs_bindgen_46869375d3526096" hs_bindgen_46869375d3526096_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Scene_Safe_scene_new@
hs_bindgen_46869375d3526096 :: IO (BG.Ptr Scene)
hs_bindgen_46869375d3526096 =
  fmap BG.fromFFIType hs_bindgen_46869375d3526096_base

{-| __C declaration:__ @scene_new@

    __defined at:__ @geometry\/scene.h 14:15@

    __exported by:__ @import_cycle.h@
-}
scene_new :: IO (BG.Ptr Scene)
scene_new = hs_bindgen_46869375d3526096
