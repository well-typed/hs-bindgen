module HsBindgen.Backend.HsModule.Render (
    render
  , renderHsBoot
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.HsModule.Pretty ()
import HsBindgen.Backend.HsModule.Translation

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Render generated bindings
render :: HsModule -> String
render = (++ "\n") . PP.renderPretty (PP.mkContext 80)

-- | Render the boot file of generated bindings
renderHsBoot :: HsBoot -> String
renderHsBoot = (++ "\n") . PP.renderPretty (PP.mkContext 80)
