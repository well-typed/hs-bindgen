module HsBindgen.Backend.HsModule.Render (
    render
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
