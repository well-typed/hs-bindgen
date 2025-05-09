-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Test02 where

import HsBindgen.TH

-- Used by generated code
import Data.Word qualified
import HsBindgen.Runtime.LibC qualified

$(do
    dir <- getPackageRoot
    let args = defaultClangArgs {
            clangQuoteIncludePathDirs = [CIncludePathDir (dir </> "examples")]
          }
    extBindingSpecs <- snd <$> loadBindingSpecs args [
        joinPath [dir, "bindings", "base.yaml"]
      , joinPath [dir, "bindings", "hs-bindgen-runtime.yaml"]
      ]
    let opts = defaultOpts {
            optsClangArgs       = args
          , optsExtBindingSpecs = extBindingSpecs
          }
    genBindings opts "test_02.h"
 )
