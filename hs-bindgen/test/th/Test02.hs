-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Test02 where

import HsBindgen.TH

import Test.Internal.Trace (degradeKnownTraces)

-- Used by generated code
import Data.Word qualified
import HsBindgen.Runtime.LibC qualified

$(do
    dir <- getPackageRoot
    let args = defaultClangArgs {
            clangQuoteIncludePathDirs = [CIncludePathDir (dir </> "examples")]
          }
    extBindings <- snd <$> (withTracerQ defaultTracerConf degradeKnownTraces $
      \tracer -> loadExtBindings tracer args [
        joinPath [dir, "bindings", "base.yaml"]
      , joinPath [dir, "bindings", "hs-bindgen-runtime.yaml"]
      ])
    let opts = defaultOpts {
            optsClangArgs   = args
          , optsExtBindings = extBindings
          }
    genBindings opts "test_02.h"
 )
