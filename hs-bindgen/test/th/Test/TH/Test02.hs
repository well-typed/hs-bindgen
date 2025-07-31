{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DerivingStrategies #-}

module Test.TH.Test02 where

import HsBindgen.Runtime.Prelude qualified
import HsBindgen.TH

let opts = def {
    extraIncludeDirs = [ RelativeToPkgRoot "examples"]
  , tracerCustomLogLevel = customLogLevelFrom [UCharHeaderResolutionTraceIsInfo]
  }
 in withHsBindgen opts $ hashInclude "test_02.h"
