{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DerivingStrategies #-}

module Test.TH.Test02 where

import HsBindgen.TH

-- Used by generated code
import HsBindgen.Runtime.Prelude qualified

withHsBindgen def {
    extraQuoteIncludeDirs = [ RelativeToPkgRoot "examples"]
  , tracerCustomLogLevel = customLogLevelFrom [UCharHeaderResolutionTraceIsInfo]
  } $ hashInclude $ CHeaderQuoteIncludePath "test_02.h"
