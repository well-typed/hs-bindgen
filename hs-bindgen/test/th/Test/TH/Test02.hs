{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DerivingStrategies #-}

module Test.TH.Test02 where

import HsBindgen.TH
import Test.Common.HsBindgen.TracePredicate

-- Used by generated code
import Clang.Paths (getCHeaderIncludePath)
import HsBindgen.Runtime.Prelude qualified


$(do
    dir <- getPackageRoot
    let args :: ClangArgs
        args = def {
            clangQuoteIncludePathDirs = [CIncludePathDir (dir </> "examples")]
          }
        -- `uchar.h` is not available on MacOS.
        uCharHeaderNotFound :: TracePredicate TraceMsg
        uCharHeaderNotFound = customTracePredicate [] $ \case
          TraceResolveHeader (ResolveHeaderNotFound h)
            | getCHeaderIncludePath h == "uchar.h"
              -> Just Tolerated
          _otherTrace -> Nothing
    withTracePredicate uCharHeaderNotFound $ \tracer -> do
      let config :: Config
          config = def { configClangArgs = args }
      hashIncludeWith tracer config ["test_02.h"]
 )
