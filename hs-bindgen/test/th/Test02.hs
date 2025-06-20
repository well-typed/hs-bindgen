-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Test02 where

import HsBindgen.TH
import Test.Internal.Tracer (TraceExpectation (..), TracePredicate,
                             customTracePredicate, defaultTracePredicate,
                             singleTracePredicate, withTracePredicate)

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
        uCharHeaderNotFound :: TracePredicate Trace
        uCharHeaderNotFound = customTracePredicate [] $ \case
          TraceResolveHeader (ResolveHeaderNotFound h)
            | getCHeaderIncludePath h == "uchar.h"
              -> Just Tolerated
          _otherTrace -> Nothing
    extBindings <-
      withTracePredicate uCharHeaderNotFound $ \tracer ->
        loadExtBindings tracer args UseStdlibBindingSpecs []
    let opts :: Opts
        opts = def {
            optsClangArgs   = args
          , optsExtBindings = extBindings
          }
    hashIncludeWith opts ["test_02.h"]
 )
