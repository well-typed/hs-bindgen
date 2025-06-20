-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Test02 where

import HsBindgen.TH
import Test.Internal.Tracer (defaultTracePredicate, withTracePredicate)

-- Used by generated code
import HsBindgen.Runtime.Prelude qualified

$(do
    dir <- getPackageRoot
    let args :: ClangArgs
        args = def {
            clangQuoteIncludePathDirs = [CIncludePathDir (dir </> "examples")]
          }
    extBindings <-
      withTracePredicate defaultTracePredicate $ \tracer ->
        snd <$> loadExtBindings tracer args True []
    let opts :: Opts
        opts = def {
            optsClangArgs   = args
          , optsExtBindings = extBindings
          }
    hashIncludeWith opts ["test_02.h"]
 )
