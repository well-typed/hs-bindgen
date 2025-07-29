{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test simple Template Haskell interface.

module Test.TH.Simple where

import HsBindgen.TH

let opts = def { extraQuoteIncludeDirs = [ RelativeToPkgRoot "examples" ] }
 in withHsBindgen opts $ do
      hashInclude $ CHeaderQuoteIncludePath "simple.h"

x :: Simple
x = Simple { simple_n = 10 }

