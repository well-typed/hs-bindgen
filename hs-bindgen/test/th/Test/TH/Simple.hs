{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test simple Template Haskell interface.

module Test.TH.Simple where

import HsBindgen.Runtime.Prelude qualified
import HsBindgen.TH

let opts = def { extraSystemIncludeDirs = [ RelativeToPkgRoot "examples" ] }
 in withHsBindgen opts $ hashInclude $ Quote "simple.h"

x :: Simple
x = Simple { simple_n = 10 }
