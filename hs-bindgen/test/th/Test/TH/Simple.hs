{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-foreign-imports #-}

-- | Test simple Template Haskell interface.

module Test.TH.Simple where

import HsBindgen.Runtime.Prelude qualified
import HsBindgen.TH

let opts = def { extraIncludeDirs = [ RelativeToPkgRoot "examples" ] }
 in withHsBindgen opts $ hashInclude "simple.h"

x :: Simple
x = Simple { simple_n = 10 }
