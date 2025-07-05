{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test simple Template Haskell interface.

module Test.TH.Simple where

import HsBindgen.TH

hashInclude ["simple.h"] def { extraIncludeDirs = [ PackageRoot "examples" ] }

x :: Simple
x = Simple { simple_n = 10 }
