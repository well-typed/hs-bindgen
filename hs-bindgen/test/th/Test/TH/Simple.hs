{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test simple Template Haskell interface.

module Test.TH.Simple where

import Optics ((%), (&), (.~))

import HsBindgen.Runtime.Prelude qualified

import HsBindgen.TH

let cfg :: Config
    cfg = def & #clang % #extraIncludeDirs .~ [
              Pkg "examples"
            ]
 in withHsBindgen cfg def $ hashInclude "simple.h"

x :: Simple
x = Simple { simple_n = 10 }
