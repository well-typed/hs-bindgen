{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test simple Template Haskell interface.

module Test.TH.Simple where

import HsBindgen.Runtime.Prelude qualified
import HsBindgen.TH

import Optics (set, (%), (&))

let cfg :: Config IncludeDir
    cfg = def & set ( #configClangArgsConfig % #clangExtraIncludeDirs )
            [ RelativeToPkgRoot "examples" ]
 in withHsBindgen cfg def $ hashInclude "simple.h"

x :: Simple
x = Simple { simple_n = 10 }
