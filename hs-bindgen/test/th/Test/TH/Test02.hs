{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell    #-}

module Test.TH.Test02 where

import HsBindgen.Runtime.Prelude qualified
import HsBindgen.TH

import Optics (set, (%), (&))

let cfg :: Config IncludeDir
    cfg = def & set ( #configClangArgsConfig % #clangExtraIncludeDirs )
            [ RelativeToPkgRoot "examples" ]
 in withHsBindgen cfg def $ hashInclude "test_02.h"
