{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.TH.Test01 where

import HsBindgen.Runtime.Prelude qualified
import HsBindgen.TH

import Optics (set, (%), (&))

let cfg :: Config IncludeDir
    cfg = def & set ( #configClangArgsConfig % #clangExtraIncludeDirs )
            [ RelativeToPkgRoot "examples" ]
 in withHsBindgen cfg def $ hashInclude "test_01.h"
