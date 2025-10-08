{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell    #-}

module Test.TH.Test02 where

import Optics ((%), (&), (.~))

import HsBindgen.Runtime.Prelude qualified

import HsBindgen.TH

let cfg :: Config
    cfg = def & #clang % #extraIncludeDirs .~ [
              Pkg "examples"
            ]
 in withHsBindgen cfg def $ hashInclude "test_02.h"
