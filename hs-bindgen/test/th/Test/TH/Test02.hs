{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Test.TH.Test02 where

import HsBindgen.Runtime.Prelude qualified
import HsBindgen.TH

let opts = def {
      extraIncludeDirs = [ RelativeToPkgRoot "examples"]
    }
 in withHsBindgen opts $ hashInclude "test_02.h"
