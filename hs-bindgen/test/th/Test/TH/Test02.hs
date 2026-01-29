{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Test.TH.Test02 where

import Optics ((%), (&), (.~))

import HsBindgen.Runtime.LibC qualified
import HsBindgen.Runtime.Prelude qualified

import HsBindgen.TH

let cfg :: Config
    cfg = def
      & #clang % #extraIncludeDirs .~ [Pkg "examples"]
    cfgTh :: ConfigTH
    cfgTh = def
 in withHsBindgen cfg cfgTh $
      hashInclude "test_02.h"
