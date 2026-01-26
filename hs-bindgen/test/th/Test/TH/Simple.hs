-- {-# OPTIONS_GHC -ddump-splices #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Test simple Template Haskell interface.

module Test.TH.Simple where

import GHC.Exts
import Optics ((%), (&), (.~))

import HsBindgen.Runtime.Prelude qualified

import HsBindgen.TH

let cfg :: Config
    cfg = def
      & #clang % #extraIncludeDirs .~ [Pkg "examples"]

    cfgTH :: ConfigTH
    cfgTH = def
      & #verbosity       .~ Verbosity Warning
      & #customLogLevels .~ [MakeTrace Info "select-mangle-names-squashed"]
      & #categoryChoice  .~ useUnsafeCategory

 in withHsBindgen cfg cfgTH $
      hashInclude "simple.h"

x :: Simple
x = Simple { simple_n = 10 }
