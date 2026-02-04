-- {-# OPTIONS_GHC -ddump-splices #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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

import Optics ((%), (&), (.~))

import HsBindgen.TH

let cfg :: Config
    cfg = def
      & #clang % #extraIncludeDirs .~ [Pkg "examples"]

    cfgTH :: ConfigTH
    cfgTH = def
      & #verbosity       .~ Verbosity Warning
      & #categoryChoice  .~ useUnsafeCategory

 in withHsBindgen cfg cfgTH $
      hashInclude "simple.h"

x :: Simple
x = Simple { simple_n = 10 }
