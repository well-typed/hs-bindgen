{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Test simple Template Haskell interface.

module Test.TH.Simple where

import Optics ((%), (&), (.~), (?~))

import HsBindgen.Runtime.Prelude qualified

import HsBindgen.TH

let cfg :: Config
    cfg = def
            & #clang % #extraIncludeDirs .~ [
                  Pkg "examples"
                ]
            & #outputBindingSpec ?~ Pkg "fixtures/simple-binding-spec.yaml"
    cfgTH :: ConfigTH
    cfgTH = def
              & #verbosity .~ Verbosity Warning
              & #customLogLevelSettings .~ [EnableMacroWarnings]
 in withHsBindgen cfg cfgTH $
      hashInclude "simple.h"

x :: Simple
x = Simple { simple_n = 10 }
