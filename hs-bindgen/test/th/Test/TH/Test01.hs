{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.TH.Test01 where

import Optics ((%), (&), (.~))

import HsBindgen.Runtime.Prelude qualified

import HsBindgen.TH

let cfg :: Config
    cfg = def
      & #clang % #extraIncludeDirs .~ [Pkg "examples"]
    cfgTh :: ConfigTH
    cfgTh = def
 in withHsBindgen cfg cfgTh $
      hashInclude "test_01.h"
